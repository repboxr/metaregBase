# FILE: /home/rstudio/repbox/metaregBase/R/mrb_repair.R

example = function() {
  repboxRun::repbox_load_libs()
  project_dir = rb_get_project_dir("~/repbox/projects_test/test")
  mrb_repair_failed_runs(project_dir)
  rstudioapi::filesPaneNavigate(project_dir)
}


#' Automatically repair metaregBase R failures by fetching Stata's exact regression data.
#'
#' Generates a Stata script for failed runs that executes the data-prep path, runs the
#' regression quietly, keeps exactly the estimation sample `e(sample)`, and saves it.
#' It then forces the R reproduction to load this exact cache, sidestepping R
#' data translation bugs.
mrb_repair_failed_runs = function(project_dir = mrb$project_dir, mrb=NULL, max_reg=10) {
  restore.point("mrb_repair_failed_runs")

  if (is.null(mrb)) {
    mrb = mrb_init(project_dir)
  }

  drf_clear_mcache()

  failed_pids = mrb_get_to_repair_runids(mrb=mrb)
  if (!is.null(max_reg)) {
    failed_pids = head(failed_pids, max_reg)
  }
  if (length(failed_pids) == 0) {
    cat("\nNo failed runs to repair.\n")
    return(mrb)
  }

  cat("\nRepair attempt by caching regression data for runids: ", paste(failed_pids, collapse=", "), "\n")
  res = mrb_cache_failed_runs_data(mrb,pids=failed_pids)

  mrb$drf = drf_apply_caches(mrb$drf, just_pids=failed_pids)

  # Rerun base to reconstruct regvar (in case xi variables were dumped into cache)
  mrb = mrb_run_r_base(mrb, just_pids = failed_pids)

  # Rerun reg
  mrb = mrb_run_r_reg(mrb, just_pids = failed_pids)

  # Re-evaluate regcheck for the whole dataset with the updated level
  mrb = mrb_make_regcheck_parcel(mrb, just_pids=failed_pids, repair_code = "c")

  return(mrb)
}
#' Automatically repair metaregBase R failures by caching at the point of translation errors.
#'
#' Identifies data translation errors on the paths of failed regressions,
#' determines a common caching point that can serve multiple pids, generates
#' the cache in Stata, and re-runs the R base and regression steps.
mrb_repair_translation_errors = function(project_dir = mrb$project_dir, mrb=NULL, max_reg=10) {
  restore.point("mrb_repair_translation_errors")

  if (is.null(mrb)) {
    mrb = mrb_init(project_dir)
  }

  drf_clear_mcache()

  failed_pids = mrb_get_to_repair_runids(mrb=mrb)
  if (!is.null(max_reg)) {
    failed_pids = head(failed_pids, max_reg)
  }
  if (length(failed_pids) == 0) {
    cat("\nNo failed runs to repair.\n")
    return(mrb)
  }

  cat("\nRepair attempt by translation error caching for runids: ", paste(failed_pids, collapse=", "), "\n")

  # Ensure r_err_runids is loaded/synced
  mrb$drf = repboxDRF:::drf_sync_r_err_runids(mrb$drf)

  for (pid in failed_pids) {
    drf = mrb$drf

    # 1. Adapt path to caches
    drf = repboxDRF:::drf_apply_caches(drf, just_pids = pid)
    path_df = drf$path_df[drf$path_df$pid == pid, ]

    if (NROW(path_df) == 0) next

    # 2. Check for r_err_runids on the path
    err_runids = intersect(path_df$runid, drf$r_err_runids)

    first_runid = path_df$runid[1]
    run_df_first = drf$run_df[drf$run_df$runid == first_runid, ]
    if (NROW(run_df_first) > 0 && isTRUE(run_df_first$has_file_cache[1])) {
      err_runids = setdiff(err_runids, first_runid)
    }

    if (length(err_runids) == 0) {
      cat(sprintf("\nFailed pid %d has no identified data translation error on its active path. Trying to re-run R base/reg to see if cache solves it.\n", pid))
      mrb$drf = drf
      mrb = mrb_run_r_base(mrb, just_pids = pid)
      mrb = mrb_run_r_reg(mrb, just_pids = pid)
      mrb = mrb_make_regcheck_parcel(mrb, just_pids = pid, repair_code = "c_t")
      next
    }

    err_runid = max(err_runids)

    # 3. Find cache_runid
    # Find all pids that use err_runid
    all_pids_with_err = unique(drf$path_df$pid[drf$path_df$runid == err_runid])

    # We want the intersection of paths for all these pids
    path_list = lapply(all_pids_with_err, function(p) {
      drf$path_df$runid[drf$path_df$pid == p]
    })
    common_runids = Reduce(intersect, path_list)
    valid_common = common_runids[common_runids >= err_runid]

    cache_runid = err_runid
    if (length(valid_common) > 0) {
      # avoid to set cache_runid to the final regression pid (unless cache_runid==pid)
      if (!err_runid %in% all_pids_with_err) {
        cands_not_pid = setdiff(valid_common, all_pids_with_err)
        if (length(cands_not_pid) > 0) {
          cache_runid = max(cands_not_pid)
        } else {
          cache_runid = err_runid
        }
      } else {
        cache_runid = err_runid
      }
    }

    cat(sprintf("\nRepairing translation error for pid %d: Caching at runid %d (err_runid was %d)\n", pid, cache_runid, err_runid))

    # 4. Make cache and try translation again
    mrb_cache_reg_data(mrb, pids = cache_runid)

    drf = repboxDRF:::drf_apply_caches(drf, just_pids = all_pids_with_err)
    mrb$drf = drf

    mrb = mrb_run_r_base(mrb, just_pids = pid)
    mrb = mrb_run_r_reg(mrb, just_pids = pid)
    mrb = mrb_make_regcheck_parcel(mrb, just_pids = pid, repair_code = "c_t")
  }

  return(mrb)
}

mrb_get_to_repair_runids = function(mrb, parcels=mrb$parcels, ignore_already_repaired=TRUE) {
  restore.point("mrb_get_to_repair_runids")
  parcels = repboxDB::repdb_load_parcels(mrb$project_dir, c("regcheck","reg"), parcels)

  regcheck = parcels$regcheck
  if (is.null(regcheck)) {
    cat("\nNo regcheck parcel found. Run mrb_make_regcheck_parcel() first.\n")
    return(NULL)
  }
  reg = parcels$reg
  if (!has_col(regcheck, "cmd") & NROW(regcheck)>0) {
    if (!is.null(reg)) {
      regcheck = left_join(regcheck, reg %>% select(cmd, runid), by="runid")
    } else {
      regcheck$cmd = ""
    }
  }
  regcheck$cmd = na.val(regcheck$cmd, "")

  regcheck = regcheck %>%
    # repair if rb did not run
    mutate(do_repair = (sb_raw_did_run) & (!rb_did_run)) %>%
    # repair if sb and rb coefs are different
    # suggests wrong data set, unless we have an R
    # command known to not match coefs
    mutate(do_repair = do_repair | is.true(!rb_sb_coef_same & !(has.substr(cmd,"logit")|has.substr(cmd, "probit")) )) %>%
    # only repair commands that have an r translation
    mutate(do_repair = do_repair & (stata_reg_cmd_has_r_trans(cmd) | cmd=="")) %>%


  if (ignore_already_repaired) {
     regcheck = mutate(regcheck,do_repair = do_repair & repair_code=="")
  }

  # 1. Identify failed runs
  # Failure criteria: sb ran, but rb failed or coefficients mismatch
  failed_pids = regcheck$runid[ regcheck$do_repair]
  failed_pids
}

mrb_cache_failed_runs_data = function(mrb, pids, overwrite=FALSE, replace_target_with_keep = NULL) {
  restore.point("mrb_cache_reg_data")

  project_dir = mrb$project_dir
  cache_dir = file.path(project_dir, "drf/cached_dta")

  cache_files = file.path(cache_dir, paste0(pids,"_cache.dta"))
  if (!overwrite) {
    has = file.exists(cache_files)
    pids = pids[!has]
    cache_files = cache_files[!has]
  }

  if (length(pids)==0) {
    return(invisible(pids))
  }

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  # 2. Generate Stata script to save e(sample) filtered caches
  sc_df = repboxDRF::drf_stata_code_df(mrb$drf, runids = pids, path_merge = "load")

  # find regression command rows for pids
  cand_rows = which(sc_df$runid==sc_df$pid)
  rows = cand_rows[match(pids, sc_df$runid[cand_rows])]

  if (is.null(replace_target_with_keep)) {
     cmd_types = mrb$drf$run_df$cmd_type[match(pids, mrb$drf$run_df$runid)]
     replace_target_with_keep = cmd_types %in% c("reg", "quasi_reg")
  } else {
     replace_target_with_keep = rep(replace_target_with_keep, length.out = length(pids))
  }

  for (i in seq_along(rows)) {
    row = rows[i]
    if (replace_target_with_keep[i]) {
       # Replace the regression with a quiet execution, then keep if e(sample)
       # (Actually currently extracts keep if/in)
       keep_code = drf_stata_code_to_keep_if_in(sc_df$code[row])
       new_code = paste0(keep_code,"\n",
         "capture save \"", cache_files[i], "\", replace\n"
       )
       sc_df$code[row] = new_code
    } else {
       # Keep the data modification command and save afterwards
       new_code = paste0(sc_df$code[row], "\n",
         "capture save \"", cache_files[i], "\", replace\n"
       )
       sc_df$code[row] = new_code
    }
  }

  script_file = file.path(mrb$project_dir, "metareg/base/stata_code/mrb_repair.do")
  metaregBase:::drf_code_write(sc_df, script_file)

  cat("\nRunning Stata repair script...\n")
  mrb_run_stata_script(mrb, do_file = script_file)
}



mrb_cache_reg_data_old = function(mrb, pids, overwrite=FALSE) {
  restore.point("mrb_cache_reg_data")


  project_dir = mrb$project_dir
  cache_dir = file.path(project_dir, "drf/cached_dta")

  cache_files = file.path(cache_dir, paste0(pids,"_cache.dta"))
  if (!overwrite) {
    has = file.exists(cache_files)
    pids = pids[!has]
    cache_files = cache_files[!has]
  }

  if (length(pids)==0) {
    return(invisible(pids))
  }

  if (!dir.exists(cache_dir)) dir.create(cache_dir)

  # 2. Generate Stata script to save e(sample) filtered caches
  sc_df = repboxDRF::drf_stata_code_df(mrb$drf, runids = pids, path_merge = "load")

  # find regression command rows for pids
  cand_rows = which(sc_df$runid==sc_df$pid)
  rows = cand_rows[match(pids, sc_df$runid[cand_rows])]

  # Replace the regression with a quiet execution, then keep if e(sample)
  keep_code = drf_stata_code_to_keep_if_in(sc_df$code[rows])

  new_code = paste0(keep_code,"\n",
    "capture save \"", cache_files, "\", replace\n"
  )
  sc_df$code[rows] = new_code
  script_file = file.path(mrb$project_dir, "metareg/base/stata_code/mrb_repair.do")
  drf_code_write(sc_df, script_file)

  cat("\nRunning Stata repair script...\n")
  mrb_run_stata_script(mrb, do_file = script_file)
}

stata_reg_cmd_has_r_trans = function(cmd) {
  sr_df = regtranslate::stata_to_r_cmds_df()
  cmd %in% sr_df$stata_cmd
}
