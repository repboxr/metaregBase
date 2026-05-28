# FILE: /home/rstudio/repbox/metaregBase/R/mrb_repair.R
#
# Contains the main repair entry point and cache-based repair strategies.
# Ignore-based repair (continue_on_error) lives in mrb_repair_ignore.R.
# Try to repair failed translations by ignoring
# R translation errors

example = function() {
  repboxRun::repbox_load_libs()
  project_dir = rb_get_project_dir("~/repbox/projects_test/test")
  mrb = mrb_init(project_dir)
  mrb = mrb_repair_via_ignore(mrb=mrb)
  rstudioapi::filesPaneNavigate(project_dir)
}


#' Automatically repair metaregBase R failures by continuing past R translation errors.
#'
#' Identifies regressions where sb ran successfully but rb failed.
#' For each failed pid, attempts to re-run data preparation with
#' continue_on_error=TRUE so that erroring data modification steps are skipped.
#' Tracks results in the regrepair parcel.
mrb_repair_via_ignore = function(project_dir = mrb$project_dir, mrb = NULL, max_reg = NULL, pids = NULL, rerun=FALSE) {
  restore.point("mrb_repair_via_ignore")

  if (is.null(mrb)) {
    mrb = mrb_init(project_dir)
    mrb = mrb_agg_stata(mrb, skip_if_has = TRUE)
  }

  drf_clear_mcache()

  if (is.null(pids)) {
    pids = mrb_get_to_repair_runids(mrb = mrb)
  }
  if (!is.null(max_reg)) {
    pids = head(pids, max_reg)
  }
  if (length(pids) == 0) {
    cat("\nNo failed runs to repair via ignore.\n")
    return(mrb)
  }

  cat("\nRepair attempt via ignore for runids: ", paste(pids, collapse = ", "), "\n")

  # Ensure r_err_runids is loaded/synced
  mrb$drf = repboxDRF:::drf_sync_r_err_runids(mrb$drf)

  # Load existing regrepair parcel so we can detect previously failed ignore attempts
  mrb$parcels = repboxDB::repdb_load_parcels(mrb$project_dir, "regrepair", mrb$parcels)

  if (!rerun) {
    regrepair = repdb_null_to_empty(mrb$parcels$regrepair,"regrepair")

    # TO DO: ignore already performed repair attempts
    #        if same cached runid was used
    cached_runid_df = drf_get_cached_runids_by_pid(mrb$drf, pids) %>% rename(runid=pid)


    regrepair = regrepair %>%
      filter(repair_code=="i") %>%
      semi_join(cached_runid_df, by=c("runid", "cached_runid"))
    pids = setdiff(pids, regrepair$runid)
  }


  mrb = mrb_run_r_base(mrb = mrb, just_pids=pids,continue_on_error = TRUE)
  mrb = mrb_run_r_reg(mrb, just_pids=pids, continue_on_error=TRUE)

  mrb = mrb_make_regcheck_parcel(mrb, just_pids=pids, repair_code="i")
  mrb = mrb_regcheck_to_regrepair(mrb, pids=pids)

  mrb
}

mrb_regcheck_to_regrepair = function(mrb, pids) {
  restore.point("mrb_regcheck_to_regrepair")
  parcels = repdb_load_parcels(mrb$project_dir, c("regrepair", "regcheck"), parcels=mrb$parcels)


  df = parcels$regcheck %>% filter(runid %in% pids)
  regrepair = parcels$regrepair

  if (!is.null(regrepair)) {
    regrepair = regrepair %>%
      anti_join(df, by=c("repair_code","runid","cached_runid"))
  }

  parcels$regrepair = bind_rows(regrepair, df)

  res  =repboxDB::repdb_save_parcels( parcels["regrepair"], file.path(mrb$project_dir, "repdb"))

  mrb$parcels = parcels
  mrb


}


#' Cache-based repair: add strategic data caches, re-run failed regressions.
#'
#' For each failed pid that still needs repair after the ignore pass,
#' determine the best cache position, generate it in Stata, and re-run.
mrb_repair_via_cache = function(project_dir = mrb$project_dir, mrb = NULL, max_reg = 10) {
  restore.point("mrb_repair_via_cache")

  if (is.null(mrb)) {
    mrb = mrb_init(project_dir)
    mrb = mrb_agg_stata(mrb, skip_if_has = TRUE)
  }

  drf_clear_mcache()

  failed_pids = mrb_get_to_repair_runids(mrb = mrb)
  if (!is.null(max_reg)) {
    failed_pids = head(failed_pids, max_reg)
  }
  if (length(failed_pids) == 0) {
    cat("\nNo failed runs left to repair via cache.\n")
    return(mrb)
  }

  cat("\nCache repair attempt for runids: ", paste(failed_pids, collapse = ", "), "\n")

  mrb$drf = repboxDRF:::drf_sync_r_err_runids(mrb$drf)

  for (pid in failed_pids) {
    drf = mrb$drf
    drf = repboxDRF:::drf_apply_caches(drf, just_pids = pid)

    cache_runid = mrb_determine_repair_cache_runid(mrb, pid = pid, drf = drf)

    if (is.null(cache_runid)) {
      cat(sprintf("\n  pid %d: cannot determine cache runid, skipping.\n", pid))
      next
    }

    cat(sprintf("\n  pid %d: caching at runid %d.\n", pid, cache_runid))

    mrb_cache_failed_runs_data(mrb, pids = cache_runid)

    drf = repboxDRF:::drf_apply_caches(drf, just_pids = pid)
    mrb$drf = drf

    mrb = mrb_run_r_base(mrb, just_pids = pid)
    mrb = mrb_run_r_reg(mrb, just_pids = pid)
    mrb = mrb_make_regcheck_parcel(mrb, just_pids = pid, repair_code = "c")

    # Record in regrepair
    rc = mrb$parcels$regcheck
    success = FALSE
    if (!is.null(rc) && pid %in% rc$runid) {
      success = isTRUE(rc$reg_ok[rc$runid == pid][1])
    }
    repair_row = mrb_regrepair_empty_row(
      pid,
      repair_code = "c",
      repair_attempted = TRUE,
      repair_success = success,
      first_err_runid = NA_integer_
    )
    mrb = mrb_update_regrepair_parcel(mrb, repair_row)
  }

  mrb
}


#' Determine the best cache runid for repairing a failed pid.
#'
#' Rules:
#'  1. If no translation errors on path, return NULL (nothing to cache).
#'  2. Find last error runid on path.
#'  3. Find the furthest downstream runid shared by all pids that use the error runid.
mrb_determine_repair_cache_runid = function(mrb, pid, drf = mrb$drf) {
  restore.point("mrb_determine_repair_cache_runid")

  path_df = drf$path_df[drf$path_df$pid == pid, ]
  if (NROW(path_df) == 0) return(NULL)

  err_runids = intersect(path_df$runid, drf$r_err_runids)

  # Exclude cached first runid from error consideration
  first_runid = path_df$runid[1]
  run_df_first = drf$run_df[drf$run_df$runid == first_runid, ]
  if (NROW(run_df_first) > 0 && isTRUE(run_df_first$has_file_cache[1])) {
    err_runids = setdiff(err_runids, first_runid)
  }

  if (length(err_runids) == 0) return(NULL)

  err_runid = max(err_runids)

  # Find all pids sharing this error runid
  all_pids_with_err = unique(drf$path_df$pid[drf$path_df$runid == err_runid])

  # Common runids across all those pids, at or after err_runid
  path_list = lapply(all_pids_with_err, function(p) {
    drf$path_df$runid[drf$path_df$pid == p]
  })
  common_runids = Reduce(intersect, path_list)
  valid_common = common_runids[common_runids >= err_runid]

  cache_runid = err_runid
  if (length(valid_common) > 0) {
    if (!err_runid %in% all_pids_with_err) {
      cands_not_pid = setdiff(valid_common, all_pids_with_err)
      if (length(cands_not_pid) > 0) {
        cache_runid = max(cands_not_pid)
      }
    }
  }

  cat(sprintf(
    "\n  Cache repair: pid=%d err_runid=%d -> cache_runid=%d\n",
    pid, err_runid, cache_runid
  ))

  cache_runid
}


#' Generate Stata caches for specified pids / runids.
mrb_cache_failed_runs_data = function(mrb, pids, overwrite = FALSE, replace_target_with_keep = NULL) {
  restore.point("mrb_cache_failed_runs_data")

  project_dir = mrb$project_dir
  cache_dir = file.path(project_dir, "drf/cached_dta")

  cache_files = file.path(cache_dir, paste0(pids, "_cache.dta"))
  if (!overwrite) {
    has = file.exists(cache_files)
    pids = pids[!has]
    cache_files = cache_files[!has]
  }

  if (length(pids) == 0) {
    return(invisible(pids))
  }

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  sc_df = repboxDRF::drf_stata_code_df(mrb$drf, runids = pids, path_merge = "load")

  cand_rows = which(sc_df$runid == sc_df$pid)
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
      keep_code = drf_stata_code_to_keep_if_in(sc_df$code[row])
      new_code = paste0(keep_code, "\n",
        "capture save \"", cache_files[i], "\", replace\n"
      )
      sc_df$code[row] = new_code
    } else {
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


stata_reg_cmd_has_r_trans = function(cmd) {
  sr_df = regtranslate::stata_to_r_cmds_df()
  cmd %in% sr_df$stata_cmd
}


mrb_get_to_repair_runids = function(mrb, parcels = mrb$parcels, ignore_already_repaired = TRUE) {
  restore.point("mrb_get_to_repair_runids")
  parcels = repboxDB::repdb_load_parcels(mrb$project_dir, c("regcheck", "reg", "regrepair"), parcels)

  regcheck = parcels$regcheck
  if (is.null(regcheck)) {
    cat("\nNo regcheck parcel found. Run mrb_make_regcheck_parcel() first.\n")
    return(NULL)
  }
  reg = parcels$reg
  if (!has_col(regcheck, "cmd") & NROW(regcheck) > 0) {
    if (!is.null(reg)) {
      regcheck = left_join(regcheck, reg %>% select(cmd, runid), by = "runid")
    } else {
      regcheck$cmd = ""
    }
  }
  regcheck$cmd = na.val(regcheck$cmd, "")

  regcheck = regcheck %>%
    mutate(do_repair = (sb_raw_did_run) & (!rb_did_run)) %>%
    mutate(do_repair = do_repair | is.true(!rb_sb_coef_same & !(has.substr(cmd, "logit") | has.substr(cmd, "probit")))) %>%
    mutate(do_repair = do_repair & (stata_reg_cmd_has_r_trans(cmd) | cmd == ""))

  if (ignore_already_repaired) {
    regrepair = parcels$regrepair
    if (!is.null(regrepair) && NROW(regrepair) > 0) {
      already_attempted = unique(regrepair$runid[isTRUE_VEC(regrepair$repair_attempted)])
      regcheck = mutate(regcheck, do_repair = do_repair & !runid %in% already_attempted)
    }
  }

  failed_pids = regcheck$runid[regcheck$do_repair]
  failed_pids
}


stata_reg_cmd_has_r_trans = function(cmd) {
  sr_df = regtranslate::stata_to_r_cmds_df()
  cmd %in% sr_df$stata_cmd
}

