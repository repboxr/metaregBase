# TO DO:

# Perhaps: Cache for import statements does not need an R error in data preparation wrong rb results should suffice.

# mrb_repair_via_caches shall automatically ignore errors. Ideally, we just build the caches and then re-run mrb_repair_via_ignore. We just need to update repair mode and re-run when original repair mode was just i.


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
mrb_repair_via_ignore = function(project_dir = mrb$project_dir, mrb = NULL, max_reg = NULL, pids = NULL, rerun=FALSE, repair_code = "i") {
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

  cat("\nRepair attempt (", repair_code, ") for runids: ", paste(pids, collapse = ", "), "\n", sep="")

  # Ensure r_err_runids is loaded/synced
  mrb$drf = repboxDRF:::drf_sync_r_err_runids(mrb$drf)

  # Load existing regrepair parcel so we can detect previously failed ignore attempts
  mrb$parcels = repboxDB::repdb_load_parcels(mrb$project_dir, "regrepair", mrb$parcels)

  if (!rerun) {
    regrepair = repboxDB::repdb_null_to_empty(mrb$parcels$regrepair,"regrepair")

    # TO DO: ignore already performed repair attempts
    #        if same cached runid was used
    cached_runid_df = repboxDRF::drf_get_cached_runids_by_pid(mrb$drf, pids) %>% rename(runid=pid)


    regrepair = regrepair %>%
      filter(repair_code==!!repair_code) %>%
      semi_join(cached_runid_df, by=c("runid", "cached_runid"))
    pids = setdiff(pids, regrepair$runid)
  }
  if (length(pids) == 0) {
    cat("\nNo failed runs that have not been already tried to be repaired with code ", repair_code, ".\n")
    return(mrb)
  }

  mrb = mrb_run_r_base(mrb = mrb, just_pids=pids,continue_on_error = TRUE)
  mrb = mrb_run_r_reg(mrb, just_pids=pids, continue_on_error=TRUE)

  mrb = mrb_make_regcheck_parcel(mrb, just_pids=pids, repair_code=repair_code)
  mrb = mrb_regcheck_to_regrepair(mrb, pids=pids)

  mrb
}

mrb_regcheck_to_regrepair = function(mrb, pids) {
  restore.point("mrb_regcheck_to_regrepair")
  parcels = repboxDB::repdb_load_parcels(mrb$project_dir, c("regrepair", "regcheck"), parcels=mrb$parcels)

  df = parcels$regcheck %>% filter(runid %in% pids)
  regrepair = parcels$regrepair

  if (!is.null(regrepair)) {
    regrepair = regrepair %>%
      anti_join(df, by=c("repair_code","runid","cached_runid"))
  }

  parcels$regrepair = bind_rows(regrepair, df)

  res  = repboxDB::repdb_save_parcels( parcels["regrepair"], file.path(mrb$project_dir, "repdb"))

  mrb$parcels = parcels
  mrb
}


mrb_repair_paths_with_imports_via_cache = function(project_dir = mrb$project_dir, mrb = NULL,max_cache=20, max_reg = Inf) {
  mrb_repair_via_cache(project_dir, mrb,max_cache=max_cache, max_reg=max_reg, only_paths_with_import = TRUE)
}

mrb_repair_paths_with_r_fail_cmds_via_cache = function(project_dir = mrb$project_dir, mrb = NULL, max_cache=10, max_reg = Inf) {
  mrb_repair_via_cache(project_dir, mrb,max_cache=max_cache, max_reg=max_reg, only_paths_with_cmds=repboxDRF::drf_cmds_to_cache_if_r_reg_fails())
}


#' Cache-based repair: add strategic data caches, re-run failed regressions.
#'
#' For each failed pid that still needs repair after the ignore pass,
#' determine the best cache position, generate it in Stata, and re-run.
mrb_repair_via_cache = function(project_dir = mrb$project_dir, mrb = NULL,max_cache=20, max_reg = Inf, only_paths_with_import = FALSE, only_paths_with_predict = FALSE, only_paths_with_cmds = NULL) {
  restore.point("mrb_repair_via_cache")

  if (is.null(mrb)) {
    mrb = mrb_init(project_dir)
    mrb = mrb_agg_stata(mrb, skip_if_has = TRUE)
  }

  drf_clear_mcache()
  failed_pids = mrb_get_to_repair_runids(mrb = mrb, only_paths_with_import = only_paths_with_import, only_paths_with_predict = only_paths_with_predict, only_paths_with_cmds = only_paths_with_cmds)

  if (max_reg < length(failed_pids)) {
      failed_pids = head(failed_pids, max_reg)
  }

  if (length(failed_pids) == 0) {
    cat("\nNo failed runs left to repair via cache.\n")
    return(mrb)
  }

  cat("\nCache repair attempt for runids: ", paste(failed_pids, collapse = ", "), "\n")

  mrb$drf = repboxDRF:::drf_sync_r_err_runids(mrb$drf)

  caches_made = 0

  while (length(failed_pids) > 0 & caches_made < max_cache) {
    cache_runid = mrb_determine_repair_cache_runid(mrb, failed_pids = failed_pids)

    if (is.null(cache_runid)) {
      cat(sprintf("\nCannot determine cache runid for remaining pids: %s. Stopping cache repair.\n", paste(failed_pids, collapse=", ")))
      break
    }
    caches_made = caches_made+1

    # Identify which failed pids are going to be repaired by this cache
    pids_to_rerun = intersect(failed_pids, unique(mrb$drf$path_df$pid[mrb$drf$path_df$runid == cache_runid]))

    if (length(pids_to_rerun) == 0) {
      cat(sprintf("\nError: Cache runid %d does not serve any failed pids. Stopping to avoid infinite loop.\n", cache_runid))
      break
    }

    cat(sprintf("\nCaching at runid %d. This serves %d failed pid(s).\n", cache_runid, length(pids_to_rerun)))

    mrb_create_cache_at_runid(mrb, cache_runid = cache_runid, pid = pids_to_rerun[1], overwrite = TRUE)

    mrb$drf = repboxDRF:::drf_apply_caches(mrb$drf, just_pids = pids_to_rerun)

    mrb = mrb_repair_via_ignore(mrb = mrb, pids = pids_to_rerun, rerun = TRUE, repair_code = "c")

    # Remove the ones we just tried so we can find the next best cache for any remaining
    failed_pids = setdiff(failed_pids, pids_to_rerun)
  }

  mrb
}


stata_reg_cmd_has_r_trans = function(cmd) {
  sr_df = regtranslate::stata_to_r_cmds_df()
  cmd %in% sr_df$stata_cmd
}


#' Determine the best cache runid for repairing failed pids.
#'
#' Evaluates downstream runids that bypass specific error checkpoints and calls
#' the DRF cache scoring algorithm to find the sweet spot that serves the most failed pids.
mrb_determine_repair_cache_runid = function(mrb, failed_pids, drf = mrb$drf) {
  restore.point("mrb_determine_repair_cache_runid")

  path_df = drf$path_df %>% filter(pid %in% failed_pids)
  if (NROW(path_df) == 0) return(NULL)

  # Ensure we bypass known error runids for each pid
  valid_paths = lapply(failed_pids, function(p) {
    pdf = path_df %>% filter(pid == p)
    err_runids = intersect(pdf$runid, drf$r_err_runids)

    first_runid = pdf$runid[1]
    run_df_first = drf$run_df[drf$run_df$runid == first_runid, ]
    if (NROW(run_df_first) > 0 && isTRUE(run_df_first$has_file_cache[1])) {
      err_runids = setdiff(err_runids, first_runid)
    }

    if (length(err_runids) > 0) {
      max_err = max(err_runids)
      pdf = pdf %>% filter(runid >= max_err)
    }
    pdf
  })

  valid_path_df = bind_rows(valid_paths)
  if (NROW(valid_path_df) == 0) return(NULL)

  # Score the valid path segments to find the best cache
  res = repboxDRF::drf_suggest_best_cache_runid(valid_path_df, must_include_pids = failed_pids)

  if (is.null(res) || NROW(res) == 0) return(NULL)

  res$runid
}


mrb_get_to_repair_runids = function(mrb, parcels = mrb$parcels,  only_paths_with_import = FALSE, only_paths_with_predict = FALSE, only_paths_with_cmds=NULL) {
  restore.point("mrb_get_to_repair_runids")
  parcels = repboxDB::repdb_load_parcels(mrb$project_dir, c("regcheck", "reg", "regrepair"), parcels)

  if (only_paths_with_predict) {
    only_paths_with_cmds = union(c("predict","predictnl"), only_paths_with_cmds)
  }

  regcheck = parcels$regcheck
  if (is.null(regcheck)) {
    cat("\nNo regcheck parcel found. Run mrb_make_regcheck_parcel() first.\n")
    return(NULL)
  }
  reg = parcels$reg
  if (!repboxUtils::has_col(regcheck, "cmd") & NROW(regcheck) > 0) {
    if (!is.null(reg)) {
      regcheck = left_join(regcheck, reg %>% select(cmd, runid), by = "runid")
    } else {
      regcheck$cmd = ""
    }
  }

  regcheck$cmd = repboxUtils::na.val(regcheck$cmd, "")

  regcheck = regcheck %>%
    mutate(do_repair = (sb_raw_did_run) & (!rb_did_run)) %>%
    mutate(do_repair = do_repair | repboxUtils::is.true(!rb_sb_coef_same & !(has.substr(cmd, "logit") | has.substr(cmd, "probit")))) %>%
    mutate(do_repair = do_repair & (stata_reg_cmd_has_r_trans(cmd) | cmd == ""))

  failed_pids = regcheck$runid[regcheck$do_repair]

  if (only_paths_with_import && length(failed_pids) > 0 && !is.null(mrb$drf$path_df)) {
    first_runids = mrb$drf$path_df %>%
      filter(pid %in% failed_pids) %>%
      group_by(pid) %>%
      summarize(first_runid = min(runid), .groups = "drop")

    run_df_first = mrb$drf$run_df %>% filter(runid %in% first_runids$first_runid)

    is_import = run_df_first$cmd %in% c("import", "insheet", "infix") & !run_df_first$has_file_cache
    import_first_runids = run_df_first$runid[is_import]

    failed_pids = first_runids$pid[first_runids$first_runid %in% import_first_runids]
  }



  if (!is.null(only_paths_with_cmds) && length(failed_pids) > 0 && !is.null(mrb$drf$path_df)) {
    path_cmds = mrb$drf$path_df %>%
      filter(pid %in% failed_pids) %>%
      left_join(mrb$drf$run_df %>% select(runid, cmd), by = "runid")

    has_cmd = path_cmds %>%
      group_by(pid) %>%
      summarize(has_cmd = any(cmd %in% only_paths_with_cmds), .groups = "drop") %>%
      filter(has_cmd)

    failed_pids = intersect(failed_pids, has_cmd$pid)
  }


  # if (only_paths_with_predict && length(failed_pids) > 0 && !is.null(mrb$drf$path_df)) {
  #   path_cmds = mrb$drf$path_df %>%
  #     filter(pid %in% failed_pids) %>%
  #     left_join(mrb$drf$run_df %>% select(runid, cmd), by = "runid")
  #
  #   has_pred = path_cmds %>%
  #     group_by(pid) %>%
  #     summarize(has_predict = any(cmd %in% c("predict", "predictnl")), .groups = "drop") %>%
  #     filter(has_predict)
  #
  #   failed_pids = intersect(failed_pids, has_pred$pid)
  # }

  failed_pids
}

#' Generate a Stata cache at a specific intermediate runid
mrb_create_cache_at_runid = function(mrb=mrb_init(project_dir), cache_runid, overwrite = FALSE, project_dir=NULL, pid=NULL) {
  restore.point("mrb_create_cache_at_runid")
  project_dir = mrb$project_dir
  cache_dir = file.path(project_dir, "drf/cached_dta")

  cache_file = file.path(cache_dir, paste0(cache_runid, "_cache.dta"))
  if (file.exists(cache_file)) {
    if (!overwrite) {
      return(invisible(cache_runid))
    } else {
      file.remove(cache_file)
    }
  }

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  if (is.null(pid)) {
    path_df = mrb$drf$path_df
    row = which(path_df$runid==cache_runid)
    pid = first(path_df$pid[row])
  }

  # Get the Stata code path for this pid
  sc_df = repboxDRF::drf_stata_code_df(mrb$drf, runids = pid, path_merge = "none", write_e_r = FALSE, cache_after_runids = cache_runid,keep_non_mod_reg = TRUE)

  # Subset up to cache_runid
  rows = which(sc_df$runid <= cache_runid)
  if (length(rows) == 0) return(invisible(cache_runid))
  sc_df = sc_df[rows, , drop = FALSE]

  script_file = file.path(mrb$project_dir, "metareg/base/stata_code/mrb_repair.do")
  metaregBase:::drf_code_write(sc_df, script_file)

  cat("\nRunning Stata repair script to generate cache at runid", cache_runid, "...\n")
  mrb_run_stata_script(mrb, do_file = script_file)
}
