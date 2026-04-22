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
mrb_repair_failed_runs = function(project_dir = mrb$project_dir, mrb=NULL) {
  restore.point("mrb_repair_failed_runs")

  if (is.null(mrb)) {
    mrb = mrb_init(project_dir)
  }

  drf_clear_mcache()

  failed_pids = mrb_get_failed_rb_runids(mrb=mrb)
  if (length(failed_pids) == 0) {
    cat("\nNo failed runs to repair.\n")
    return(mrb)
  }

  cat("\nRepair attempt by caching regression data for runids: ", paste(failed_pids, collapse=", "), "\n")
  mrb_cache_reg_data(mrb,pids=failed_pids)

  mrb$drf = drf_apply_caches(mrb$drf, just_pids=failed_pids)

  # Rerun base to reconstruct regvar (in case xi variables were dumped into cache)
  mrb = mrb_run_r_base(mrb, just_pids = failed_pids)

  # Rerun reg
  mrb = mrb_run_r_reg(mrb, just_pids = failed_pids)

  # Re-evaluate regcheck for the whole dataset with the updated level
  mrb = mrb_make_regcheck_parcel(mrb, just_pids=failed_pids, repair_code = "c")

  return(mrb)
}

mrb_get_failed_rb_runids = function(mrb, parcels=mrb$parcels) {
  restore.point("mrb_get_failed_rb_runids")
  parcels = repboxDB::repdb_load_parcels(mrb$project_dir, "regcheck", parcels)

  regcheck = parcels$regcheck
  if (is.null(regcheck)) {
    cat("\nNo regcheck parcel found. Run mrb_make_regcheck_parcel() first.\n")
    return(NULL)
  }

  # 1. Identify failed runs
  # Failure criteria: sb ran, but rb failed or coefficients mismatch
  failed_pids = regcheck$runid[regcheck$sb_did_run & (!regcheck$rb_did_run | !regcheck$rb_sb_coef_same)]
  failed_pids
}

mrb_cache_reg_data = function(mrb, pids, overwrite=FALSE) {
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
