# runs a complete metaregBase analysis

example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)


  drf = drf_load(project_dir)
}

mrb_run_all = function(project_dir, drf=repboxDRF::drf_load(project_dir,apply_caches = FALSE), repair_failed=TRUE) {
  restore.point("mrb_run_all")

  mrb = mrb_init(project_dir, drf=drf)

  # Original Stata reproduction coefficients are independent input evidence.
  # Generate them before the metaregBase sb/rb pipeline, so they survive even
  # if mrb_run_r_base_step fails for some runids.
  mrb = mrb_make_so_parcels(mrb)

  mrb = mrb_full_stata_script(mrb)


  # removes previous mrb regression output files
  mrb_clear_stata_reg_out(project_dir)

  mrb = mrb_run_stata_script(mrb)
  # The Stata script can create new DRF cache files, e.g. after xi commands.
  mrb$drf = repboxDRF:::drf_apply_caches(mrb$drf)


  mrb = mrb_agg_stata(mrb)
  mrb = mrb_run_r_base(mrb)
  mrb = mrb_run_r_reg(mrb)
  mrb = mrb_make_regcheck_parcel(mrb)

  if (repair_failed) {
    mrb = mrb_repair_via_ignore(mrb=mrb)
    mrb = mrb_repair_paths_with_imports_via_cache(mrb=mrb)
    mrb = mrb_repair_paths_with_predict_via_cache(mrb=mrb, max_reg=10)
  }

  mrb
}

mrb_init = function(project_dir=drf$project_dir, drf=NULL,use_mcache=TRUE, mcache_files = use_mcache, mcache_runid=use_mcache, mcache_clear = TRUE, with_try=TRUE, custom_cache_min_score=100,custom_max_caches=20) {
  restore.point("mrb_init")
  project_dir = normalizePath(project_dir)
  if (is.null(drf)) {
    drf = drf_load(project_dir)
  }

  if (mcache_files | mcache_runid) {
    drf = repboxDRF:::drf_enable_mcache(drf,use_file_cache = mcache_files, use_runid_cache = mcache_runid,  clear = mcache_clear)
  }

  mrb = list(
    drf = drf,
    project_dir = project_dir,
    repdb_dir = file.path(project_dir, "repdb"),
    mrb_dir = file.path(project_dir, "metareg", "base"),
    parcels = drf$parcels,
    reg_runids = unique(drf$path_df$pid),
    with_try = with_try,
    custom_cache_min_score = custom_cache_min_score,
    custom_max_caches = custom_max_caches
  )
  mrb
}
