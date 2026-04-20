# runs a complete metaregBase analysis

example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)


  drf = drf_load(project_dir)
}

mrb_run_all = function(project_dir, drf=repboxDRF::drf_load(project_dir)) {
  restore.point("mrb_run_all")
  mrb = mrb_init(project_dir, drf=drf)
  mrb = mrb_full_stata_script(mrb)
  # removes previous mrb regression output files
  mrb_clear_stata_reg_out(project_dir)
  mrb = mrb_run_stata_script(mrb)
  mrb = mrb_agg_stata(mrb)
  mrb = mrb_run_r_base(mrb)
  mrb = mrb_run_r_reg(mrb)
  mrb = mrb_make_regcheck_parcel(mrb)
  mrb
}

mrb_init = function(project_dir=drf$project_dir, drf=NULL,use_mcache=TRUE, mcache_files = use_mcache, mcache_runid=use_mcache, mcache_clear = TRUE) {
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
    reg_runids = unique(drf$path_df$pid)
  )
  mrb
}
