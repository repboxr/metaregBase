# runs a complete metaregBase analysis

example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)


  drf = drf_load(project_dir)
}

mrb_run_all = function(project_dir, drf=drf_load(project_dir)) {
  restore.point("mrb_run_all")
  mrb = mrb_init(project_dir, drf=drf)
  mrb = mrb_make_cmdpart_parcel(mrb)
  mrb_full_stata_script(mrb)

}

mrb_init = function(project_dir=drf$project_dir, drf=NULL) {
  project_dir = normalizePath(project_dir)
  if (is.null(drf)) {
    drf = drf_load(project_dir)
  }


  mrb = list(drf=drf, project_dir=project_dir, repdb_dir = file.path(project_dir, "repdb"), mrb_dir = file.path(project_dir, "metareg","base"), parcels=drf$parcels, reg_runids = unique(drf$path_df$pid))
  mrb
}




mrb_make_cmdpart_parcel = function(mrb, overwrite=FALSE) {
  restore.point("mrb_make_cmdpart")

  if (!overwrite) {
    if (repboxDB::repdb_has_parcel(mrb$project_dir, "cmdpart")) {
      return(mrb)
    }
  }

  run_df = mrb$drf$run_df
  rows = which(run_df$cmd_type %in% c("reg", "quasi_reg"))
  cp_df = cmdparts_of_stata_reg(run_df$cmdline[rows]) %>%
    arrange(str_row)
  cp_df$runid = run_df$runid[cp_df$str_row]


  mrb$parcels[["cmdpart"]] = list(cmdpart = cp_df)

  repboxDB::repdb_save_parcels(mrb$parcels["cmdpart"],dir=mrb$repdb_dir)
  mrb
}
