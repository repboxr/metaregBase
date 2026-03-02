mr_header_exists = function(project_dir, metaid, repdb.dir = file.path(project_dir,"metareg",metaid,"repdb")) {
  header.file = paste0(repdb.dir,"/", metaid,"_header.Rds")
  file.exists(header.file)
}

mr_set_no_run_header = function(mr, system_info="", total_runtime = NA, se_parser_version = get_se_parser_version(), problem=mr[["problem"]], comment="") {
  if (is.null(problem)) problem = ""
  header = list(timestamp=Sys.time(), num_studied = 0, num_problem = 0, num_large_deviation =0, os_type = .Platform$OS.type,se_parser_version=se_parser_version, r_version = as.character(getRversion()), stata_version=as.numeric(mr$stata_version), system_info=system_info, total_runtime = total_runtime,  comment=comment, problem=problem)
  cols = c("metaid","main_metaid","artid","version","dap_version","stata_version")
  header[cols] = mr[cols]
  mr[["header"]] = header
  mr

}


mr_get_header = function(mr) {
  mr[["header"]]
}

mr_set_header = function(mr, regcheck, system_info="", total_runtime = as.numeric(Sys.time()) - as.numeric(mr$run_start_time), se_parser_version = get_se_parser_version(), problem=mr[["problem"]], comment="") {
  restore.point("mr_get_header")
  if (is.null(problem)) problem = ""

  if (missing(regcheck)) {
    stop("You must provide the argument regcheck so that the header can store how many steps with problems were in the analysis. You can create a raw version by calling

regcheck = mr_get_regcheck_after_run(mr, variant=\"your_variant\")

But you may want to adapt the resulting data frame. If for some reason you don't want to store any regcheck info, set regcheck=NULL when calling mr_get_header.")
  }

  problem_steps = unique(regcheck$step[is.true(regcheck$problem != "")])
  large_dev_steps = unique(regcheck$step[is.true(regcheck$deviation > regcheck$tolerable_deviation)]) %>% setdiff(problem_steps)

  header = list(timestamp=Sys.time(), num_studied = length(mr_get_asteps(mr)), num_problem = length(problem_steps), num_large_deviation = length(large_dev_steps), os_type = .Platform$OS.type,se_parser_version=se_parser_version, r_version = as.character(getRversion()), stata_version=as.numeric(mr$stata_version), system_info=system_info, total_runtime = total_runtime,  comment=comment, problem=problem)
  cols = c("metaid","main_metaid","artid","version","dap_version","stata_version")
  header[cols] = mr[cols]
  mr[["header"]] = header
  mr
}

mr_save_header = function(mr) {
  restore.point("mr_save_header")
  header = mr_get_header(mr)
  if (is.null(header)) {
    cat("\nNo header was specified during the analysis. You should call mr_set_header in your study_agg_fun or stata_agg_fun. Every metastudy should create this header info which will be automatically saved. Only if you are VERY sure that you don't want to store header information, in mr_opts the argument save.header=FALSE. (But I don't know why you would want to do that).")
  }
  saveRDS(list(header=header), file.path(mr$repdb_out_dir,paste0(header$metaid,"_header.Rds")))
}


example = function() {
  project.dirs = list.files("~/repbox/projects_reg",full.names = TRUE)
  metaids = "base"
  headers = mr_load_headers(project.dirs, metaids)
}

mr_get_header_files = function(project.dirs, metaids) {
  restore.point("mr_get_header_files")
  grid = expand.grid(project_dir=project.dirs, metaid = metaids,stringsAsFactors = FALSE)
  grid$parcel.dir = file.path(grid$project_dir,"metareg",grid$metaid,"repdb")
  paste0(grid$parcel.dir,"/",grid$metaid,"_header.Rds")

}

mr_load_headers = function(project.dirs=NULL, metaids=NULL, header.files = mr_get_header_files(project.dirs, metaids)) {
  restore.point("mr_load_header")
  header.files = header.files[file.exists(header.files)]
  li = lapply(header.files, function(file) {
    readRDS(file)$header
  })
  bind_rows(li)
}

example = function() {
  project.dirs = list.files("~/repbox/projects_reg",full.names = TRUE)
  project_dir = "/home/rstudio/repbox/projects_reg/testsupp"
  for (project_dir in project.dirs) {
    extract_header_from_parcel_and_save(project_dir, "base","reg_core", overwrite=TRUE)
  }
}



extract_header_from_parcel_and_save = function(project_dir, metaid, parcel, overwrite=FALSE) {
  restore.point("helper_extract_and_save_header")
  parcel.file = file.path(project_dir, "metareg",metaid,"repdb",paste0(parcel,".Rds"))
  if (!file.exists(parcel.file)) {
    cat(parcel.file, " not found.\n")
    return()
  }
  header.file = file.path(project_dir, "metareg",metaid,"repdb",paste0(metaid,"_header.Rds"))
  if (file.exists(header.file) & !overwrite) return()
  li = readRDS(parcel.file)
  header = li$header
  if (!is.null(header)) {
    saveRDS(list(header=header),header.file)
    cat("\nHeader parcel ", header.file, " saved.\n")
  }
}
