# Tools for systematic testing the metaregBase pipeline
# Will be used for specific test projects

# Key: Generate a report file (including run logs and sources) that
# can be given to an AI that can search for the bug.

example = function() {
  project_dir = "~/repbox/projects/test"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  outfile = mrb_run_as_test(project_dir, file.path(project_dir, "run/run_test.R"))
  rstudioapi::navigateToFile(outfile, line=10000)
}


mrb_run_as_test = function(project_dir, run_script, check_reg=TRUE, navigate=TRUE) {
  restore.point("mrb_run_as_test")

  try(library(repboxRun), silent=TRUE)
  library(metaregBase)

  if (!file.exists(run_script)) {
    stop(paste0("The script to run ", run_script, " does not exist."))
  }
  test_dir = file.path(project_dir, "test_report")
  if (!dir.exists(test_dir)) dir.create(test_dir)
  outfile = file.path(test_dir, "test_report.Rmd")
  con = file(outfile, open="wt")
  on.exit(try(close(con), silent=TRUE), add = TRUE)
  add = function(...) {
    txt = paste0(c(...), collapse="")
    txt = paste0("\n",txt, "\n")
    if (is.null(txt)) return(invisible())
    writeLines(txt, con)
  }
  add("Report of test run for project ", project_dir)

  add("# do files in the project")
  do_files = list.files(file.path(project_dir,"mod"), glob2rx("*.do"), recursive = TRUE, full.names = TRUE)
  do_files = do_files[!startsWith(basename(do_files), "repbox_")]
  add(files_to_md_fences(do_files))

  add("# The R test script that is run")
  add(files_to_md_fences(run_script))

  add("# Run log of the script")
  source_with_log(run_script,log_con=con)

  parcels = list()

  if (check_reg) {
    add("# Comparision of Stata and R regressions")
    parcels = repdb_load_parcels(
      project_dir,
      c("regcoef_diff", "reg", "reg_cmdpart", "regvar", "regxvar", "regsource"),
      parcels = parcels
    )
    txt = mrb_test_regcoeff_diff(project_dir, parcels)
    add(txt)
  }


  try(close(con), silent=TRUE)

  if (navigate) {
    rstudioapi::navigateToFile(outfile, line=10000)
  }
  outfile
}

files_to_md_fences = function(files, path) {
  if (length(files)==0) return(NULL)
  txt = sapply(files, function(file) {
    txt = readLines(file, warn=FALSE)
    ext = tools::file_ext(file)
    txt = c(paste0("## ", file), paste0("```{",ext,"}"), txt, "```")
    paste0(txt, collapse="\n")
  })
  txt = paste0(txt, collapse="\n")
  txt
}


