# Tools for systematic testing the metaregBase pipeline
# Will be used for specific test projects

# Key: Generate a report file (including run logs and sources) that
# can be given to an AI that can search for the bug.

example = function() {
  project_dir = "~/repbox/projects/test"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  opts = mrb_test_opts()
  outfile = mrb_run_as_test(project_dir, file.path(project_dir, "run/run_test.R"))
  rstudioapi::navigateToFile(outfile, line=10000)
}

mrb_test_opts = function(show_org_data=TRUE, show_reg_data=TRUE, show_pre_reg_data=TRUE, data_head_rows=10, data_tail_rows=0,data_width=1000, max_cases=Inf, just_runid=NULL, ignore_flags=NULL, data_add_org_row=FALSE,  max_rel_diff_tol = 0.01,
  max_deviation_tol = 1e-6) {
  as.list(environment())
}



mrb_run_as_test = function(project_dir, run_script, navigate=TRUE, opts=mrb_test_opts()) {
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
    txt = paste0(txt, "\n")
    if (is.null(txt)) return(invisible())
    writeLines(txt, con)
  }

  add("# Report of test run for project ", project_dir)

  # Allows knitting to HTML for humans
  add("\n```{r setup, include=FALSE}
knitr::opts_chunk$set(eval = FALSE)\n```")

  add("# do files in the project")
  do_files = list.files(file.path(project_dir,"mod"), glob2rx("*.do"), recursive = TRUE, full.names = TRUE)
  do_files = do_files[!startsWith(basename(do_files), "repbox_")]
  add(files_to_md_fences(do_files))

  add("# The R test script that is run")
  add(files_to_md_fences(run_script))

  add("# Run log of the script")
  source_with_log(run_script, log_con=con)

  parcels = list()
  parcels = repboxDB::repdb_load_parcels(
    project_dir,
    c("regcoef_diff", "reg", "reg_rb", "reg_cmdpart", "regvar", "regxvar", "regsource", "regcoef", "regcoef_rb"),
    parcels = parcels
  )

  drf = repboxDRF::drf_load(project_dir, parcels = parcels)
  txt = mrb_test_report(project_dir, parcels, drf, opts=opts)
  add(txt)


  try(close(con), silent=TRUE)

  if (navigate) {
    rstudioapi::navigateToFile(outfile, line=10000)
  }
  outfile
}

mrb_test_report = function(project_dir, parcels, drf, opts=mrb_test_opts()) {
  restore.point("mrb_test_report")
  max_cases = opts$max_cases

  flags = mrb_test_generate_flags(project_dir, parcels, drf, opts=opts)

  # Extract Stata reproduction errors
  stata_err_text = ""
  if (!is.null(drf$run_df) && "errcode" %in% names(drf$run_df)) {
    err_df = drf$run_df %>% filter(!is.na(errcode) & errcode != 0)
    if (NROW(err_df) > 0) {
      stata_err_text = paste0(
        "# Stata Reproduction Run Errors\n\n",
        "The following ", NROW(err_df), " Stata commands threw an error during the original reproduction run:\n\n"
      )

      err_items = lapply(seq_len(NROW(err_df)), function(i) {
        file_info = if ("found_path" %in% names(err_df)) basename(err_df$found_path[i]) else "Unknown file"
        paste0(
          "**runid ", err_df$runid[i], "** (File: ", file_info, ")\n\n",
          "- **Error Code:** ", err_df$errcode[i], "\n",
          "```stata\n", err_df$cmdline[i], "\n```\n"
        )
      })
      stata_err_text = paste0(stata_err_text, paste(err_items, collapse = "\n"), "\n\n")
    }
  }

  if (NROW(flags) == 0) return(paste0(stata_err_text, "\n No regressions found to compare."))

  probs = flags %>% filter(is_problem | is_note)

  if (!is.null(opts$just_runid)) {
    probs = probs[probs$runid %in% opts$just_runid,]
  }

  num_all_reg = n_distinct(flags$runid)
  num_all_prob = sum(flags$is_problem, na.rm = TRUE)
  num_all_note = sum(flags$is_note & !flags$is_problem, na.rm = TRUE)

  if (num_all_prob == 0 && num_all_note == 0) {
    return(paste0(stata_err_text, "\n-- In all regressions R and Stata coefficients and standard errors match, and all results are generated successfully. --"))
  }

  if (is.finite(max_cases) && NROW(probs) > max_cases) {
    probs = probs[seq_len(max_cases), , drop = FALSE]
  }

  txt = lapply(seq_len(NROW(probs)), function(i) {
    row = probs[i, ]
    runid = row$runid

    header = paste0("## runid ", runid)

    # Compile the specific flags into a readable summary string
    issues = c()
    if (!row$has_sb && row$has_rb) issues = c(issues, "Missing Stata (sb) results (R produced results)")
    if (row$has_sb && !row$has_rb) issues = c(issues, "Missing R (rb) results (Stata produced results)")
    if (row$coef_diff) issues = c(issues, "Coefficients differ")
    if (row$se_diff) issues = c(issues, "Standard errors differ (coefficients match)")

    notes = c()
    if (!row$has_sb && !row$has_rb) notes = c(notes, "Both Stata and R yielded no results (e.g. empty data or expected abort)")
    if (!row$has_sb && row$has_rb) notes = c(notes, "Stata yielded no results (but R did)")
    if (row$has_sb && !row$has_rb) notes = c(notes, "R yielded no results (but Stata did)")

    issues_text = ""
    if (length(issues) > 0) {
      issues_text = paste0("**Issues detected:** ", paste(issues, collapse = ", "))
    }

    notes_text = ""
    if (length(notes) > 0) {
      notes_text = paste0("**Notes:** ", paste(notes, collapse = ", "))
    }

    # Get the table of differences if both Stata and R have outputs
    diff_res = list(text = "", note = "")
    if (row$has_sb && row$has_rb && (row$coef_diff || row$se_diff)) {
      coef_pair = mrb_test_get_regcoef_pair(runid = runid, variant1 = "rb", variant2 = "sb", parcels = parcels)

      if (!is.null(coef_pair$co1) && !is.null(coef_pair$co2) && NROW(coef_pair$co1) > 0 && NROW(coef_pair$co2) > 0) {
        diff_tab = coef_diff_table(coef_pair$co1, coef_pair$co2)
        diff_res = mrb_test_regcoef_diff_text(diff_tab, opts=opts)
      } else {
         diff_res$text = "- Could not create coefficient comparison table. One variant returned an empty table."
      }
    }

    # Generate the comprehensive path of Stata and R code
    code_path_text = mrb_test_code_path(project_dir, runid, parcels, drf, opts=opts)

    # Generate the data preview text (Original & Regression Datasets)
    data_preview_text = mrb_test_data_preview_text(runid, drf, parcels, opts=opts)

    block = c(
      header,
      "",
      if (nzchar(issues_text)) c(issues_text, "") else NULL,
      if (nzchar(notes_text)) c(notes_text, "") else NULL,
      if (nzchar(diff_res$note)) c(diff_res$note, "") else NULL,
      if (nzchar(diff_res$text)) c(diff_res$text, "") else NULL,
      "### Code Path",
      "```r",
      code_path_text,
      "```",
      if (nzchar(data_preview_text)) c(data_preview_text, "") else NULL
    )

    paste0(block, collapse = "\n")
  })

  head = paste0("In ", num_all_prob, " of ", num_all_reg, " regressions, problems were detected. ")
  if (num_all_note > 0) {
    head = paste0(head, "Also ", num_all_note, " notes were generated. ")
  }
  head = paste0(head, NROW(probs), " cases are shown below.")

  paste0(c(stata_err_text, head, unlist(txt)), collapse = "\n\n")
}







mrb_test_generate_flags = function(project_dir, parcels, drf = NULL,  opts=mrb_test_opts()) {
  restore.point("mrb_test_generate_flags")
  if (is.null(drf)) drf = repboxDRF::drf_load(project_dir, parcels = parcels)
  max_rel_diff_tol = opts$max_rel_diff_tol
  max_deviation_tol = opts$max_deviation_tol

  pids = repboxDRF::drf_pids(drf)
  if (length(pids) == 0) return(tibble())

  res = tibble(runid = pids)

  sb_runs = unique(parcels$regcoef$runid)
  rb_runs = unique(parcels$regcoef_rb$runid)

  res$has_sb = res$runid %in% sb_runs
  res$has_rb = res$runid %in% rb_runs

  diff = parcels$regcoef_diff
  if (!is.null(diff) && NROW(diff) > 0) {
    diff_all = diff %>%
      filter(compare_what == "all") %>%
      select(runid, max_rel_diff, max_deviation, identical)
    res = left_join(res, diff_all, by = "runid")

    diff_coef = diff %>%
      filter(compare_what == "coef") %>%
      select(runid, coef_rel_diff = max_rel_diff, coef_dev = max_deviation, coef_identical = identical)
    res = left_join(res, diff_coef, by = "runid")
  } else {
    res$max_rel_diff = NA_real_
    res$max_deviation = NA_real_
    res$identical = NA
    res$coef_rel_diff = NA_real_
    res$coef_dev = NA_real_
    res$coef_identical = NA
  }

  # Add logic to flag each individual issue
  res = res %>%
    mutate(
      coef_diff = has_sb & has_rb & (!isTRUE(coef_identical) &
                    (is.na(coef_rel_diff) | coef_rel_diff > max_rel_diff_tol |
                     is.na(coef_dev) | coef_dev > max_deviation_tol)),

      overall_diff = has_sb & has_rb & (!isTRUE(identical) &
                    (is.na(max_rel_diff) | max_rel_diff > max_rel_diff_tol |
                     is.na(max_deviation) | max_deviation > max_deviation_tol)),

      se_diff = overall_diff & !coef_diff,

      is_problem = (has_sb != has_rb) | coef_diff | se_diff,

      is_note = !has_sb | !has_rb
    )

  return(res)
}
