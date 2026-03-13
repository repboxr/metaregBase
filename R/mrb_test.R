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


mrb_run_as_test = function(project_dir, run_script, check_reg=TRUE, navigate=TRUE, show_data_samples=TRUE) {
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

  add("# do files in the project")
  do_files = list.files(file.path(project_dir,"mod"), glob2rx("*.do"), recursive = TRUE, full.names = TRUE)
  do_files = do_files[!startsWith(basename(do_files), "repbox_")]
  add(files_to_md_fences(do_files))

  add("# The R test script that is run")
  add(files_to_md_fences(run_script))

  add("# Run log of the script")
  source_with_log(run_script, log_con=con)

  parcels = list()

  if (check_reg) {
    add("# Comparison of Stata and R regressions")
    parcels = repboxDB::repdb_load_parcels(
      project_dir,
      c("regcoef_diff", "reg", "reg_rb", "reg_cmdpart", "regvar", "regxvar", "regsource", "regcoef", "regcoef_rb"),
      parcels = parcels
    )

    drf = repboxDRF::drf_load(project_dir, parcels = parcels)
    txt = mrb_test_report(project_dir, parcels, drf, show_data_samples = show_data_samples)
    add(txt)
  }

  try(close(con), silent=TRUE)

  if (navigate) {
    rstudioapi::navigateToFile(outfile, line=10000)
  }
  outfile
}

mrb_test_report = function(project_dir, parcels, drf, max_cases = Inf, show_data_samples = TRUE) {
  restore.point("mrb_test_report")

  flags = mrb_test_generate_flags(project_dir, parcels, drf)
  if (NROW(flags) == 0) return("\n No regressions found to compare.")

  probs = flags %>% filter(is_problem)

  num_all_reg = n_distinct(flags$runid)
  num_all_prob = n_distinct(probs$runid)

  if (num_all_prob == 0) {
    return("\n-- In all regressions R and Stata coefficients and standard errors match, and all results are generated successfully. --")
  }

  if (is.finite(max_cases) && num_all_prob > max_cases) {
    probs = probs[seq_len(max_cases), , drop = FALSE]
  }

  txt = lapply(seq_len(NROW(probs)), function(i) {
    row = probs[i, ]
    runid = row$runid

    header = paste0("## runid ", runid)

    # Compile the specific flags into a readable summary string
    issues = c()
    if (!row$has_sb) issues = c(issues, "Missing Stata (sb) results")
    if (!row$has_rb) issues = c(issues, "Missing R (rb) results")
    if (row$coef_diff) issues = c(issues, "Coefficients differ")
    if (row$se_diff) issues = c(issues, "Standard errors differ (coefficients match)")

    issues_text = paste0("**Issues detected:** ", paste(issues, collapse = ", "))

    # Get the table of differences if both Stata and R have outputs
    diff_res = list(text = "", note = "")
    if (row$has_sb && row$has_rb && (row$coef_diff || row$se_diff)) {
      coef_pair = mrb_test_get_regcoef_pair(runid = runid, variant1 = "rb", variant2 = "sb", parcels = parcels)

      if (!is.null(coef_pair$co1) && !is.null(coef_pair$co2) && NROW(coef_pair$co1) > 0 && NROW(coef_pair$co2) > 0) {
        diff_tab = coef_diff_table(coef_pair$co1, coef_pair$co2)
        diff_res = mrb_test_regcoef_diff_text(diff_tab)
      } else {
         diff_res$text = "- Could not create coefficient comparison table. One variant returned an empty table."
      }
    }

    # Generate the comprehensive path of Stata and R code
    code_path_text = mrb_test_code_path(project_dir, runid, parcels, drf)

    # Generate the data preview text (Original & Regression Datasets)
    data_preview_text = ""
    if (show_data_samples) {
      data_preview_text = mrb_test_data_preview_text(runid, drf, parcels)
    }

    block = c(
      header,
      "",
      issues_text,
      "",
      if (nzchar(diff_res$note)) c(diff_res$note, "") else NULL,
      if (nzchar(diff_res$text)) c(diff_res$text, "") else NULL,
      if (nzchar(data_preview_text)) c(data_preview_text, "") else NULL,
      "### Code Path",
      "```r",
      code_path_text,
      "```"
    )

    paste0(block, collapse = "\n")
  })

  head = paste0("In ", num_all_prob, " of ", num_all_reg, " regressions, problems were detected. ", NROW(probs), " problematic cases are shown below.")

  paste0(c(head, unlist(txt)), collapse = "\n\n")
}

mrb_test_data_preview_text = function(runid, drf, parcels, n = 5) {
  restore.point("mrb_test_data_preview_text")

  # Get the path for this runid to find the first step (the data load state)
  path_df = drf$path_df %>% filter(pid == !!runid, runid <= !!runid) %>% arrange(runid)
  if (NROW(path_df) == 0) return("")

  first_runid = path_df$runid[1]

  # 1. Fetch Original Data
  dat_org = try(repboxDRF::drf_get_data(first_runid, drf = drf), silent = TRUE)

  # 2. Fetch Regression Data
  regvar = parcel_for_runid(parcels$regvar, runid)
  regxvar = if (!is.null(parcels$regxvar)) parcel_for_runid(parcels$regxvar, runid) else tibble()

  dat_reg = try(mrb_get_regression_data(runid, drf, regvar, regxvar), silent = TRUE)

  res = c("### Data Samples")

  format_df_sample = function(df, title, is_reg = FALSE) {
    if (inherits(df, "try-error") || is.null(df)) return(paste0("Could not load ", title, "."))
    if (NROW(df) == 0) return(paste0(title, " is empty."))

    # Filter to relevant columns for dat_reg to avoid overwhelming the output
    if (is_reg && NROW(regvar) > 0) {
       rel_cols = unique(c(regvar$var, regvar$cterm, regxvar$cterm))
       rel_cols = intersect(rel_cols, colnames(df))
       if (length(rel_cols) > 0) {
         df = df[, rel_cols, drop = FALSE]
       }
    }

    # Using tibble prints nicely across terminal widths truncating extra cols safely
    df_tibble = tibble::as_tibble(df)

    out_head = paste0(capture.output(print(head(df_tibble, n))), collapse = "\n")

    if (NROW(df) > n) {
       out_tail = paste0(capture.output(print(tail(df_tibble, n))), collapse = "\n")
       paste0("**", title, " (Head):**\n```text\n", out_head, "\n```\n\n**", title, " (Tail):**\n```text\n", out_tail, "\n```")
    } else {
       paste0("**", title, " (All rows):**\n```text\n", out_head, "\n```")
    }
  }

  res = c(res, format_df_sample(dat_org, "Original Data (First Step)", is_reg = FALSE), "")
  res = c(res, format_df_sample(dat_reg, "Regression Data (Final Step)", is_reg = TRUE), "")

  paste0(res, collapse = "\n")
}




















mrb_test_generate_flags = function(project_dir, parcels, drf = NULL, max_rel_diff_tol = 0.01, max_deviation_tol = 1e-6) {
  restore.point("mrb_test_generate_flags")
  if (is.null(drf)) drf = repboxDRF::drf_load(project_dir, parcels = parcels)

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

      is_problem = !has_sb | !has_rb | coef_diff | se_diff
    )

  return(res)
}
