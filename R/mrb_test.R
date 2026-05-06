# Tools for systematic testing the metaregBase pipeline
# Will be used for specific test projects
#
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


mrb_test_opts = function(
  show_do_files = FALSE,
  show_test_script = FALSE,
  show_test_script_log = FALSE,
  show_org_data = TRUE,
  show_reg_data = FALSE,
  show_pre_reg_data = FALSE,
  data_head_rows = 10,
  data_tail_rows = 0,
  data_width = 1000,
  max_cases = Inf,
  just_runid = NULL,
  ignore_flags = NULL,
  data_add_org_row = FALSE,
  max_rel_diff_tol = 0.01,
  max_deviation_tol = 1e-6
) {
  as.list(environment())
}


mrb_run_as_test = function(project_dir, run_script, navigate = FALSE, opts = mrb_test_opts()) {
  restore.point("mrb_run_as_test")

  try(library(repboxRun), silent = TRUE)
  library(metaregBase)

  if (!file.exists(run_script)) {
    stop(paste0("The script to run ", run_script, " does not exist."))
  }

  test_dir = file.path(project_dir, "reports")
  if (!dir.exists(test_dir)) dir.create(test_dir)

  outfile = file.path(test_dir, "test_report.Rmd")
  con = file(outfile, open = "wt")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  add = function(...) {
    txt = paste0(c(...), collapse = "")
    txt = paste0(txt, "\n")
    if (is.null(txt)) return(invisible())
    writeLines(txt, con)
  }

  add("# Report of test run for project ", project_dir)

  # Allows knitting to HTML for humans
  add("\n```{r setup, include=FALSE}
knitr::opts_chunk$set(eval = FALSE)\n```")

  if (isTRUE(opts$show_do_files)) {
    add("# do files in the project")
    do_files = list.files(file.path(project_dir, "org"), glob2rx("*.do"), recursive = TRUE, full.names = TRUE)
    do_files = do_files[!startsWith(basename(do_files), "repbox_")]
    add(files_to_md_fences(do_files))
  }

  if (opts$show_test_script) {
    add("# The R test script that is run")
    add(files_to_md_fences(run_script))
  }

  add("# Run log of the test script")
  if (opts$show_test_script_log) {
    source_with_log(run_script, log_con = con)
  } else {
    source(run_script, echo = TRUE, print.eval = TRUE)
  }

  parcels = list()
  parcels = repboxDB::repdb_load_parcels(
    project_dir,
    c(
      "regcheck",
      "regcoef_diff",
      "reg",
      "reg_rb",
      "reg_cmdpart",
      "regvar",
      "regxvar",
      "regsource",
      "regcoef",
      "regcoef_rb",
      "regcoef_so"
    ),
    parcels = parcels
  )

  drf = repboxDRF::drf_load(project_dir, parcels = parcels)
  txt = mrb_test_report(project_dir, parcels, drf, opts = opts)
  add(txt)

  try(close(con), silent = TRUE)

  if (navigate) {
    rstudioapi::navigateToFile(outfile, line = 10000)
  }

  outfile
}


mrb_test_report = function(project_dir, parcels, drf, opts = mrb_test_opts()) {
  restore.point("mrb_test_report")
  library(metaregBase)

  max_cases = opts$max_cases

  flags = mrb_test_generate_flags(project_dir, parcels, drf, opts = opts)
  flag_source = attr(flags, "source")
  if (is.null(flag_source)) flag_source = "reconstructed"

  stata_err_text = mrb_test_stata_error_text(drf)

  if (NROW(flags) == 0) {
    return(paste0(stata_err_text, "\n No regressions found to compare."))
  }

  if (!is.null(opts$just_runid)) {
    flags = flags[flags$runid %in% opts$just_runid, , drop = FALSE]
  }

  probs = flags %>% filter(is_problem | is_note)

  num_all_reg = n_distinct(flags$runid)
  num_all_prob = sum(flags$is_problem, na.rm = TRUE)
  num_all_prob_reg = n_distinct(flags$runid[is.true(flags$is_problem)])
  num_all_note = sum(flags$is_note & !flags$is_problem, na.rm = TRUE)

  source_note = mrb_test_flag_source_note(flag_source)

  if (num_all_prob == 0 && num_all_note == 0) {
    return(paste0(
      stata_err_text,
      "\n",
      source_note,
      "\n\n-- In all regressions R and Stata coefficients and standard errors match, and all results are generated successfully. --"
    ))
  }

  probs = probs %>%
    arrange(desc(severity), desc(is_problem), runid)

  overview_df = probs %>%
    mutate(problem_combo = ifelse(problem_combo == "", "Unclassified", problem_combo)) %>%
    group_by(problem_combo) %>%
    summarize(
      runids = paste(sort(unique(runid)), collapse = ", "),
      severity = max(severity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(severity), problem_combo)

  overview_text = paste0(
    source_note,
    "\n\nProblem types and runids:\n\n",
    paste0("  - ", overview_df$problem_combo, ": ", overview_df$runids, collapse = "\n")
  )

  if (is.finite(max_cases) && NROW(probs) > max_cases) {
    probs = probs[seq_len(max_cases), , drop = FALSE]
  }

  txt = lapply(seq_len(NROW(probs)), function(i) {
    mrb_test_report_runid_block(
      row = probs[i, ],
      project_dir = project_dir,
      parcels = parcels,
      drf = drf,
      opts = opts
    )
  })

  head = paste0("In ", num_all_prob_reg, " of ", num_all_reg, " regressions, problems were detected. ")
  if (num_all_note > 0) {
    head = paste0(head, "Also ", num_all_note, " notes were generated. ")
  }
  head = paste0(head, NROW(probs), " cases are shown below.")

  paste0(c(stata_err_text, head, overview_text, unlist(txt)), collapse = "\n\n")
}


mrb_test_stata_error_text = function(drf) {
  restore.point("mrb_test_stata_error_text")

  if (is.null(drf$run_df) || !"errcode" %in% names(drf$run_df)) {
    return("")
  }

  err_df = drf$run_df %>% filter(!is.na(errcode) & errcode != 0)
  if (NROW(err_df) == 0) {
    return("")
  }

  err_items = lapply(seq_len(NROW(err_df)), function(i) {
    file_info = if ("found_path" %in% names(err_df)) basename(err_df$found_path[i]) else "Unknown file"
    paste0(
      "**runid ", err_df$runid[i], "** (File: ", file_info, ")\n\n",
      "- **Error Code:** ", err_df$errcode[i], "\n",
      "```stata\n", err_df$cmdline[i], "\n```\n"
    )
  })

  paste0(
    "# Stata Reproduction Run Errors\n\n",
    "The following ", NROW(err_df), " Stata commands threw an error during the original reproduction run:\n\n",
    paste(err_items, collapse = "\n"),
    "\n\n"
  )
}


mrb_test_flag_source_note = function(flag_source) {
  restore.point("mrb_test_flag_source_note")

  if (identical(flag_source, "regcheck")) {
    paste0(
      "The report uses the fresh `regcheck` parcel as the primary source for flags. ",
      "`raw_did_run` means the underlying Stata run produced raw results, while ",
      "`did_run` means the corresponding repdb parcels were also generated."
    )
  } else {
    paste0(
      "No complete fresh `regcheck` parcel was available for the requested runids. ",
      "The report reconstructs flags from available result parcels."
    )
  }
}


mrb_test_report_runid_block = function(row, project_dir, parcels, drf, opts = mrb_test_opts()) {
  restore.point("mrb_test_report_runid_block")

  runid = row$runid
  header = paste0("## runid ", runid)

  run_cmd = mrb_test_run_cmd(runid, parcels)
  uses_regcheck = "from_regcheck" %in% names(row) && isTRUE(row$from_regcheck[[1]])

  if (uses_regcheck) {
    issue_note = mrb_test_issues_notes_from_regcheck_row(row)
  } else {
    issue_note = mrb_test_issues_notes_from_fallback_row(row)
  }

  issues = unique(issue_note$issues[nzchar(issue_note$issues)])
  notes = unique(issue_note$notes[nzchar(issue_note$notes)])

  issues_text = ""
  if (length(issues) > 0) {
    issues_text = paste0("**Issues detected:** ", paste(issues, collapse = ", "))
  }

  notes_text = ""
  if (length(notes) > 0) {
    notes_text = paste0("**Notes:** ", paste(notes, collapse = ", "))
  }

  diff_text = mrb_test_diff_details_text(row, runid, parcels, run_cmd, opts)
  source_text = mrb_test_source_text(runid, parcels = parcels, drf = drf)
  code_path_text = mrb_test_code_path(project_dir, runid, parcels, drf, opts = opts)
  data_preview_text = mrb_test_data_preview_text(runid, drf, parcels, opts = opts)

  block = c(
    header,
    "",
    if (nzchar(issues_text)) c(issues_text, "") else NULL,
    if (nzchar(notes_text)) c(notes_text, "") else NULL,
    if (nzchar(diff_text$note)) c(trimws(diff_text$note), "") else NULL,
    if (nzchar(diff_text$text)) c(diff_text$text, "") else NULL,
    if (nzchar(source_text)) c(source_text, "") else NULL,
    "### Code Path",
    "```r",
    code_path_text,
    "```",
    if (nzchar(data_preview_text)) c(data_preview_text, "") else NULL
  )

  paste0(block, collapse = "\n")
}


mrb_test_run_cmd = function(runid, parcels) {
  restore.point("mrb_test_run_cmd")

  if (!is.null(parcels$reg) && NROW(parcels$reg) > 0 && "cmd" %in% names(parcels$reg)) {
    reg_row = parcels$reg[parcels$reg$runid == runid, , drop = FALSE]
    if (NROW(reg_row) > 0) {
      cmd = as.character(reg_row$cmd[1])
      if (!is.na(cmd) && nzchar(cmd)) return(cmd)
    }
  }

  if (!is.null(parcels$reg_rb) && NROW(parcels$reg_rb) > 0 && "cmd" %in% names(parcels$reg_rb)) {
    reg_rb_row = parcels$reg_rb[parcels$reg_rb$runid == runid, , drop = FALSE]
    if (NROW(reg_rb_row) > 0) {
      cmd = as.character(reg_rb_row$cmd[1])
      if (!is.na(cmd) && nzchar(cmd)) return(cmd)
    }
  }

  NA_character_
}


mrb_test_issues_notes_from_regcheck_row = function(row) {
  restore.point("mrb_test_issues_notes_from_regcheck_row")

  issues = character()
  notes = character()

  if (!is.na(row$regcheck_problem) && nzchar(row$regcheck_problem)) {
    issues = c(issues, row$regcheck_problem)
  }

  if (!is.na(row$regcheck_comment) && nzchar(row$regcheck_comment)) {
    notes = c(notes, row$regcheck_comment)
  }

  if (isTRUE(row$so_raw_did_run) && !isTRUE(row$has_so)) {
    notes = c(notes, "Original Stata reproduction (so) ran, but the so repdb parcels were not generated.")
  } else if (!isTRUE(row$so_raw_did_run) && !isTRUE(row$has_so)) {
    issues = c(issues, "Original Stata reproduction (so) did not run.")
  }

  if (isTRUE(row$sb_raw_did_run) && !isTRUE(row$has_sb)) {
    notes = c(notes, "metaregBase Stata replication (sb) ran, but the sb repdb parcels were not generated.")
  } else if (!isTRUE(row$sb_raw_did_run) && !isTRUE(row$has_sb)) {
    issues = c(issues, "metaregBase Stata replication (sb) did not run.")
  }

  if (!isTRUE(row$has_rb) && !isTRUE(row$error_in_r)) {
    issues = c(issues, "metaregBase R replication (rb) did not run.")
  }

  if (isTRUE(row$has_sb) && isTRUE(row$has_so) && isTRUE(row$sb_so_diff)) {
    issues = c(issues, "regcheck reports that sb and so are not identical.")
  }

  if (isTRUE(row$has_sb) && isTRUE(row$has_rb) && isTRUE(row$sb_rb_coef_diff) && isTRUE(row$sb_rb_se_diff)) {
    issues = c(issues, "regcheck reports that rb and sb differ in coefficients and standard errors.")
  } else if (isTRUE(row$has_sb) && isTRUE(row$has_rb) && isTRUE(row$sb_rb_coef_diff)) {
    issues = c(issues, "regcheck reports that rb and sb coefficients differ by more than the tolerance.")
  } else if (isTRUE(row$has_sb) && isTRUE(row$has_rb) && isTRUE(row$sb_rb_se_diff)) {
    issues = c(issues, "regcheck reports that rb and sb standard errors differ by more than the tolerance.")
  }

  if (!is.na(row$repair_code) && nzchar(row$repair_code)) {
    notes = c(notes, paste0("Repair code: ", row$repair_code))
  }

  list(issues = issues, notes = notes)
}


mrb_test_issues_notes_from_fallback_row = function(row) {
  restore.point("mrb_test_issues_notes_from_fallback_row")

  issues = character()
  notes = character()

  if (isTRUE(row$error_in_r)) {
    if (!is.na(row$error_msg) && nzchar(row$error_msg)) {
      issues = c(issues, paste0("R translation/execution failed: ", row$error_msg))
    } else {
      issues = c(issues, "R translation/execution failed")
    }
  }

  if (!row$has_sb && !row$has_rb && !row$has_so) {
    issues = c(issues, "No replication worked (so), (sb) and (rb)")
  } else if (!row$has_sb && !row$has_rb) {
    issues = c(issues, "(so) worked but neither metareg Stata (sb) nor R (rb) produced results")
  } else if (!row$has_sb && row$has_rb && row$has_so) {
    issues = c(issues, "(so) and (rb) worked but missing metareg Stata (sb) results. Weird...")
  } else if (!row$has_sb && row$has_rb && !row$has_so) {
    issues = c(issues, "(rb) worked but missing Stata (so) and (sb) results. Weird...")
  } else if (row$has_sb && !row$has_rb && !isTRUE(row$error_in_r)) {
    issues = c(issues, "Missing R (rb) results (Stata produced results)")
  } else if (row$has_sb && !row$has_so) {
    issues = c(issues, "Missing original Stata (so) results (metareg Stata produced results)")
  }

  if (isTRUE(row$sb_so_coef_diff) && isTRUE(row$sb_so_se_diff)) {
    issues = c(issues, "Metareg Stata (sb) and Original Stata (so) differ in coefficients and SEs")
  } else if (isTRUE(row$sb_so_coef_diff)) {
    issues = c(issues, "Metareg Stata (sb) and Original Stata (so) differ in coefficients")
  } else if (isTRUE(row$sb_so_se_diff)) {
    issues = c(issues, "Metareg Stata (sb) and Original Stata (so) differ only in SEs")
  }

  if (isTRUE(row$sb_rb_coef_diff) && isTRUE(row$sb_rb_se_diff)) {
    issues = c(issues, "Metareg Stata (sb) and R (rb) differ in coefficients and SEs")
  } else if (isTRUE(row$sb_rb_coef_diff)) {
    issues = c(issues, "Metareg Stata (sb) and R (rb) differ in coefficients")
  } else if (isTRUE(row$sb_rb_se_diff)) {
    issues = c(issues, "Metareg Stata (sb) and R (rb) differ only in SEs")
  }

  if (!row$has_sb && !row$has_rb) notes = c(notes, "Both Stata and R yielded no results (e.g. empty data or expected abort)")
  if (!row$has_sb && row$has_rb) notes = c(notes, "Stata yielded no results (but R did)")
  if (row$has_sb && !row$has_rb && !isTRUE(row$error_in_r)) notes = c(notes, "R yielded no results (but Stata did)")

  list(issues = issues, notes = notes)
}


mrb_test_diff_details_text = function(row, runid, parcels, run_cmd, opts = mrb_test_opts()) {
  restore.point("mrb_test_diff_details_text")

  diff_res_text = ""
  diff_res_note = ""

  if (row$has_sb && row$has_so && (isTRUE(row$sb_so_coef_diff) || isTRUE(row$sb_so_se_diff) || isTRUE(row$sb_so_diff))) {
    coef_pair = mrb_test_get_regcoef_pair(runid = runid, variant1 = "sb", variant2 = "so", parcels = parcels)

    if (!is.null(coef_pair$co1) && !is.null(coef_pair$co2) && NROW(coef_pair$co1) > 0 && NROW(coef_pair$co2) > 0) {
      diff_tab = coef_diff_table(coef_pair$co1, coef_pair$co2)
      d_res = mrb_test_regcoef_diff_text(diff_tab, variant1 = "sb", variant2 = "so", cmd = run_cmd, opts = opts)
      diff_res_text = paste0(diff_res_text, "\n\n**sb vs so difference:**\n", d_res$text)
      diff_res_note = paste0(diff_res_note, " ", d_res$note)
    } else {
      diff_res_text = paste0(
        diff_res_text,
        "\n- regcheck reports an sb vs so difference, but the comparison table could not be reconstructed from coefficient parcels."
      )
    }
  }

  if (row$has_sb && row$has_rb && (isTRUE(row$sb_rb_coef_diff) || isTRUE(row$sb_rb_se_diff))) {
    coef_pair = mrb_test_get_regcoef_pair(runid = runid, variant1 = "sb", variant2 = "rb", parcels = parcels)

    if (!is.null(coef_pair$co1) && !is.null(coef_pair$co2) && NROW(coef_pair$co1) > 0 && NROW(coef_pair$co2) > 0) {
      diff_tab = coef_diff_table(coef_pair$co1, coef_pair$co2)
      d_res = mrb_test_regcoef_diff_text(diff_tab, variant1 = "sb", variant2 = "rb", cmd = run_cmd, opts = opts)
      diff_res_text = paste0(diff_res_text, "\n\n**sb vs rb difference:**\n", d_res$text)
      diff_res_note = paste0(diff_res_note, " ", d_res$note)
    } else {
      diff_res_text = paste0(diff_res_text, "\n- Could not create sb vs rb comparison table.")
    }
  }

  list(text = diff_res_text, note = diff_res_note)
}


mrb_test_generate_flags = function(project_dir, parcels, drf = NULL, opts = mrb_test_opts()) {
  restore.point("mrb_test_generate_flags")

  if (is.null(drf)) drf = repboxDRF::drf_load(project_dir, parcels = parcels)

  flags = NULL
  if (!is.null(parcels$regcheck)) {
    flags = mrb_test_flags_from_regcheck(project_dir, parcels, drf = drf, opts = opts)
  }

  if (!is.null(flags)) {
    attr(flags, "source") = "regcheck"
    return(flags)
  }

  flags = mrb_test_generate_flags_from_parcels(project_dir, parcels, drf = drf, opts = opts)
  attr(flags, "source") = "reconstructed"
  flags
}


mrb_test_flags_from_regcheck = function(project_dir, parcels, drf = NULL, opts = mrb_test_opts()) {
  restore.point("mrb_test_flags_from_regcheck")

  if (is.null(drf)) drf = repboxDRF::drf_load(project_dir, parcels = parcels)

  if (is.null(parcels$regcheck)) {
    parcels = repboxDB::repdb_load_parcels(project_dir, "regcheck", parcels = parcels)
  }

  regcheck = parcels$regcheck
  pids = repboxDRF::drf_pids(drf)

  if (!mrb_test_regcheck_is_usable(regcheck, pids, just_runid = opts$just_runid, project_dir = project_dir)) {
    return(NULL)
  }

  if (!is.null(opts$just_runid)) {
    regcheck = regcheck[regcheck$runid %in% opts$just_runid, , drop = FALSE]
  }

  regcheck = regcheck[regcheck$runid %in% pids, , drop = FALSE]
  regcheck = regcheck[match(intersect(pids, regcheck$runid), regcheck$runid), , drop = FALSE]

  res = tibble::tibble(
    runid = as.integer(regcheck$runid),
    from_regcheck = TRUE,

    has_so = mrb_test_bool_col(regcheck, "so_did_run"),
    has_sb = mrb_test_bool_col(regcheck, "sb_did_run"),
    has_rb = mrb_test_bool_col(regcheck, "rb_did_run"),

    so_raw_did_run = mrb_test_bool_col(regcheck, "so_raw_did_run"),
    sb_raw_did_run = mrb_test_bool_col(regcheck, "sb_raw_did_run"),

    reg_ok = mrb_test_bool_col(regcheck, "reg_ok"),
    sb_so_identical = mrb_test_bool_col(regcheck, "sb_so_identical", default = NA),
    rb_sb_coef_same = mrb_test_bool_col(regcheck, "rb_sb_coef_same", default = NA),
    rb_sb_se_same = mrb_test_bool_col(regcheck, "rb_sb_se_same", default = NA),

    regcheck_problem = mrb_test_chr_col(regcheck, "problem"),
    regcheck_comment = mrb_test_chr_col(regcheck, "comment"),
    repair_code = mrb_test_chr_col(regcheck, "repair_code"),

    error_msg = ""
  )

  res$error_in_r =
    !res$has_rb &
    stringi::stri_detect_fixed(res$regcheck_problem, "R replication rb failed:")

  res$error_msg = ifelse(
    res$error_in_r,
    stringi::stri_trim_both(
      stringi::stri_replace_first_fixed(res$regcheck_problem, "R replication rb failed:", "")
    ),
    ""
  )

  res$sb_so_diff = res$has_sb & res$has_so & !dplyr::coalesce(res$sb_so_identical, FALSE)
  res$sb_so_coef_diff = res$sb_so_diff
  res$sb_so_se_diff = FALSE

  res$sb_rb_coef_diff = res$has_sb & res$has_rb & !dplyr::coalesce(res$rb_sb_coef_same, FALSE)
  res$sb_rb_se_diff = res$has_sb & res$has_rb & !dplyr::coalesce(res$rb_sb_se_same, FALSE)
  res$sb_rb_diff = res$sb_rb_coef_diff | res$sb_rb_se_diff

  res$sb_rb_coef_max_dev = mrb_test_num_col(regcheck, "rb_sb_coef_max_dev")
  res$sb_rb_coef_max_rel = res$sb_rb_coef_max_dev
  res$sb_rb_se_max_dev = mrb_test_num_col(regcheck, "rb_sb_se_max_dev")
  res$sb_rb_se_max_rel = res$sb_rb_se_max_dev

  res$sb_rb_max_dev = pmax(res$sb_rb_coef_max_dev, res$sb_rb_se_max_dev, na.rm = TRUE)
  res$sb_rb_max_dev[is.infinite(res$sb_rb_max_dev)] = NA_real_
  res$sb_rb_max_rel = res$sb_rb_max_dev

  res$sb_so_max_dev = NA_real_
  res$sb_so_max_rel = NA_real_
  res$sb_so_coef_max_dev = NA_real_
  res$sb_so_coef_max_rel = NA_real_
  res$sb_so_se_max_dev = NA_real_
  res$sb_so_se_max_rel = NA_real_

  raw_parcel_note =
    (res$so_raw_did_run & !res$has_so) |
    (res$sb_raw_did_run & !res$has_sb)

  res$is_problem =
    !dplyr::coalesce(res$reg_ok, FALSE) &
    (
      nzchar(res$regcheck_problem) |
      !res$has_so | !res$has_sb | !res$has_rb |
      res$sb_so_diff | res$sb_rb_diff
    )

  res$is_note =
    raw_parcel_note |
    nzchar(res$regcheck_comment) |
    (!res$has_so | !res$has_sb | !res$has_rb)

  combo_li = lapply(seq_len(NROW(res)), function(i) {
    mrb_test_problem_labels_from_regcheck(res[i, , drop = FALSE])
  })

  res$problem_combo = unlist(combo_li, use.names = FALSE)
  res$severity = vapply(seq_len(NROW(res)), function(i) {
    mrb_test_severity_from_regcheck(res[i, , drop = FALSE])
  }, numeric(1))

  res
}


mrb_test_generate_flags_from_parcels = function(project_dir, parcels, drf = NULL, opts = mrb_test_opts()) {
  restore.point("mrb_test_generate_flags_from_parcels")

  if (is.null(drf)) drf = repboxDRF::drf_load(project_dir, parcels = parcels)

  max_rel_diff_tol = opts$max_rel_diff_tol
  max_deviation_tol = opts$max_deviation_tol

  pids = repboxDRF::drf_pids(drf)
  if (length(pids) == 0) return(tibble())

  get_run_cmd = function(runid) {
    mrb_test_run_cmd(runid, parcels)
  }

  pair_diff = function(runid, variant1, variant2) {
    pair = mrb_test_get_regcoef_pair(runid = runid, variant1 = variant1, variant2 = variant2, parcels = parcels)

    if (is.null(pair$co1) || is.null(pair$co2) || NROW(pair$co1) == 0 || NROW(pair$co2) == 0) {
      return(mrb_test_eval_diff_tab(NULL))
    }

    diff_tab = coef_diff_table(pair$co1, pair$co2)

    mrb_test_eval_diff_tab(
      diff_tab = diff_tab,
      cmd = get_run_cmd(runid),
      variant2 = variant2,
      max_rel_diff_tol = max_rel_diff_tol,
      max_deviation_tol = max_deviation_tol
    )
  }

  sb_runs = if (!is.null(parcels$regcoef) && NROW(parcels$regcoef) > 0) unique(parcels$regcoef$runid) else integer()
  rb_runs = if (!is.null(parcels$regcoef_rb) && NROW(parcels$regcoef_rb) > 0) unique(parcels$regcoef_rb$runid) else integer()
  so_runs = if (!is.null(parcels$regcoef_so) && NROW(parcels$regcoef_so) > 0) unique(parcels$regcoef_so$runid) else integer()

  res = tibble(runid = pids)
  res$from_regcheck = FALSE

  res$has_sb = res$runid %in% sb_runs
  res$has_rb = res$runid %in% rb_runs
  res$has_so = res$runid %in% so_runs

  # In fallback mode we cannot reliably distinguish raw_did_run from did_run.
  res$so_raw_did_run = res$has_so
  res$sb_raw_did_run = res$has_sb

  res$error_in_r = FALSE
  res$error_msg = ""
  res$regcheck_problem = ""
  res$regcheck_comment = ""
  res$repair_code = ""

  if (!is.null(parcels$reg_rb) && NROW(parcels$reg_rb) > 0) {
    if (!has_col(parcels$reg_rb, "error_msg")) {
      parcels$reg_rb$error_msg = ""
    }

    tmp = parcels$reg_rb %>%
      select(runid, error_in_r, error_msg)

    res = left_join(res, tmp, by = "runid", suffix = c("", "_new")) %>%
      mutate(
        error_in_r = coalesce(error_in_r_new, error_in_r),
        error_msg = coalesce(error_msg_new, error_msg)
      ) %>%
      select(-error_in_r_new, -error_msg_new)
  }

  pair_res = lapply(res$runid, function(runid) {
    sb_rb = pair_diff(runid, "sb", "rb")
    sb_so = pair_diff(runid, "sb", "so")

    tibble(
      runid = runid,

      sb_rb_diff = sb_rb$all_diff,
      sb_rb_coef_diff = sb_rb$coef_diff,
      sb_rb_se_diff = sb_rb$se_diff,
      sb_rb_max_dev = sb_rb$all_max_dev,
      sb_rb_max_rel = sb_rb$all_max_rel,
      sb_rb_coef_max_dev = sb_rb$coef_max_dev,
      sb_rb_coef_max_rel = sb_rb$coef_max_rel,
      sb_rb_se_max_dev = sb_rb$se_max_dev,
      sb_rb_se_max_rel = sb_rb$se_max_rel,

      sb_so_diff = sb_so$all_diff,
      sb_so_coef_diff = sb_so$coef_diff,
      sb_so_se_diff = sb_so$se_diff,
      sb_so_max_dev = sb_so$all_max_dev,
      sb_so_max_rel = sb_so$all_max_rel,
      sb_so_coef_max_dev = sb_so$coef_max_dev,
      sb_so_coef_max_rel = sb_so$coef_max_rel,
      sb_so_se_max_dev = sb_so$se_max_dev,
      sb_so_se_max_rel = sb_so$se_max_rel
    )
  }) %>%
    bind_rows()

  res = left_join(res, pair_res, by = "runid") %>%
    mutate(
      is_problem = (has_sb != has_so) | ((has_sb != has_rb) & !error_in_r) | error_in_r | sb_so_diff | sb_rb_diff,
      is_note = !has_sb | !has_rb | !has_so
    )

  combo_li = lapply(seq_len(NROW(res)), function(i) {
    mrb_test_problem_labels_from_parcels(res[i, , drop = FALSE])
  })

  res$problem_combo = vapply(combo_li, function(x) paste(x, collapse = "; "), character(1))
  res$severity = vapply(seq_len(NROW(res)), function(i) {
    mrb_test_severity_from_parcels(res[i, , drop = FALSE])
  }, numeric(1))

  res
}


mrb_test_problem_labels_from_parcels = function(row) {
  restore.point("mrb_test_problem_labels_from_parcels")

  labels = character()

  if (isTRUE(row$error_in_r[[1]])) {
    labels = c(labels, "R translation/execution failed")
  }

  if (!isTRUE(row$has_sb[[1]]) && !isTRUE(row$has_rb[[1]])) {
    labels = c(labels, "No sb or rb results")
  } else {
    if (!isTRUE(row$has_sb[[1]]) && isTRUE(row$has_rb[[1]])) {
      labels = c(labels, "Missing sb results")
    }

    if (isTRUE(row$has_sb[[1]]) && !isTRUE(row$has_rb[[1]]) && !isTRUE(row$error_in_r[[1]])) {
      labels = c(labels, "Missing rb results")
    }
  }

  if (isTRUE(row$has_sb[[1]]) && !isTRUE(row$has_so[[1]])) {
    labels = c(labels, "Missing so results")
  }

  if (isTRUE(row$sb_so_coef_diff[[1]]) && isTRUE(row$sb_so_se_diff[[1]])) {
    labels = c(labels, "sb vs so: coef differ and SE differ")
  } else if (isTRUE(row$sb_so_coef_diff[[1]])) {
    labels = c(labels, "sb vs so: coef differ")
  } else if (isTRUE(row$sb_so_se_diff[[1]])) {
    labels = c(labels, "sb vs so: SE differ")
  }

  if (isTRUE(row$sb_rb_coef_diff[[1]]) && isTRUE(row$sb_rb_se_diff[[1]])) {
    labels = c(labels, "sb vs rb: coef differ and SE differ")
  } else if (isTRUE(row$sb_rb_coef_diff[[1]])) {
    labels = c(labels, "sb vs rb: coef differ")
  } else if (isTRUE(row$sb_rb_se_diff[[1]])) {
    labels = c(labels, "sb vs rb: SE differ")
  }

  if (length(labels) == 0 && isTRUE(row$is_note[[1]])) {
    labels = c(labels, "Only note")
  }

  labels
}


mrb_test_severity_from_parcels = function(row) {
  restore.point("mrb_test_severity_from_parcels")

  score = 0L

  if (isTRUE(row$error_in_r[[1]])) {
    score = score + 1000L
  }
  if (!isTRUE(row$has_sb[[1]]) && isTRUE(row$has_rb[[1]])) {
    score = score + 900L
  }
  if (!isTRUE(row$has_sb[[1]]) && !isTRUE(row$has_rb[[1]])) {
    score = score + 850L
  }
  if (isTRUE(row$has_sb[[1]]) && !isTRUE(row$has_rb[[1]]) && !isTRUE(row$error_in_r[[1]])) {
    score = score + 800L
  }
  if (isTRUE(row$has_sb[[1]]) && !isTRUE(row$has_so[[1]])) {
    score = score + 700L
  }

  if (isTRUE(row$sb_rb_coef_diff[[1]])) {
    score = score + 400L
  }
  if (isTRUE(row$sb_so_coef_diff[[1]])) {
    score = score + 350L
  }
  if (isTRUE(row$sb_rb_se_diff[[1]])) {
    score = score + 200L
  }
  if (isTRUE(row$sb_so_se_diff[[1]])) {
    score = score + 150L
  }

  if (score == 0L && isTRUE(row$is_note[[1]])) {
    score = 50L
  }

  score
}


mrb_test_problem_labels_from_regcheck = function(row) {
  restore.point("mrb_test_problem_labels_from_regcheck")

  labels = character()

  problem = row$regcheck_problem[[1]]
  comment = row$regcheck_comment[[1]]

  if (!is.na(problem) && nzchar(problem)) {
    labels = c(labels, problem)
  }

  if (isTRUE(row$so_raw_did_run[[1]]) && !isTRUE(row$has_so[[1]])) {
    labels = c(labels, "so raw run succeeded but so parcels are missing")
  }
  if (isTRUE(row$sb_raw_did_run[[1]]) && !isTRUE(row$has_sb[[1]])) {
    labels = c(labels, "sb raw run succeeded but sb parcels are missing")
  }

  if (!isTRUE(row$so_raw_did_run[[1]]) && !isTRUE(row$has_so[[1]])) {
    labels = c(labels, "so raw run failed")
  }
  if (!isTRUE(row$sb_raw_did_run[[1]]) && !isTRUE(row$has_sb[[1]])) {
    labels = c(labels, "sb raw run failed")
  }

  if (!isTRUE(row$has_rb[[1]]) && !isTRUE(row$error_in_r[[1]])) {
    labels = c(labels, "rb did not run")
  }

  if (isTRUE(row$has_sb[[1]]) && isTRUE(row$has_so[[1]]) && isTRUE(row$sb_so_diff[[1]])) {
    labels = c(labels, "sb vs so: not identical")
  }

  if (isTRUE(row$has_sb[[1]]) && isTRUE(row$has_rb[[1]]) && isTRUE(row$sb_rb_coef_diff[[1]]) && isTRUE(row$sb_rb_se_diff[[1]])) {
    labels = c(labels, "sb vs rb: coef differ and SE differ")
  } else if (isTRUE(row$has_sb[[1]]) && isTRUE(row$has_rb[[1]]) && isTRUE(row$sb_rb_coef_diff[[1]])) {
    labels = c(labels, "sb vs rb: coef differ")
  } else if (isTRUE(row$has_sb[[1]]) && isTRUE(row$has_rb[[1]]) && isTRUE(row$sb_rb_se_diff[[1]])) {
    labels = c(labels, "sb vs rb: SE differ")
  }

  if (!is.na(comment) && nzchar(comment)) {
    labels = c(labels, paste0("Comment: ", comment))
  }

  if (length(labels) == 0 && isTRUE(row$is_note[[1]])) {
    labels = c(labels, "Only note")
  }

  paste(labels, collapse = "; ")
}


mrb_test_severity_from_regcheck = function(row) {
  restore.point("mrb_test_severity_from_regcheck")

  score = 0L

  if (isTRUE(row$error_in_r[[1]])) {
    score = score + 1000L
  }

  if (!isTRUE(row$has_sb[[1]]) && isTRUE(row$sb_raw_did_run[[1]])) {
    score = score + 920L
  } else if (!isTRUE(row$has_sb[[1]])) {
    score = score + 880L
  }

  if (!isTRUE(row$has_so[[1]]) && isTRUE(row$so_raw_did_run[[1]])) {
    score = score + 820L
  } else if (!isTRUE(row$has_so[[1]])) {
    score = score + 760L
  }

  if (!isTRUE(row$has_rb[[1]])) {
    score = score + 700L
  }

  if (isTRUE(row$sb_rb_coef_diff[[1]])) {
    score = score + 400L
  }
  if (isTRUE(row$sb_so_diff[[1]])) {
    score = score + 350L
  }
  if (isTRUE(row$sb_rb_se_diff[[1]])) {
    score = score + 200L
  }

  if (score == 0L && isTRUE(row$is_note[[1]])) {
    score = 50L
  }

  score
}


mrb_test_bool_col = function(df, col, default = FALSE) {
  if (!col %in% names(df)) {
    return(rep(default, NROW(df)))
  }

  x = df[[col]]
  x[is.na(x)] = default
  as.logical(x)
}


mrb_test_chr_col = function(df, col, default = "") {
  if (!col %in% names(df)) {
    return(rep(default, NROW(df)))
  }

  x = as.character(df[[col]])
  x[is.na(x)] = default
  x
}


mrb_test_num_col = function(df, col, default = NA_real_) {
  if (!col %in% names(df)) {
    return(rep(default, NROW(df)))
  }

  suppressWarnings(as.numeric(df[[col]]))
}


mrb_test_find_repdb_parcel_files = function(project_dir, parcel_names) {
  restore.point("mrb_test_find_repdb_parcel_files")

  repdb_dir = file.path(project_dir, "repdb")
  if (!dir.exists(repdb_dir)) {
    return(character())
  }

  files = list.files(repdb_dir, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    return(character())
  }

  base = basename(files)
  stem = tools::file_path_sans_ext(base)

  files[stem %in% parcel_names]
}


mrb_test_regcheck_input_files = function(project_dir) {
  restore.point("mrb_test_regcheck_input_files")

  parcel_files = mrb_test_find_repdb_parcel_files(
    project_dir,
    c("reg", "reg_rb", "regcoef", "regcoef_so", "regcoef_rb")
  )

  regtab_file = file.path(project_dir, "repbox/stata/regtab.Rds")

  stata_out_dir = file.path(project_dir, "metareg/base/stata_reg_out")
  stata_out_files = character()
  if (dir.exists(stata_out_dir)) {
    stata_out_files = list.files(
      stata_out_dir,
      pattern = "\\.(dta|txt|csv|tsv)$",
      full.names = TRUE
    )
  }

  files = c(parcel_files, regtab_file, stata_out_files)
  files[file.exists(files)]
}


mrb_test_regcheck_file = function(project_dir) {
  restore.point("mrb_test_regcheck_file")

  files = mrb_test_find_repdb_parcel_files(project_dir, "regcheck")
  if (length(files) == 0) {
    return(NA_character_)
  }

  info = file.info(files)
  files[which.max(info$mtime)]
}


mrb_test_regcheck_is_fresh = function(project_dir) {
  restore.point("mrb_test_regcheck_is_fresh")

  regcheck_file = mrb_test_regcheck_file(project_dir)
  if (length(regcheck_file) == 0 || is.na(regcheck_file) || !file.exists(regcheck_file)) {
    return(FALSE)
  }

  input_files = mrb_test_regcheck_input_files(project_dir)
  if (length(input_files) == 0) {
    return(TRUE)
  }

  regcheck_time = file.info(regcheck_file)$mtime[[1]]
  input_time = max(file.info(input_files)$mtime, na.rm = TRUE)

  !is.na(regcheck_time) && !is.na(input_time) && regcheck_time >= input_time
}


mrb_test_regcheck_is_usable = function(regcheck, pids, just_runid = NULL, project_dir = NULL) {
  restore.point("mrb_test_regcheck_is_usable")

  if (is.null(regcheck) || NROW(regcheck) == 0) {
    return(FALSE)
  }

  required = c(
    "runid", "reg_ok",
    "so_raw_did_run", "sb_raw_did_run",
    "so_did_run", "sb_did_run", "rb_did_run",
    "sb_so_identical",
    "rb_sb_coef_same", "rb_sb_coef_max_dev",
    "rb_sb_se_same", "rb_sb_se_max_dev",
    "problem", "comment"
  )

  if (!all(required %in% names(regcheck))) {
    return(FALSE)
  }

  if (any(duplicated(regcheck$runid))) {
    return(FALSE)
  }

  check_pids = pids
  if (!is.null(just_runid)) {
    check_pids = intersect(check_pids, just_runid)
  }

  if (length(check_pids) == 0) {
    return(FALSE)
  }

  if (!all(check_pids %in% regcheck$runid)) {
    return(FALSE)
  }

  if (!is.null(project_dir) && !mrb_test_regcheck_is_fresh(project_dir)) {
    return(FALSE)
  }

  TRUE
}
