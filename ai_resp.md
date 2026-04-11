I added a small helper that pulls the original Stata source info from `regsource` when available, falls back to `drf$run_df` otherwise, and formats file path, original line range, and a source snippet. Then I wired that into the per-runid issue section of the test report.

!MODIFICATION mrb_test_source.R
scope = "file"
file = "R/mrb_test_source.R"
is_new_file = true
description = "Add helpers that recover and format original Stata source file and line information for a regression runid."
---------------------------------------------------------------------------------------------------------------------------

````r
mrb_test_first_non_missing_value = function(df, cols, default = NA) {
  restore.point("mrb_test_first_non_missing_value")

  cols = cols[cols %in% names(df)]
  if (length(cols) == 0) {
    return(default)
  }

  for (col in cols) {
    val = df[[col]][[1]]
    if (length(val) == 0) {
      next
    }
    if (is.character(val)) {
      if (!is.na(val) && nzchar(val)) {
        return(val)
      }
    } else if (!all(is.na(val))) {
      return(val)
    }
  }

  default
}


mrb_test_line_range_text = function(start, end = start) {
  restore.point("mrb_test_line_range_text")

  start = suppressWarnings(as.integer(start))
  end = suppressWarnings(as.integer(end))

  if (is.na(start) && is.na(end)) {
    return(NA_character_)
  }
  if (is.na(start)) {
    start = end
  }
  if (is.na(end)) {
    end = start
  }
  if (end < start) {
    end = start
  }
  if (start == end) {
    return(as.character(start))
  }
  paste0(start, "-", end)
}


mrb_test_get_regsource_row = function(runid, parcels = list(), drf = NULL) {
  restore.point("mrb_test_get_regsource_row")

  if (!is.null(parcels$regsource) && NROW(parcels$regsource) > 0) {
    src = parcels$regsource[parcels$regsource$runid == runid, , drop = FALSE]
    if (NROW(src) > 0) {
      return(src[1, , drop = FALSE])
    }
  }

  if (is.null(drf) || is.null(drf$run_df) || NROW(drf$run_df) == 0) {
    return(tibble())
  }

  run_df = drf$run_df[drf$run_df$runid == runid, , drop = FALSE]
  if (NROW(run_df) == 0) {
    return(tibble())
  }

  script_path = mrb_test_first_non_missing_value(
    run_df,
    c("file_path", "found_path", "root_file_path"),
    default = NA_character_
  )
  run_line = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(run_df, c("line"), default = NA_integer_)
  ))
  org_line = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(run_df, c("orgline"), default = NA_integer_)
  ))
  cmdline = mrb_test_first_non_missing_value(run_df, c("cmdline"), default = NA_character_)

  tibble(
    runid = runid,
    script_path = as.character(script_path),
    script_file = ifelse(is.na(script_path), NA_character_, basename(script_path)),
    code_line_start = ifelse(is.na(org_line), run_line, org_line),
    code_line_end = ifelse(is.na(org_line), run_line, org_line),
    line = run_line,
    cmdline = as.character(cmdline),
    text = NA_character_
  )
}


mrb_test_source_snippet_text = function(src_row) {
  restore.point("mrb_test_source_snippet_text")

  if (NROW(src_row) == 0) {
    return("")
  }
  if (!("text" %in% names(src_row))) {
    return("")
  }

  text = src_row$text[[1]]
  if (length(text) == 0 || is.na(text) || !nzchar(text)) {
    return("")
  }

  start = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(src_row, c("code_line_start", "line"), default = NA_integer_)
  ))
  end = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(src_row, c("code_line_end"), default = start)
  ))

  if (is.na(start) || is.na(end)) {
    return("")
  }

  lines = stringi::stri_split_lines1(as.character(text))
  if (length(lines) == 0) {
    return("")
  }

  start = max(1L, start)
  end = min(length(lines), end)
  if (end < start) {
    end = start
  }

  idx = seq.int(start, end)
  snippet = paste0(sprintf("%4d ", idx), lines[idx], collapse = "\n")
  snippet
}


mrb_test_source_text = function(runid, parcels = list(), drf = NULL) {
  restore.point("mrb_test_source_text")

  src = mrb_test_get_regsource_row(runid, parcels = parcels, drf = drf)
  if (NROW(src) == 0) {
    return("")
  }

  script_path = as.character(
    mrb_test_first_non_missing_value(
      src,
      c("script_path", "file_path", "found_path", "root_file_path"),
      default = NA_character_
    )
  )
  code_start = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(src, c("code_line_start", "orgline", "line"), default = NA_integer_)
  ))
  code_end = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(src, c("code_line_end"), default = code_start)
  ))
  run_line = suppressWarnings(as.integer(
    mrb_test_first_non_missing_value(src, c("line"), default = NA_integer_)
  ))
  cmdline = as.character(
    mrb_test_first_non_missing_value(src, c("cmdline"), default = NA_character_)
  )
  snippet = mrb_test_source_snippet_text(src)

  has_info =
    (!is.na(script_path) && nzchar(script_path)) ||
    !is.na(code_start) ||
    (!is.na(run_line) && run_line > 0) ||
    (!is.na(cmdline) && nzchar(cmdline)) ||
    nzchar(snippet)

  if (!has_info) {
    return("")
  }

  out = c("### Original Stata Source", "")

  if (!is.na(script_path) && nzchar(script_path)) {
    out = c(out, paste0("- File: `", script_path, "`"))
  }

  line_range = mrb_test_line_range_text(code_start, code_end)
  if (!is.na(line_range)) {
    out = c(out, paste0("- Original line", ifelse(grepl("-", line_range, fixed = TRUE), "s", ""), ": ", line_range))
  }

  if (!is.na(run_line) && (is.na(code_start) || run_line != code_start)) {
    out = c(out, paste0("- Parsed run line: ", run_line))
  }

  if (!nzchar(snippet) && !is.na(cmdline) && nzchar(cmdline)) {
    out = c(out, paste0("- Command: `", cmdline, "`"))
  }

  if (nzchar(snippet)) {
    out = c(out, "", "```stata", snippet, "```")
  }

  paste0(out, collapse = "\n")
}
````

!END_MODIFICATION mrb_test_source.R

!MODIFICATION mrb_test.R
scope = "file"
file = "R/mrb_test.R"
is_new_file = false
description = "Update the metaregBase test report so each shown problematic runid also includes the original Stata do file and source line information when available."
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

````r
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
    c("regcoef_diff", "reg", "reg_rb", "reg_cmdpart", "regvar", "regxvar", "regsource", "regcoef", "regcoef_rb", "regcoef_so"),
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
    if (isTRUE(row$error_in_r)) issues = c(issues, paste0("R translation/execution failed: ", row$error_msg))
    if (!row$has_sb && row$has_rb) issues = c(issues, "Missing metareg Stata (sb) results (but R produced results)")
    if (row$has_sb && !row$has_rb && !isTRUE(row$error_in_r)) issues = c(issues, "Missing R (rb) results (Stata produced results)")
    if (row$has_sb && !row$has_so) issues = c(issues, "Missing original Stata (so) results (metareg Stata produced results)")

    if (isTRUE(row$sb_so_diff)) issues = c(issues, "Metareg Stata (sb) and Original Stata (so) differ")
    if (isTRUE(row$sb_rb_diff)) issues = c(issues, "Metareg Stata (sb) and R (rb) differ")

    notes = c()
    if (!row$has_sb && !row$has_rb) notes = c(notes, "Both Stata and R yielded no results (e.g. empty data or expected abort)")
    if (!row$has_sb && row$has_rb) notes = c(notes, "Stata yielded no results (but R did)")
    if (row$has_sb && !row$has_rb && !isTRUE(row$error_in_r)) notes = c(notes, "R yielded no results (but Stata did)")

    issues_text = ""
    if (length(issues) > 0) {
      issues_text = paste0("**Issues detected:** ", paste(issues, collapse = ", "))
    }

    notes_text = ""
    if (length(notes) > 0) {
      notes_text = paste0("**Notes:** ", paste(notes, collapse = ", "))
    }

    # Get the table of differences if both Stata and R have outputs
    diff_res_text = ""
    diff_res_note = ""

    if (row$has_sb && row$has_so && isTRUE(row$sb_so_diff)) {
      coef_pair = mrb_test_get_regcoef_pair(runid = runid, variant1 = "sb", variant2 = "so", parcels = parcels)
      if (!is.null(coef_pair$co1) && !is.null(coef_pair$co2) && NROW(coef_pair$co1) > 0 && NROW(coef_pair$co2) > 0) {
        diff_tab = coef_diff_table(coef_pair$co1, coef_pair$co2)
        d_res = mrb_test_regcoef_diff_text(diff_tab, variant1="sb", variant2="so", opts=opts)
        diff_res_text = paste0(diff_res_text, "\n\n**sb vs so difference:**\n", d_res$text)
        diff_res_note = paste0(diff_res_note, " ", d_res$note)
      } else {
        diff_res_text = paste0(diff_res_text, "\n- Could not create sb vs so comparison table.")
      }
    }

    if (row$has_sb && row$has_rb && isTRUE(row$sb_rb_diff)) {
      coef_pair = mrb_test_get_regcoef_pair(runid = runid, variant1 = "sb", variant2 = "rb", parcels = parcels)
      if (!is.null(coef_pair$co1) && !is.null(coef_pair$co2) && NROW(coef_pair$co1) > 0 && NROW(coef_pair$co2) > 0) {
        diff_tab = coef_diff_table(coef_pair$co1, coef_pair$co2)
        d_res = mrb_test_regcoef_diff_text(diff_tab, variant1="sb", variant2="rb", opts=opts)
        diff_res_text = paste0(diff_res_text, "\n\n**sb vs rb difference:**\n", d_res$text)
        diff_res_note = paste0(diff_res_note, " ", d_res$note)
      } else {
        diff_res_text = paste0(diff_res_text, "\n- Could not create sb vs rb comparison table.")
      }
    }

    source_text = mrb_test_source_text(runid, parcels = parcels, drf = drf)

    # Generate the comprehensive path of Stata and R code
    code_path_text = mrb_test_code_path(project_dir, runid, parcels, drf, opts=opts)

    # Generate the data preview text (Original & Regression Datasets)
    data_preview_text = mrb_test_data_preview_text(runid, drf, parcels, opts=opts)

    block = c(
      header,
      "",
      if (nzchar(issues_text)) c(issues_text, "") else NULL,
      if (nzchar(notes_text)) c(notes_text, "") else NULL,
      if (nzchar(diff_res_note)) c(diff_res_note, "") else NULL,
      if (nzchar(diff_res_text)) c(diff_res_text, "") else NULL,
      if (nzchar(source_text)) c(source_text, "") else NULL,
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
  so_runs = unique(parcels$regcoef_so$runid)

  res$has_sb = res$runid %in% sb_runs
  res$has_rb = res$runid %in% rb_runs
  res$has_so = res$runid %in% so_runs

  if (!is.null(parcels$reg_rb) && NROW(parcels$reg_rb) > 0) {
    if (!has_col(parcels$reg_rb,"error_msg")) {
      parcels$reg_rb$error_msg = ""
    }
    res = left_join(res, parcels$reg_rb %>% select(runid, error_in_r, error_msg), by = "runid")
  } else {
    res$error_in_r = NA
    res$error_msg = NA
  }

  diff = parcels$regcoef_diff
  if (!is.null(diff) && NROW(diff) > 0) {
    diff_sb_rb = diff %>% filter(compare_what == "all", variant1 == "sb", variant2 == "rb") %>%
       select(runid, sb_rb_max_dev = max_deviation, sb_rb_max_rel = max_rel_diff, sb_rb_identical = identical)
    diff_sb_so = diff %>% filter(compare_what == "all", variant1 == "sb", variant2 == "so") %>%
       select(runid, sb_so_max_dev = max_deviation, sb_so_max_rel = max_rel_diff, sb_so_identical = identical)

    res = left_join(res, diff_sb_rb, by = "runid")
    res = left_join(res, diff_sb_so, by = "runid")
  } else {
    res$sb_rb_max_dev = NA_real_
    res$sb_rb_max_rel = NA_real_
    res$sb_rb_identical = NA
    res$sb_so_max_dev = NA_real_
    res$sb_so_max_rel = NA_real_
    res$sb_so_identical = NA
  }

  res = res %>%
    mutate(
      sb_so_diff = has_sb & has_so & (!isTRUE(sb_so_identical) &
                     (is.na(sb_so_max_dev) | sb_so_max_dev > max_deviation_tol |
                      is.na(sb_so_max_rel) | sb_so_max_rel > max_rel_diff_tol)),
      sb_rb_diff = has_sb & has_rb & (!isTRUE(sb_rb_identical) &
                     (is.na(sb_rb_max_dev) | sb_rb_max_dev > max_deviation_tol |
                      is.na(sb_rb_max_rel) | sb_rb_max_rel > max_rel_diff_tol)),

      is_problem = (has_sb != has_so) | (has_sb != has_rb & !isTRUE(error_in_r)) | isTRUE(error_in_r) | sb_so_diff | sb_rb_diff,

      is_note = !has_sb | !has_rb | !has_so
    )

  return(res)
}
````

!END_MODIFICATION mrb_test.R

This should make a shown case like runid 23 include a new "Original Stata Source" section with the do-file path, original line number or range, and a snippet when `regsource` contains the script text.
