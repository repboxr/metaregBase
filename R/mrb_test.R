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
  do_files = list.files(file.path(project_dir,"org"), glob2rx("*.do"), recursive = TRUE, full.names = TRUE)
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

mrb_test_report = function(project_dir, parcels, drf, opts = mrb_test_opts()) {
  restore.point("mrb_test_report")
  library(metaregBase)
  max_cases = opts$max_cases

  flags = mrb_test_generate_flags(project_dir, parcels, drf, opts = opts)

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

  if (!is.null(opts$just_runid)) {
    flags = flags[flags$runid %in% opts$just_runid, , drop = FALSE]
  }

  probs = flags %>% filter(is_problem | is_note)

  num_all_reg = n_distinct(flags$runid)
  num_all_prob = sum(flags$is_problem, na.rm = TRUE)
  num_all_note = sum(flags$is_note & !flags$is_problem, na.rm = TRUE)

  if (num_all_prob == 0 && num_all_note == 0) {
    return(paste0(stata_err_text, "\n-- In all regressions R and Stata coefficients and standard errors match, and all results are generated successfully. --"))
  }

  probs = probs %>%
    arrange(desc(severity), desc(is_problem), runid)

  overview_df = probs %>%
    mutate(problem_combo = ifelse(problem_combo == "", "Unclassified", problem_combo)) %>%
    group_by(problem_combo) %>%
    summarize(
      runids = paste(sort(runid), collapse = ", "),
      severity = max(severity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(severity), problem_combo)

  overview_text = paste0(
    "Problem types and runids:\n\n",
    paste0("  - ", overview_df$problem_combo, ": ", overview_df$runids, collapse = "\n")
  )

  if (is.finite(max_cases) && NROW(probs) > max_cases) {
    probs = probs[seq_len(max_cases), , drop = FALSE]
  }

  txt = lapply(seq_len(NROW(probs)), function(i) {
    row = probs[i, ]
    runid = row$runid

    header = paste0("## runid ", runid)

    run_cmd = NA_character_
    if (!is.null(parcels$reg) && NROW(parcels$reg) > 0 && "cmd" %in% names(parcels$reg)) {
      reg_row = parcels$reg[parcels$reg$runid == runid, , drop = FALSE]
      if (NROW(reg_row) > 0) {
        run_cmd = as.character(reg_row$cmd[1])
      }
    }
    if ((is.na(run_cmd) || !nzchar(run_cmd)) && !is.null(parcels$reg_rb) && NROW(parcels$reg_rb) > 0 && "cmd" %in% names(parcels$reg_rb)) {
      reg_rb_row = parcels$reg_rb[parcels$reg_rb$runid == runid, , drop = FALSE]
      if (NROW(reg_rb_row) > 0) {
        run_cmd = as.character(reg_rb_row$cmd[1])
      }
    }

    issues = character()
    if (isTRUE(row$error_in_r)) {
      if (!is.na(row$error_msg) && nzchar(row$error_msg)) {
        issues = c(issues, paste0("R translation/execution failed: ", row$error_msg))
      } else {
        issues = c(issues, "R translation/execution failed")
      }
    }

    if (!row$has_sb && row$has_rb) issues = c(issues, "Missing metareg Stata (sb) results (but R produced results)")
    if (!row$has_sb && !row$has_rb) issues = c(issues, "Neither metareg Stata (sb) nor R (rb) produced results")
    if (row$has_sb && !row$has_rb && !isTRUE(row$error_in_r)) issues = c(issues, "Missing R (rb) results (Stata produced results)")
    if (row$has_sb && !row$has_so) issues = c(issues, "Missing original Stata (so) results (metareg Stata produced results)")

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

    notes = character()
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

    diff_res_text = ""
    diff_res_note = ""

    if (row$has_sb && row$has_so && (isTRUE(row$sb_so_coef_diff) || isTRUE(row$sb_so_se_diff))) {
      coef_pair = mrb_test_get_regcoef_pair(runid = runid, variant1 = "sb", variant2 = "so", parcels = parcels)
      if (!is.null(coef_pair$co1) && !is.null(coef_pair$co2) && NROW(coef_pair$co1) > 0 && NROW(coef_pair$co2) > 0) {
        diff_tab = coef_diff_table(coef_pair$co1, coef_pair$co2)
        d_res = mrb_test_regcoef_diff_text(diff_tab, variant1 = "sb", variant2 = "so", cmd = run_cmd, opts = opts)
        diff_res_text = paste0(diff_res_text, "\n\n**sb vs so difference:**\n", d_res$text)
        diff_res_note = paste0(diff_res_note, " ", d_res$note)
      } else {
        diff_res_text = paste0(diff_res_text, "\n- Could not create sb vs so comparison table.")
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

    source_text = mrb_test_source_text(runid, parcels = parcels, drf = drf)
    code_path_text = mrb_test_code_path(project_dir, runid, parcels, drf, opts = opts)
    data_preview_text = mrb_test_data_preview_text(runid, drf, parcels, opts = opts)

    block = c(
      header,
      "",
      if (nzchar(issues_text)) c(issues_text, "") else NULL,
      if (nzchar(notes_text)) c(notes_text, "") else NULL,
      if (nzchar(diff_res_note)) c(trimws(diff_res_note), "") else NULL,
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

  paste0(c(stata_err_text, head, overview_text, unlist(txt)), collapse = "\n\n")
}








mrb_test_generate_flags = function(project_dir, parcels, drf = NULL, opts = mrb_test_opts()) {
  restore.point("mrb_test_generate_flags")
  if (is.null(drf)) drf = repboxDRF::drf_load(project_dir, parcels = parcels)
  max_rel_diff_tol = opts$max_rel_diff_tol
  max_deviation_tol = opts$max_deviation_tol

  pids = repboxDRF::drf_pids(drf)
  if (length(pids) == 0) return(tibble())

  get_run_cmd = function(runid) {
    cmds = character()

    if (!is.null(parcels$reg) && NROW(parcels$reg) > 0 && "cmd" %in% names(parcels$reg)) {
      cmds = c(cmds, as.character(parcels$reg$cmd[parcels$reg$runid == runid]))
    }
    if (!is.null(parcels$reg_rb) && NROW(parcels$reg_rb) > 0 && "cmd" %in% names(parcels$reg_rb)) {
      cmds = c(cmds, as.character(parcels$reg_rb$cmd[parcels$reg_rb$runid == runid]))
    }

    cmds = cmds[!is.na(cmds) & nzchar(cmds)]
    if (length(cmds) == 0) return(NA_character_)
    cmds[[1]]
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

  problem_labels = function(row) {
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
      labels = c(labels, "sb vs so: coef differ & SE differ")
    } else if (isTRUE(row$sb_so_coef_diff[[1]])) {
      labels = c(labels, "sb vs so: coef differ")
    } else if (isTRUE(row$sb_so_se_diff[[1]])) {
      labels = c(labels, "sb vs so: SE differ")
    }

    if (isTRUE(row$sb_rb_coef_diff[[1]]) && isTRUE(row$sb_rb_se_diff[[1]])) {
      labels = c(labels, "sb vs rb: coef differ & SE differ")
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

  severity_of = function(row) {
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

  sb_runs = if (!is.null(parcels$regcoef) && NROW(parcels$regcoef) > 0) unique(parcels$regcoef$runid) else integer()
  rb_runs = if (!is.null(parcels$regcoef_rb) && NROW(parcels$regcoef_rb) > 0) unique(parcels$regcoef_rb$runid) else integer()
  so_runs = if (!is.null(parcels$regcoef_so) && NROW(parcels$regcoef_so) > 0) unique(parcels$regcoef_so$runid) else integer()

  res = tibble(runid = pids)
  res$has_sb = res$runid %in% sb_runs
  res$has_rb = res$runid %in% rb_runs
  res$has_so = res$runid %in% so_runs

  res$error_in_r = FALSE
  res$error_msg = ""

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
  }) %>% bind_rows()

  res = left_join(res, pair_res, by = "runid") %>%
    mutate(
      is_problem = (has_sb != has_so) | ((has_sb != has_rb) & !error_in_r) | error_in_r | sb_so_diff | sb_rb_diff,
      is_note = !has_sb | !has_rb | !has_so
    )

  combo_li = lapply(seq_len(NROW(res)), function(i) {
    problem_labels(res[i, , drop = FALSE])
  })

  res$problem_combo = vapply(combo_li, function(x) paste(x, collapse = "; "), character(1))
  res$severity = vapply(seq_len(NROW(res)), function(i) severity_of(res[i, , drop = FALSE]), numeric(1))

  return(res)
}


