Yes. I would change it in three places.

`max_cases` is the parameter that limits how many examples are shown. It is defined in `mrb_test_opts()` and then read in `mrb_test_report()` via `max_cases = opts$max_cases`. In the updated `mrb_test_report()`, I sort cases by severity before truncating to `max_cases`, with coefficient problems ranked above SE-only problems.

I also fixed another report-side issue: `mrb_test_get_regcoef_pair()` did not include `regcoef_so`, so `sb` vs `so` comparisons could silently fail.

!MODIFICATION mrb_test_coef.R
scope = "file"
file = "/home/rstudio/repbox/metaregBase/R/mrb_test_coef.R"
description = "Improve report-side coefficient diff handling: include regcoef_so, add an intercept filter for fixest translations, and use that filter in the diff text/stat helpers."
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

````r
mrb_test_filter_ignored_intercept_diff = function(diff_tab, cmd = NA_character_, variant2 = "rb", ignore_intercept_cmds = mrb_cmds_ignore_intercept_in_r()) {
  restore.point("mrb_test_filter_ignored_intercept_diff")

  if (is.null(diff_tab) || NROW(diff_tab) == 0) {
    return(diff_tab)
  }

  cmd = as.character(cmd)[1]
  variant2 = as.character(variant2)[1]

  if (variant2 != "rb") {
    return(diff_tab)
  }
  if (is.na(cmd) || !nzchar(cmd) || !cmd %in% ignore_intercept_cmds) {
    return(diff_tab)
  }

  diff_tab %>%
    filter(cterm != "(Intercept)")
}


mrb_test_regcoef_diff_text = function(
  diff_tab,
  variant1 = "rb",
  variant2 = "sb",
  cmd = NA_character_,
  max_rel_diff_tol = opts$max_rel_diff_tol,
  max_deviation_tol = opts$max_deviation_tol,
  opts = mrb_test_opts()
) {
  restore.point("mrb_test_regcoef_diff_text")

  diff_tab = mrb_test_filter_ignored_intercept_diff(diff_tab, cmd = cmd, variant2 = variant2)

  if (is.null(diff_tab) || NROW(diff_tab) == 0) {
    return(list(text = "- No comparison rows available.", note = ""))
  }

  # Add safety layers and deviation checks to all rows
  tab = diff_tab %>%
    mutate(
      coef_missing_one = xor(is.na(coef_1), is.na(coef_2)),
      se_missing_one = xor(is.na(se_1), is.na(se_2)),

      # Safe numeric extraction substituting NAs with -Inf for pure sorting purposes
      safe_abs_coef = ifelse(is.na(abs_err_coef), -Inf, abs_err_coef),
      safe_rel_coef = ifelse(is.na(rel_err_coef), -Inf, rel_err_coef),
      safe_abs_se = ifelse(is.na(abs_err_se), -Inf, abs_err_se),
      safe_rel_se = ifelse(is.na(rel_err_se), -Inf, rel_err_se),

      is_coef_diff = !coef_missing_one & (safe_abs_coef > max_deviation_tol | safe_rel_coef > max_rel_diff_tol),
      is_se_diff = !se_missing_one & (safe_abs_se > max_deviation_tol | safe_rel_se > max_rel_diff_tol)
    )

  # Note if all coefficients match but SEs differ
  only_se_wrong = !any(tab$is_coef_diff, na.rm = TRUE) && !any(tab$coef_missing_one, na.rm = TRUE) && any(tab$is_se_diff, na.rm = TRUE)
  note = if (only_se_wrong) "Note: All coefficients match within tolerance; only standard errors differ." else ""

  # Category 3: NA in only one of the two settings
  cat_missing = tab %>%
    filter(coef_missing_one) %>%
    head(2)

  # Category 1: Top wrong coefficients
  cat_coef = tab %>%
    filter(is_coef_diff) %>%
    arrange(desc(safe_rel_coef), desc(safe_abs_coef)) %>%
    head(2)

  # Category 2: Wrong overall (focusing on SEs). Only added if the SE deviation dominates the Coef deviation
  max_rel_coef = suppressWarnings(max(tab$safe_rel_coef, na.rm = TRUE))
  max_rel_se = suppressWarnings(max(tab$safe_rel_se, na.rm = TRUE))

  cat_se = tibble()
  if (max_rel_se > max_rel_coef && max_rel_se > max_rel_diff_tol) {
    cat_se = tab %>%
      filter(is_se_diff & !coef_missing_one) %>%
      arrange(desc(safe_rel_se), desc(safe_abs_se)) %>%
      head(2)
  }

  # Combine categories and remove possible overlaps
  show_tab = bind_rows(cat_missing, cat_coef, cat_se)
  if ("eq" %in% colnames(show_tab)) {
    show_tab = distinct(show_tab, eq, cterm, .keep_all = TRUE)
  } else {
    show_tab = distinct(show_tab, cterm, .keep_all = TRUE)
  }

  if (NROW(show_tab) == 0) {
    return(list(text = "- No differing coefficients to show.", note = note))
  }

  # Assemble the exact layout format
  has_eq = "eq" %in% colnames(show_tab) && any(show_tab$eq != "", na.rm = TRUE)

  if (has_eq) {
    show = tibble(
      eq = show_tab$eq,
      cterm = show_tab$cterm,
      coef_1 = show_tab$coef_1,
      coef_2 = show_tab$coef_2,
      se_1 = show_tab$se_1,
      se_2 = show_tab$se_2
    )
    names(show)[3:4] = c(paste0("coef_", variant1), paste0("coef_", variant2))
    names(show)[5:6] = c(paste0("se_", variant1), paste0("se_", variant2))
  } else {
    show = tibble(
      cterm = show_tab$cterm,
      coef_1 = show_tab$coef_1,
      coef_2 = show_tab$coef_2,
      se_1 = show_tab$se_1,
      se_2 = show_tab$se_2
    )
    names(show)[2:3] = c(paste0("coef_", variant1), paste0("coef_", variant2))
    names(show)[4:5] = c(paste0("se_", variant1), paste0("se_", variant2))
  }

  out = paste0(capture.output(print(as.data.frame(show), row.names = FALSE, right = FALSE)), collapse = "\n")

  text = paste0(
    "Examples of mismatches (Coefficients or SEs):\n\n```text\n",
    out,
    "\n```"
  )

  return(list(text = text, note = note))
}



mrb_test_coef_diff_stats = function(diff_tab, cmd = NA_character_, variant2 = "rb") {
  restore.point("mrb_test_coef_diff_stats")

  diff_tab = mrb_test_filter_ignored_intercept_diff(diff_tab, cmd = cmd, variant2 = variant2)

  if (is.null(diff_tab) || NROW(diff_tab) == 0) {
    return(mrb_test_empty_coef_diff_stats())
  }

  coef_missing = xor(is.na(diff_tab$coef_1), is.na(diff_tab$coef_2))
  abs_err_coef = as.numeric(diff_tab$abs_err_coef)
  rel_err_coef = as.numeric(diff_tab$rel_err_coef)

  identical_coef = !coef_missing & !is.na(abs_err_coef) & abs_err_coef == 0
  within_1pc_coef = !coef_missing & !is.na(rel_err_coef) & rel_err_coef <= 0.01

  max_rel_diff_coef = if (all(is.na(rel_err_coef))) NA_real_ else max(rel_err_coef, na.rm = TRUE)

  dev_coef = pmin(abs_err_coef, rel_err_coef)
  max_deviation_coef = if (all(is.na(dev_coef))) NA_real_ else max(dev_coef, na.rm = TRUE)

  list(
    identical_share_coef = mean(identical_coef),
    within_1pc_share_coef = mean(within_1pc_coef),
    max_rel_diff_coef = max_rel_diff_coef,
    max_deviation_coef = max_deviation_coef
  )
}


mrb_test_empty_coef_diff_stats = function() {
  list(
    identical_share_coef = NA_real_,
    within_1pc_share_coef = NA_real_,
    max_rel_diff_coef = NA_real_,
    max_deviation_coef = NA_real_
  )
}


mrb_test_get_regcoef_pair = function(runid, variant1 = "rb", variant2 = "sb", parcels = list()) {
  restore.point("mrb_test_get_regcoef_pair")

  li = list()
  if (!is.null(parcels$regcoef) && NROW(parcels$regcoef) > 0) {
    li[[length(li) + 1]] = parcels$regcoef
  }
  if (!is.null(parcels$regcoef_rb) && NROW(parcels$regcoef_rb) > 0) {
    li[[length(li) + 1]] = parcels$regcoef_rb
  }
  if (!is.null(parcels$regcoef_so) && NROW(parcels$regcoef_so) > 0) {
    li[[length(li) + 1]] = parcels$regcoef_so
  }
  if (length(li) == 0) {
    return(list(co1 = NULL, co2 = NULL))
  }

  all_coef = bind_rows(li)
  all_coef = all_coef[all_coef$runid == runid, , drop = FALSE]

  get_one = function(variant) {
    if (NROW(all_coef) == 0) return(NULL)
    if ("variant" %in% colnames(all_coef)) {
      res = all_coef[all_coef$variant == variant, , drop = FALSE]
      if (NROW(res) > 0) return(res)
    }
    NULL
  }

  list(
    co1 = get_one(variant1),
    co2 = get_one(variant2)
  )
}


mrb_test_reg_r_code = function(project_dir, runid, parcels = list(), prefer = "fixest") {
  restore.point("mrb_test_reg_r_code")

  need = c("reg", "regvar", "regxvar", "reg_cmdpart")
  missing = need[!need %in% names(parcels)]
  if (length(missing) > 0) {
    parcels = repboxDB::repdb_load_parcels(project_dir, missing, parcels = parcels)
  }

  if (is.null(parcels$reg) || is.null(parcels$regvar) || is.null(parcels$reg_cmdpart)) {
    return("# Could not reconstruct R translation because required parcels are missing.")
  }

  reg = parcel_for_runid(parcels$reg, runid)
  regvar = parcel_for_runid(parcels$regvar, runid)
  regxvar = if (!is.null(parcels$regxvar)) parcel_for_runid(parcels$regxvar, runid) else tibble()
  cmdpart = parcel_for_runid(parcels$reg_cmdpart, runid)

  if (NROW(reg) == 0 || NROW(regvar) == 0 || NROW(cmdpart) == 0) {
    return("# Could not reconstruct R translation because required parcels are missing.")
  }

  res = try({
    opts = regtranslate::code_options(add_function = TRUE, add_broom = TRUE)
    code_df = regtranslate::reg_stata_to_r_code(
      reg = reg,
      regvar = regvar,
      regxvar = regxvar,
      cmdpart = cmdpart,
      prefer = prefer,
      opts = opts
    )
    paste0(code_df$code, collapse = "\n")
  }, silent = TRUE)

  if (inherits(res, "try-error")) {
    msg = conditionMessage(attr(res, "condition"))
    return(paste0("# Could not reconstruct R translation: ", msg))
  }

  res
}


mrb_test_fmt_num = function(x, digits = 4) {
  if (length(x) == 0 || is.na(x)) return("NA")
  formatC(as.numeric(x), digits = digits, format = "fg", flag = "#")
}
````

!END_MODIFICATION mrb_test_coef.R

!MODIFICATION mrb_test_generate_flags in mrb_test.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_test.R"
function_name = "mrb_test_generate_flags"
description = "Recompute report-side diff types directly from coefficient tables, classify coef vs SE problems separately, and attach severity and problem-combination labels for overview and example ranking."
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
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

  eval_diff_tab = function(diff_tab) {
    if (is.null(diff_tab) || NROW(diff_tab) == 0) {
      return(list(
        all_diff = FALSE,
        coef_diff = FALSE,
        se_diff = FALSE,
        all_max_dev = NA_real_,
        all_max_rel = NA_real_,
        coef_max_dev = NA_real_,
        coef_max_rel = NA_real_,
        se_max_dev = NA_real_,
        se_max_rel = NA_real_
      ))
    }

    coef_missing_one = xor(is.na(diff_tab$coef_1), is.na(diff_tab$coef_2))
    se_missing_one = xor(is.na(diff_tab$se_1), is.na(diff_tab$se_2))

    coef_diff_row =
      coef_missing_one |
      (!is.na(diff_tab$abs_err_coef) & diff_tab$abs_err_coef > max_deviation_tol) |
      (!is.na(diff_tab$rel_err_coef) & diff_tab$rel_err_coef > max_rel_diff_tol)

    se_diff_row =
      se_missing_one |
      (!is.na(diff_tab$abs_err_se) & diff_tab$abs_err_se > max_deviation_tol) |
      (!is.na(diff_tab$rel_err_se) & diff_tab$rel_err_se > max_rel_diff_tol)

    coef_dev = pmin(as.numeric(diff_tab$abs_err_coef), as.numeric(diff_tab$rel_err_coef))
    se_dev = pmin(as.numeric(diff_tab$abs_err_se), as.numeric(diff_tab$rel_err_se))

    list(
      all_diff = any(coef_diff_row | se_diff_row, na.rm = TRUE),
      coef_diff = any(coef_diff_row, na.rm = TRUE),
      se_diff = any(se_diff_row, na.rm = TRUE),
      all_max_dev = max_empty_na(c(coef_dev, se_dev), na.rm = TRUE),
      all_max_rel = max_empty_na(c(as.numeric(diff_tab$rel_err_coef), as.numeric(diff_tab$rel_err_se)), na.rm = TRUE),
      coef_max_dev = max_empty_na(coef_dev, na.rm = TRUE),
      coef_max_rel = max_empty_na(as.numeric(diff_tab$rel_err_coef), na.rm = TRUE),
      se_max_dev = max_empty_na(se_dev, na.rm = TRUE),
      se_max_rel = max_empty_na(as.numeric(diff_tab$rel_err_se), na.rm = TRUE)
    )
  }

  pair_diff = function(runid, variant1, variant2) {
    pair = mrb_test_get_regcoef_pair(runid = runid, variant1 = variant1, variant2 = variant2, parcels = parcels)
    if (is.null(pair$co1) || is.null(pair$co2) || NROW(pair$co1) == 0 || NROW(pair$co2) == 0) {
      return(eval_diff_tab(NULL))
    }

    diff_tab = coef_diff_table(pair$co1, pair$co2)
    diff_tab = mrb_test_filter_ignored_intercept_diff(diff_tab, cmd = get_run_cmd(runid), variant2 = variant2)
    eval_diff_tab(diff_tab)
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
```

!END_MODIFICATION mrb_test_generate_flags in mrb_test.R

!MODIFICATION mrb_test_report in mrb_test.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_test.R"
function_name = "mrb_test_report"
description = "Add a grouped problem overview with runids, rank shown examples by severity, and pass the run command into diff rendering so ignored intercept mismatches are not reported."
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

````r
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
````

!END_MODIFICATION mrb_test_report in mrb_test.R

The limit for shown examples is `max_cases` in `mrb_test_opts()`, and the updated report now uses it after sorting by severity.
