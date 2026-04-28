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


mrb_test_annotate_diff_tab = function(
  diff_tab,
  cmd = NA_character_,
  variant2 = "rb",
  max_rel_diff_tol = 0.01,
  max_deviation_tol = 1e-6
) {
  restore.point("mrb_test_annotate_diff_tab")

  diff_tab = mrb_test_filter_ignored_intercept_diff(diff_tab, cmd = cmd, variant2 = variant2)

  if (is.null(diff_tab) || NROW(diff_tab) == 0) {
    return(tibble())
  }

  diff_tab %>%
    mutate(
      abs_err_coef = as.numeric(abs_err_coef),
      rel_err_coef = as.numeric(rel_err_coef),
      abs_err_se = as.numeric(abs_err_se),
      rel_err_se = as.numeric(rel_err_se),

      coef_missing_one = xor(is.na(coef_1), is.na(coef_2)),
      se_missing_one = xor(is.na(se_1), is.na(se_2)),

      coef_diff_abs = !coef_missing_one & !is.na(abs_err_coef) & abs_err_coef > max_deviation_tol,
      coef_diff_rel = !coef_missing_one & !is.na(rel_err_coef) & rel_err_coef > max_rel_diff_tol,
      se_diff_abs = !se_missing_one & !is.na(abs_err_se) & abs_err_se > max_deviation_tol,
      se_diff_rel = !se_missing_one & !is.na(rel_err_se) & rel_err_se > max_rel_diff_tol,

      is_coef_diff = coef_missing_one | (coef_diff_abs & coef_diff_rel),
      is_se_diff = se_missing_one | (se_diff_abs & se_diff_rel),
      any_diff = is_coef_diff | is_se_diff,

      safe_abs_coef = dplyr::coalesce(abs_err_coef, -Inf),
      safe_rel_coef = dplyr::coalesce(rel_err_coef, -Inf),
      safe_abs_se = dplyr::coalesce(abs_err_se, -Inf),
      safe_rel_se = dplyr::coalesce(rel_err_se, -Inf)
    )
}


mrb_test_eval_diff_tab = function(
  diff_tab,
  cmd = NA_character_,
  variant2 = "rb",
  max_rel_diff_tol = 0.01,
  max_deviation_tol = 1e-6
) {
  restore.point("mrb_test_eval_diff_tab")

  tab = mrb_test_annotate_diff_tab(
    diff_tab = diff_tab,
    cmd = cmd,
    variant2 = variant2,
    max_rel_diff_tol = max_rel_diff_tol,
    max_deviation_tol = max_deviation_tol
  )

  if (NROW(tab) == 0) {
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

  list(
    all_diff = any(tab$any_diff, na.rm = TRUE),
    coef_diff = any(tab$is_coef_diff, na.rm = TRUE),
    se_diff = any(tab$is_se_diff, na.rm = TRUE),

    all_max_dev = max_empty_na(c(tab$abs_err_coef, tab$abs_err_se), na.rm = TRUE),
    all_max_rel = max_empty_na(c(tab$rel_err_coef, tab$rel_err_se), na.rm = TRUE),

    coef_max_dev = max_empty_na(tab$abs_err_coef, na.rm = TRUE),
    coef_max_rel = max_empty_na(tab$rel_err_coef, na.rm = TRUE),

    se_max_dev = max_empty_na(tab$abs_err_se, na.rm = TRUE),
    se_max_rel = max_empty_na(tab$rel_err_se, na.rm = TRUE)
  )
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

  tab = mrb_test_annotate_diff_tab(
    diff_tab = diff_tab,
    cmd = cmd,
    variant2 = variant2,
    max_rel_diff_tol = max_rel_diff_tol,
    max_deviation_tol = max_deviation_tol
  )

  if (NROW(tab) == 0) {
    return(list(text = "- No comparison rows available.", note = ""))
  }

  only_se_wrong = !any(tab$is_coef_diff, na.rm = TRUE) && any(tab$is_se_diff, na.rm = TRUE)
  note = if (only_se_wrong) "Note: All coefficients match within tolerance; only standard errors differ." else ""

  cat_missing = tab %>%
    filter(coef_missing_one | se_missing_one) %>%
    head(2)

  cat_coef = tab %>%
    filter(is_coef_diff) %>%
    arrange(desc(safe_rel_coef), desc(safe_abs_coef)) %>%
    head(2)

  cat_se = tab %>%
    filter(is_se_diff) %>%
    arrange(desc(safe_rel_se), desc(safe_abs_se)) %>%
    head(2)

  show_tab = bind_rows(cat_missing, cat_coef, cat_se)
  if ("eq" %in% colnames(show_tab)) {
    show_tab = distinct(show_tab, eq, cterm, .keep_all = TRUE)
  } else {
    show_tab = distinct(show_tab, cterm, .keep_all = TRUE)
  }

  if (NROW(show_tab) == 0) {
    return(list(text = "- No differing coefficients or standard errors to show.", note = note))
  }

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
    "Examples of mismatches (coefficients and/or SEs):\n\n```text\n",
    out,
    "\n```"
  )

  return(list(text = text, note = note))
}



mrb_test_coef_diff_stats = function(
  diff_tab,
  cmd = NA_character_,
  variant2 = "rb"
) {
  restore.point("mrb_test_coef_diff_stats")

  tab = mrb_test_annotate_diff_tab(
    diff_tab = diff_tab,
    cmd = cmd,
    variant2 = variant2
  )

  if (NROW(tab) == 0) {
    return(mrb_test_empty_coef_diff_stats())
  }

  identical_coef = !tab$coef_missing_one & !is.na(tab$abs_err_coef) & tab$abs_err_coef == 0
  within_1pc_coef = !tab$coef_missing_one & !is.na(tab$rel_err_coef) & tab$rel_err_coef <= 0.01

  list(
    identical_share_coef = mean(identical_coef),
    within_1pc_share_coef = mean(within_1pc_coef),
    max_rel_diff_coef = max_empty_na(tab$rel_err_coef, na.rm = TRUE),
    max_deviation_coef = max_empty_na(tab$abs_err_coef, na.rm = TRUE)
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


mrb_test_reg_r_code = function(project_dir, runid, parcels = list(), prefer = "fixest", add_function = FALSE) {
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
    opts = regtranslate::code_options(add_function = add_function, add_broom = TRUE)
    code_df = regtranslate::reg_stata_to_r_code(
      reg = reg,
      regvar = regvar,
      regxvar = regxvar,
      cmdpart = cmdpart,
      prefer = prefer,
      opts = opts
    )
    code = paste0(code_df$code, collapse = "\n")

    if (!add_function) {
      lines = stringi::stri_split_lines1(code)
      lines = stringi::stri_trim_both(lines)

      if (
        length(lines) >= 2 &&
        startsWith(lines[[1]], "function(") &&
        identical(lines[[length(lines)]], "}")
      ) {
        code = paste0(lines[2:(length(lines) - 1)], collapse = "\n")
      }
    }

    code
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

