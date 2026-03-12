mrb_test_regcoeff_diff = function(
  project_dir,
  parcels = list(),
  max_rel_diff_tol = 0.01,
  max_deviation_tol = 1e-6,
  min_within_share = 0.99,
  max_cases = Inf
) {
  restore.point("mrb_test_regcoeff_diff")

  parcels = repdb_load_parcels(
    project_dir,
    c("regcoef_diff", "reg", "reg_cmdpart", "regvar", "regxvar", "regsource", "regcoef", "regcoef_rb"),
    parcels = parcels
  )

  diff = parcels$regcoef_diff

  num_all_reg = n_distinct(diff$runid)

  if (is.null(diff) || NROW(diff) == 0) return("\n regcoef_diff parcel contains no regressions, yet.")

  # Find problematic runs using overall ("all") deviations
  diff = diff %>%
    filter(compare_what == "all") %>%
    mutate(
      within_1pc_share = as.numeric(within_1pc_share),
      max_rel_diff = as.numeric(max_rel_diff),
      max_deviation = as.numeric(max_deviation),
      identical = !is.na(identical) & identical,
      is_problem =
        !identical &
        (
          is.na(within_1pc_share) |
          within_1pc_share < min_within_share |
          (!is.na(max_rel_diff) & max_rel_diff > max_rel_diff_tol) |
          (!is.na(max_deviation) & max_deviation > max_deviation_tol)
        )
    ) %>%
    filter(is_problem)
  #%>%
    #arrange(desc(max_rel_diff), desc(max_deviation), runid)

  if (NROW(diff) == 0) return("\n-- In all regressions R and Stata coefficients match. --")

  num_all_prob = n_distinct(diff$runid)

  if (is.finite(max_cases) && NROW(diff) > max_cases) {
    diff = diff[seq_len(max_cases), , drop = FALSE]
  }

  reg = parcels$reg

  txt = lapply(seq_len(NROW(diff)), function(i) {
    row = diff[i, ]
    runid = row$runid

    reg_row = NULL
    if (!is.null(reg) && NROW(reg) > 0) {
      reg_row = reg[reg$runid == runid & reg$variant == row$variant2, , drop = FALSE]
      if (NROW(reg_row) == 0) {
        reg_row = reg[reg$runid == runid, , drop = FALSE]
      }
    }

    stata_code = ""
    if (!is.null(reg_row) && NROW(reg_row) > 0 && "cmdline" %in% colnames(reg_row) && !is.na(reg_row$cmdline[1])) {
      stata_code = reg_row$cmdline[1]
    }

    r_code = mrb_test_reg_r_code(project_dir, runid, parcels = parcels)

    coef_pair = mrb_test_get_regcoef_pair(
      runid = runid,
      variant1 = row$variant1,
      variant2 = row$variant2,
      parcels = parcels
    )

    diff_res = list(text = "- Could not create coefficient comparison table.", note = "")

    if (!is.null(coef_pair$co1) && !is.null(coef_pair$co2) &&
        NROW(coef_pair$co1) > 0 && NROW(coef_pair$co2) > 0) {
      diff_tab = coef_diff_table(coef_pair$co1, coef_pair$co2)
      diff_res = mrb_test_regcoef_diff_text(
        diff_tab = diff_tab,
        variant1 = row$variant1,
        variant2 = row$variant2,
        max_rel_diff_tol = max_rel_diff_tol,
        max_deviation_tol = max_deviation_tol
      )
    }

    header = paste0("## runid ", runid)

    block = c(
      header,
      "",
      if (nzchar(diff_res$note)) c(diff_res$note, "") else NULL,
      diff_res$text,
      "",
      "Stata command:",
      "```stata",
      stata_code,
      "```",
      "",
      "R translation used by metaregBase:",
      "```r",
      r_code,
      "```"
    )

    paste0(block, collapse = "\n")
  })

  head = paste0("In ", num_all_prob, " of ", num_all_reg, " regressions the R and Stata coefficients don't match. ", NROW(diff), " problematic cases are shown below.")

  paste0(c(head,txt), collapse = "\n\n")
}


mrb_test_regcoef_diff_text = function(
  diff_tab,
  variant1 = "rb",
  variant2 = "sb",
  max_rel_diff_tol = 0.01,
  max_deviation_tol = 1e-6
) {
  restore.point("mrb_test_regcoef_diff_text")

  if (is.null(diff_tab) || NROW(diff_tab) == 0) {
    return(list(text = "- No coefficient comparison rows available.", note = ""))
  }

  # Add safety layers and deviation checks to all rows
  tab = diff_tab %>%
    mutate(
      coef_missing_one = xor(is.na(coef_1), is.na(coef_2)),
      se_missing_one = xor(is.na(se_1), is.na(se_2)),

      # Safe numeric extraction substituting NAs with -Inf for pure sorting purposes
      safe_abs_coef = ifelse(is.na(abs_err_coef), -Inf, abs_err_coef),
      safe_rel_coef = ifelse(is.na(rel_err_coef), -Inf, rel_err_coef),
      safe_abs_se   = ifelse(is.na(abs_err_se), -Inf, abs_err_se),
      safe_rel_se   = ifelse(is.na(rel_err_se), -Inf, rel_err_se),

      is_coef_diff = !coef_missing_one & (safe_abs_coef > max_deviation_tol | safe_rel_coef > max_rel_diff_tol),
      is_se_diff   = !se_missing_one & (safe_abs_se > max_deviation_tol | safe_rel_se > max_rel_diff_tol)
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
  max_rel_se   = suppressWarnings(max(tab$safe_rel_se, na.rm = TRUE))

  cat_se = tibble()
  if (max_rel_se > max_rel_coef && max_rel_se > max_rel_diff_tol) {
    cat_se = tab %>%
      filter(is_se_diff & !coef_missing_one) %>%
      arrange(desc(safe_rel_se), desc(safe_abs_se)) %>%
      head(2)
  }

  # Combine categories and remove possible overlaps (e.g. if a row qualified for multiple)
  show_tab = bind_rows(cat_missing, cat_coef, cat_se) %>% distinct(cterm, .keep_all = TRUE)

  if (NROW(show_tab) == 0) {
    return(list(text = "- No differing coefficients to show.", note = note))
  }

  # Assemble the exact layout format
  show = tibble(
    cterm = show_tab$cterm,
    coef_1 = show_tab$coef_1,
    coef_2 = show_tab$coef_2,
    se_1 = show_tab$se_1,
    se_2 = show_tab$se_2
  )
  names(show)[2:3] = c(paste0("coef_", variant1), paste0("coef_", variant2))
  names(show)[4:5] = c(paste0("se_", variant1), paste0("se_", variant2))

  out = paste0(capture.output(print(as.data.frame(show), row.names = FALSE, right = FALSE)), collapse = "\n")

  text = paste0(
    "Examples of coefficient mismatches:\n\n```text\n",
    out,
    "\n```"
  )

  return(list(text = text, note = note))
}


mrb_test_coef_diff_stats = function(diff_tab) {
  restore.point("mrb_test_coef_diff_stats")

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
    parcels = repdb_load_parcels(project_dir, missing, parcels = parcels)
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
