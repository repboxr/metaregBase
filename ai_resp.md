You are completely correct that comparing the intercept for fixed effects models is usually not helpful, as it is a nuisance parameter that is handled and reported differently across packages (e.g., Stata’s `areg`/`xtreg` often report a constant, while R’s `fixest::feols` absorbs and omits it entirely).

In fact, the exact helper function you suggested **already exists** in `metaregBase/R/mrb_cmd_types.R`:

```r
mrb_cmds_ignore_intercept_in_r = function() {
  c("areg", "xtreg", "reghdfe", "ivreghdfe", "xtlogit", "xtprobit", "xtpoisson", "clogit", "xtivreg", "xtivreg2")
}
```

The reason you are still seeing intercept differences in your test reports is due to a small bug in how this helper is conditionally applied in `coef_diff_table` and `mrb_test_filter_ignored_intercept_diff`. 

Currently, the code only ignores the intercept if the comparison specifically detects that the second variant is `"rb"` (`v2 == "rb"`). However, when parcels are saved to and loaded from the database, the `variant` column is sometimes stripped to match the database schema (making `v2` default to `"unknown"`). This bypasses the intercept-ignoring logic completely, causing the difference to be falsely flagged.

To fix this, we should remove the strict `v2 == "rb"` requirement. Dropping the intercept comparison universally for these commands is sensible and robust, even when comparing Stata Base (`sb`) to Stata Original (`so`), since intercept estimates can vary slightly across Stata routines anyway.

Here are the code modifications to correctly enforce this logic across all comparisons.

!MODIFICATION coef_diff_table metaregBase/R/mrb_regcoef.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_regcoef.R"
function_name = "coef_diff_table"
description = "Remove strict v2 == 'rb' requirement to ensure intercepts are ignored for FE models even if the variant column is stripped."
---
```r
coef_diff_table = function(
  co1,
  co2,
  check.ref.levels = TRUE,
  eq_mode = c("auto", "exact")[1],
  cmd = NULL,
  ignore_intercept_cmds = mrb_cmds_ignore_intercept_in_r()
) {
  restore.point("regcoef_check_same")

  if (is.null(co1) | is.null(co2)) return(NULL)

  v1 = if ("variant" %in% names(co1)) co1$variant[1] else "unknown"
  v2 = if ("variant" %in% names(co2)) co2$variant[1] else "unknown"

  prep = regcoef_prepare_eq_for_diff(co1, co2, eq_mode = eq_mode)
  co1 = prep$co1
  co2 = prep$co2

  # Match results
  cod = full_join(co1, co2, by = c("eq", "cterm", "runid"), suffix = c("_1", "_2"))

  # Ignore (Intercept) if translating to R natively absorbs it for these commands.
  # In saved regcoef parcels the cmd column is usually not present, so callers can
  # pass cmd explicitly. This is needed for reghdfe, areg, xtreg, etc.
  if (!is.null(ignore_intercept_cmds) && NROW(cod) > 0) {
    cmd_for_ignore = rep(NA_character_, NROW(cod))

    if (!is.null(cmd)) {
      cmd_chr = as.character(cmd)

      if (!is.null(names(cmd_chr)) && "runid" %in% names(cod)) {
        ind = match(as.character(cod$runid), names(cmd_chr))
        cmd_for_ignore = cmd_chr[ind]
      } else if (length(cmd_chr) == 1) {
        cmd_for_ignore = rep(cmd_chr, NROW(cod))
      } else if (length(cmd_chr) == NROW(cod)) {
        cmd_for_ignore = cmd_chr
      }
    }

    if (all(is.na(cmd_for_ignore))) {
      cmd_col = if ("cmd_1" %in% names(cod)) {
        "cmd_1"
      } else if ("cmd" %in% names(cod)) {
        "cmd"
      } else {
        NULL
      }

      if (!is.null(cmd_col)) {
        cmd_for_ignore = as.character(cod[[cmd_col]])
      }
    }

    cod$.repbox_cmd_for_ignore = cmd_for_ignore

    cod = cod %>%
      filter(
        !(
          cterm == "(Intercept)" &
            !is.na(.data$.repbox_cmd_for_ignore) &
            nzchar(.data$.repbox_cmd_for_ignore) &
            .data$.repbox_cmd_for_ignore %in% ignore_intercept_cmds
        )
      ) %>%
      select(-.repbox_cmd_for_ignore)
  }

  # Ignore coefficients that are missing in both co1 and co2
  cod = cod %>%
    filter(!(is.na(coef_1) & is.na(coef_2)))

  # Should be TRUE whenever co1 and co2 come from different regression commands
  # We try to correct for the fact that they may pick different reference levels
  # when creating the dummy variables
  if (check.ref.levels) {
    cod = cod %>%
      mutate(
        is_ia = has.substr(cterm, "#"),
        is_factor = has.substr(cterm, "="),
        factor_group = stringi::stri_replace_all_regex(paste0(cterm, ":"), "=([^\\:]*):", ":") %>% str.remove.ends(right = 1)
      ) %>%
      group_by(runid, eq, factor_group) %>%
      mutate(
        ref_level_differs = is_factor & any(is.na(coef_2)),
        offset.2 = ifelse(ref_level_differs, -coef_1[first(which(is.na(coef_2)))], 0),
        num_diff_ref_coef_2 = sum(is.na(coef_2))
      ) %>%
      ungroup() %>%
      mutate(
        coef_2 = ifelse(is.na(coef_2) & ref_level_differs, 0, coef_2),
        coef_2 = ifelse(ref_level_differs, coef_2 + offset.2, coef_2)
      )

    # Adapt (Intercept) if there are different reference levels
    cod = cod %>%
      group_by(runid, eq) %>%
      mutate(
        ref_level_differs = ifelse(cterm == "(Intercept)" & any(ref_level_differs), any(ref_level_differs, na.rm = TRUE), ref_level_differs),
        offset.2.intercept = ifelse(cterm == "(Intercept)" & any(ref_level_differs), -sum(unique(offset.2), na.rm = TRUE), offset.2),
        coef_2 = ifelse(cterm == "(Intercept)" & any(ref_level_differs), coef_2 + offset.2.intercept, coef_2)
      )
  } else {
    cod$ref_level_differs = rep(FALSE, NROW(cod))
  }

  # Compute absolute and relative differences between coefficients and se
  cod = cod %>%
    mutate(
      abs_err_coef = abs(coef_1 - coef_2),
      abs_err_se = abs(se_1 - se_2),
      rel_err_coef = abs_err_coef / (0.5 * (abs(coef_1) + abs(coef_2))),
      rel_err_se = abs_err_se / (0.5 * (abs(se_1) + abs(se_2))),

      rel_within_1pc_coef = rel_err_coef < 0.01,
      rel_within_1pc = rel_err_coef < 0.01 & rel_err_se < 0.01,
      identical_coef = coef_1 == coef_2,
      identical = identical_coef & se_1 == se_2
    )

  cod = cod %>%
    group_by(runid, eq) %>%
    mutate(
      step_refs_differ =
        any(ref_level_differs) |
        any(!is.na(coef_1) & is.na(coef_2))
    ) %>%
    ungroup()

  cod = cod %>%
    select(runid, eq, cterm, identical, identical_coef, everything())

  cod
}
```
!END_MODIFICATION coef_diff_table metaregBase/R/mrb_regcoef.R

!MODIFICATION mrb_test_filter_ignored_intercept_diff metaregBase/R/mrb_test_coef.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_test_coef.R"
function_name = "mrb_test_filter_ignored_intercept_diff"
description = "Remove variant2 == 'rb' requirement so that intercepts in FE models are consistently ignored in all comparisons."
---
```r
mrb_test_filter_ignored_intercept_diff = function(diff_tab, cmd = NA_character_, variant2 = "rb", ignore_intercept_cmds = mrb_cmds_ignore_intercept_in_r()) {
  restore.point("mrb_test_filter_ignored_intercept_diff")

  if (is.null(diff_tab) || NROW(diff_tab) == 0) {
    return(diff_tab)
  }

  cmd = as.character(cmd)[1]

  if (is.na(cmd) || !nzchar(cmd) || !cmd %in% ignore_intercept_cmds) {
    return(diff_tab)
  }

  diff_tab %>%
    filter(cterm != "(Intercept)")
}
```
!END_MODIFICATION mrb_test_filter_ignored_intercept_diff metaregBase/R/mrb_test_coef.R
