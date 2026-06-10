### Discussion: Where should the Stata data cleaning logic live?

You raise an excellent architectural question: *Since Stata's `marksample` (listwise deletion) is a global feature applied to almost every regression, should it be baked into `mrb_get_regression_data`?*

Here is the breakdown:

1. **Yes, it is a global Stata feature.** Almost every Stata estimation command silently drops rows where *any* variable in the regression contains a numeric missing (`.`, `.a`, etc.) or an empty string (`""`), treating them all as missing. Stata also has no concept of `Inf` (division by zero creates a missing `.`), so `Inf` in R must be dropped to match Stata perfectly.
2. **Why it is better to keep the code injection in `regtranslate`:** The primary goal of `regtranslate` is to generate a **standalone, reproducible R script** that is completely transparent to a human reader. If we hide the `""` and `Inf` dropping inside `mrb_get_regression_data`, the exported R script will magically receive clean data. If a user runs that exact script outside the pipeline on raw data, the observation counts will mismatch. Therefore, the translated R code itself *must* explicitly contain the code that drops the missing values.
3. **Why it also makes sense to add it to `mrb_get_regression_data`:** As you noted, it might be highly useful to query the exact `e(sample)` data for a regression without needing to parse or execute the full generated `reg_fun`.

**The Proposal:**
We will do both, gracefully bridging your requests.
1. We will create the highly optimized cleaning function `mrb_stata_drop_missing` in `metaregBase`.
2. We will add an option `drop_missing = FALSE` to `mrb_get_regression_data`. This defaults to `FALSE` to prevent double-dropping during the standard pipeline, but allows you to extract the strict `e(sample)` whenever you want.
3. We will update `regtranslate`'s `r_listwise_deletion_code` to inject a call to `metaregBase::mrb_stata_drop_missing()` instead of R's naive `complete.cases()`.
4. We will apply the `match_stata_singletons = TRUE` fix to `regtranslate` to dynamically handle `fixef.rm = "none"`, preserving singletons exactly as Stata does.

Here are the required modifications:

### 1. Modifications in `metaregBase`

!MODIFICATION mrb_stata_drop_missing mrb_r_reg.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_reg.R"
insert_bottom = true
description = "Add helper to cleanly drop Stata-equivalent missing values"
---
```r
#' Emulate Stata's listwise deletion (e(sample))
#'
#' Drops NA, but also empty strings ("") and non-finite numbers (Inf, NaN),
#' which Stata treats as missing values during marksample.
#' @export
mrb_stata_drop_missing = function(dat, vars) {
  vars = intersect(vars, colnames(dat))
  if (length(vars) == 0) return(dat)

  keep = rep(TRUE, nrow(dat))
  for (v in vars) {
    val = dat[[v]]
    if (is.character(val) || is.factor(val)) {
      char_val = as.character(val)
      keep = keep & !is.na(char_val) & (char_val != "")
    } else if (is.numeric(val)) {
      keep = keep & is.finite(val)
    } else {
      keep = keep & !is.na(val)
    }
  }

  if (!all(keep)) {
    dat = dat[keep, , drop = FALSE]
  }
  dat
}
```
!END_MODIFICATION mrb_stata_drop_missing mrb_r_reg.R

!MODIFICATION mrb_get_regression_data mrb_r_reg.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_reg.R"
function_name = "mrb_get_regression_data"
description = "Add drop_missing toggle to optionally emulate Stata's marksample inside the data fetcher"
---
```r
#' Get and prepare regression data (creates cterms and regxvar columns)
mrb_get_regression_data = function(runid, drf, reg=NULL, regvar, regxvar = NULL, continue_on_error=FALSE, parcels=NULL, drop_missing=FALSE) {
  restore.point("mrb_get_regression_data")

  # Phase 1: Fetch UNFILTERED data to correctly evaluate time-series lags and leads
  dat = repboxDRF::drf_get_data(runid, drf = drf, filtered = FALSE, continue_on_error = continue_on_error)
  if (is.null(dat) || inherits(dat, "try-error")) return(dat)

  # Extract panel/time variables if available
  timevar = NA; panelvar = NA; tdelta = NA
  if (!is.null(reg) && nrow(reg) > 0) {
    timevar = reg$timevar[1]
    panelvar = reg$panelvar[1]
    tdelta = reg$tdelta[1]
  }

  if (!is.null(regvar) && nrow(regvar) > 0) {
    dat = create_cterm_cols(dat, unique(regvar$cterm), timevar = timevar, panelvar = panelvar, tdelta = tdelta)
  }

  # Phase 2: Apply the regression filter AFTER generating the time-series variables
  if (!is.null(parcels)) {
    filter_code = repboxDRF::drf_get_filter_code(runid, drf, parcels = parcels)
    if (length(filter_code) > 0 && any(nzchar(filter_code))) {
      data = dat # The evaluated filter code safely acts on the local variable 'data'
      for (code in filter_code) {
        if (nzchar(code)) {
          eval(parse(text = code))
        }
      }
      dat = data # Map back to 'dat' to continue standard processing
    }
  }

  # Phase 3: Build interactions / dummy variables ONLY on the filtered estimation sample
  if (!is.null(regxvar) && nrow(regxvar) > 0) {
    dat = make_regxvar_cols(dat, regxvar)
  }

  # Phase 4: Optional Listwise deletion of missing values (matching Stata's marksample)
  if (drop_missing && !is.null(regvar) && nrow(regvar) > 0) {
    cc_cols = setdiff(unique(regvar$cterm), c("(Intercept)", ""))
    dat = mrb_stata_drop_missing(dat, cc_cols)
  }

  return(dat)
}
```
!END_MODIFICATION mrb_get_regression_data mrb_r_reg.R

### 2. Modifications in `regtranslate`

!MODIFICATION code_options to_r.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r.R"
function_name = "code_options"
description = "Add match_stata_singletons toggle to control fixest singleton behavior natively"
---
```r
code_options = function(prefer_sandwich=FALSE, prefer_summary=FALSE, add_broom=TRUE, add_function=FALSE, add_restorepoint=FALSE, drop_perfect_predictors=TRUE, match_stata_singletons=TRUE) {
  list(
    prefer_sandwich=prefer_sandwich, 
    prefer_summary=prefer_summary, 
    add_broom=add_broom, 
    add_function=add_function, 
    add_restorepoint=add_restorepoint, 
    drop_perfect_predictors=drop_perfect_predictors,
    match_stata_singletons=match_stata_singletons
  )
}
```
!END_MODIFICATION code_options to_r.R

!MODIFICATION r_listwise_deletion_code to_r.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r.R"
function_name = "r_listwise_deletion_code"
description = "Use metaregBase::mrb_stata_drop_missing in translated R code instead of complete.cases"
---
```r
#' Generate R code to emulate Stata's listwise deletion (e(sample))
#'
#' Stata drops missing values for all variables in the original varlist
#' *before* omitting collinear terms. Since our R translation explicitly
#' drops collinear terms from the formula, we need to manually drop NAs
#' across all original variables to match Stata's e(sample).
r_listwise_deletion_code = function(regvar) {
  all_base_cterms = unique(regvar$cterm)
  all_base_cterms = setdiff(all_base_cterms, c("(Intercept)", ""))

  if (length(all_base_cterms) == 0) return("")

  paste0(
    "cc_cols = c(", paste0('"', all_base_cterms, '"', collapse=", "), ")\n",
    "dat = metaregBase::mrb_stata_drop_missing(dat, cc_cols)"
  )
}
```
!END_MODIFICATION r_listwise_deletion_code to_r.R

!MODIFICATION stata_to_r_code_fixest to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "stata_to_r_code_fixest"
description = "Inject fixef.rm = 'none' for Stata commands that retain singletons (areg, xtreg) or reghdfe with keepsingletons"
---
```r
stata_to_r_code_fixest = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_fixest")

  org_depvars = regvar$cterm[regvar$role=="dep"]
  mod_depvars = replace_cterm_special_symbols(org_depvars)

  formula = regvar_to_formula_fixest(regvar, regxvar, cmdpart, reg = reg)

  vcov_type = fixest_vcov_type_from_regdb(reg$se_type, reg$se_args)
  ssc_expr = fixest_ssc_code_from_reg(reg, vcov_type = vcov_type)
  use_ssc = !is.null(ssc_expr)

  use_sandwich = (vcov_type == "sandwich") | opts$prefer_sandwich
  use_summary = use_sandwich | opts$prefer_summary

  if (use_sandwich) {
    reg_vcov = "iid"
    vcov = regdb_se_to_sandwich(reg$se_category, reg$se_type, reg$se_args)
  } else {
    reg_vcov = fixest_vcov_code_from_regdb(reg$se_type, reg$se_args, vcov_type, quote=FALSE, reg=reg)
    if (use_summary) {
      vcov = reg_vcov
    }
  }

  command = "feols"
  arg_str = NULL
  if (reg$cmd %in% c("ppmlhdfe", "poisson", "xtpoisson")) {
    command = "fepois"
  } else if (reg$cmd %in% c("nbreg", "gnbreg")) {
    command = "fenegbin"
  } else if (reg$cmd %in% c("logit","xtlogit", "clogit")) {
    command = "feglm"
    arg_str = "family=binomial()"
  } else if (reg$cmd %in% c("probit","xtprobit","dprobit")) {
    command = "feglm"
    arg_str = 'family=binomial(link = "probit")'
  }

  arg_str = c(
    paste0("fml = formula"),
    paste0("data = dat"),
    paste0("vcov = reg_vcov"),
    arg_str
  )

  # Pass ssc to fixest natively when relevant.
  if (use_ssc) {
    arg_str = c(arg_str, "ssc = ssc")
  }

  # Handle singleton observations according to Stata behavior
  if (isTRUE(opts$match_stata_singletons)) {
    if (reg$cmd %in% c("areg", "xtreg", "xtivreg", "xtivreg2", "xtpoisson", "xtlogit", "xtprobit", "clogit")) {
      arg_str = c(arg_str, 'fixef.rm = "none"')
    } else if (reg$cmd %in% c("reghdfe", "ivreghdfe", "ppmlhdfe")) {
      # reghdfe/ppmlhdfe drops them by default, but allows keepsingletons option
      keep_singletons = FALSE
      if (!is.null(cmdpart)) {
        keep_singletons = any(cmdpart$part == "opt" & startsWith(tolower(cmdpart$content), "keepsingleton"))
      }
      if (keep_singletons) {
        arg_str = c(arg_str, 'fixef.rm = "none"')
      }
    }
  }

  library_code = "library(fixest)"
  rcmd_code = paste0('rcmd = "',command,'"')
  if (all(org_depvars==mod_depvars)) {
    data_code = ""
  } else {
    data_code = paste0(
      'dat[["', mod_depvars,'"]] = dat[["', org_depvars,'"]]',
      collapse="\n"
    )
  }

  # Apply explicit listwise deletion to emulate Stata's e(sample)
  lw_code = r_listwise_deletion_code(regvar)
  if (nzchar(lw_code)) {
    data_code = if (nzchar(data_code)) paste0(data_code, "\n", lw_code) else lw_code
  }

  is_binary = reg$cmd %in% c("logit", "xtlogit", "probit", "xtprobit", "dprobit", "clogit", "logistic", "exlogistic", "blogit", "glogit", "binreg")

  if (is_binary && isTRUE(opts$drop_perfect_predictors)) {
    # Check all possible expanded predictors before filtering omitted formulas
    pred_cols = unique(regxvar$cterm)
    pred_cols = setdiff(pred_cols, c("(Intercept)", ""))
    if (length(pred_cols) > 0) {
      pred_str = paste0('c(', paste0('"', pred_cols, '"', collapse=", "), ')')
      dp_code = paste0(
        'dp_cols = intersect(', pred_str, ', colnames(dat))\n',
        'dp_res = regtranslate::stata_drop_perfect_predictors(dat, "', mod_depvars[1], '", dp_cols, verbose = TRUE)\n',
        'dat = dp_res$dat'
      )
      data_code = paste0(data_code, "\n", dp_code)
    }
  }

  # Apply dynamic weights via centralized helper
  wt = r_weight_code(reg, template = "~ `%s`")
  if (nzchar(wt$data_code)) {
    data_code = if (nzchar(data_code)) paste0(data_code, "\n", wt$data_code) else wt$data_code
  }
  if (nzchar(wt$weight_arg)) {
    arg_str = c(arg_str, wt$weight_arg)
  }

  ssc_code = if (use_ssc) paste0("ssc = ", ssc_expr) else NULL
  formula_code = paste0("formula = ", formula)
  reg_vcov_code = paste0("reg_vcov = ", quote_arg(reg_vcov))
  reg_code = paste0("reg = ", command, "(", paste0(arg_str, collapse=","), ")")

  code_df = tibble(
    part = c("library", "rcmd", "data", "formula", if (use_ssc) "ssc", "reg_vcov", "reg"),
    code = c(library_code, rcmd_code, data_code, formula_code, if (use_ssc) ssc_code, reg_vcov_code, reg_code)
  )

  if (use_summary) {
    sum_vcov_code = paste0("sum_vcov = ", quote_arg(vcov))
    sum_code = "sum = summary(reg, vcov = sum_vcov)"
    code_df = bind_rows(
      code_df,
      tibble(part = c("sum_vcov","sum"), code = c(sum_vcov_code, sum_code))
    )
  }
  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=use_summary, use_conf_int=TRUE)
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}
```
!END_MODIFICATION stata_to_r_code_fixest to_r_fixest.R
