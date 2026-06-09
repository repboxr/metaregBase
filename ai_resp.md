You spotted the discrepancy perfectly. 

### Why did R drop 598 instead of 599 observations?
Stata's "predicts success/failure perfectly" heuristic only checks **dummy/binary variables**.
Because my previous code checked *all* variables in the formula, it evaluated the continuous variables (`apt_size_h`, `apt_price_t`) as well. It's extremely likely that one of those continuous variables had exactly one observation equal to `0`. Since a sample size of 1 always has `min(y) == max(y)` trivially, my previous algorithm improperly dropped that 1 single observation, resulting in 598 instead of 599!

### The Fix
We need to restrict the `x == 0` vs `x != 0` separation check exclusively to variables that only take a maximum of 2 distinct values in the dataset. This perfectly mimics Stata's pre-estimation behavior for indicators. 

I've also added the `verbose = TRUE` logging to print out the exact notes Stata generates.

Here are the modifications to fix the algorithm and explicitly inject `verbose = TRUE` into the R translation.

!MODIFICATION drop_perfect_predictors.R
scope = "file"
file = "/home/rstudio/repbox/regtranslate/R/drop_perfect_predictors.R"
description = "Fix Stata perfect predictor dropping logic to only apply to binary variables, preventing erroneous continuous singleton drops. Add verbose logging to match Stata notes."
---
```r
#' Drop perfect predictors for binary outcome models
#'
#' Emulates Stata's behavior of dropping variables and observations
#' that perfectly predict the outcome in logit/probit models.
#'
#' @param dat data.frame
#' @param depvar character, name of dependent variable
#' @param indepvars character vector of independent variables
#' @param verbose logical, if TRUE prints Stata-like notes
#' @return A list with `dat` (filtered data) and `indepvars` (variables kept).
#' @export
stata_drop_perfect_predictors = function(dat, depvar, indepvars, verbose = TRUE) {
  if (length(depvar) != 1 || !depvar %in% colnames(dat)) return(list(dat=dat, indepvars=indepvars))
  
  y = dat[[depvar]]
  if (is.logical(y)) y = as.integer(y)
  y = as.numeric(y)
  # Stata treats exactly 0 as failure, and non-zero as success
  y = ifelse(y == 0, 0, 1)
  
  uni_y = unique(na.omit(y))
  if (length(uni_y) < 2) {
    return(list(dat=dat, indepvars=indepvars))
  }
  
  kept_vars = indepvars
  keep_rows = !is.na(y)
  
  changed = TRUE
  
  while(changed) {
    changed = FALSE
    cur_y = y[keep_rows]
    
    for (var in kept_vars) {
      if (!var %in% colnames(dat)) next
      x = dat[[var]]
      
      if (is.character(x) || is.factor(x)) next
      
      cur_x = x[keep_rows]
      if (all(is.na(cur_x))) next
      
      # CRITICAL FIX: Stata only applies the "!= 0" heuristic to dummy variables.
      # If we don't check this, continuous variables with a single 0 observation
      # will falsely appear as perfect predictors and drop the observation.
      uni_x = unique(na.omit(cur_x))
      if (length(uni_x) > 2) next
      
      idx_neq_0 = which(!is.na(cur_x) & cur_x != 0)
      idx_eq_0 = which(!is.na(cur_x) & cur_x == 0)
      
      if (length(idx_neq_0) > 0 && length(idx_eq_0) > 0) {
        
        y_neq = cur_y[idx_neq_0]
        if (min(y_neq) == max(y_neq)) {
          if (verbose) {
             cat(sprintf("note: %s != 0 predicts %s perfectly;\n      %s omitted and %d obs not used.\n", 
                         var, ifelse(y_neq[1]==1, "success", "failure"), var, length(idx_neq_0)))
          }
          keep_rows[which(keep_rows)[idx_neq_0]] = FALSE
          kept_vars = setdiff(kept_vars, var)
          changed = TRUE
          break
        }
        
        y_eq = cur_y[idx_eq_0]
        if (min(y_eq) == max(y_eq)) {
          if (verbose) {
             cat(sprintf("note: %s == 0 predicts %s perfectly;\n      %s omitted and %d obs not used.\n", 
                         var, ifelse(y_eq[1]==1, "success", "failure"), var, length(idx_eq_0)))
          }
          keep_rows[which(keep_rows)[idx_eq_0]] = FALSE
          kept_vars = setdiff(kept_vars, var)
          changed = TRUE
          break
        }
      }
    }
  }
  
  if (!all(keep_rows)) {
    dat = dat[keep_rows, , drop=FALSE]
  }
  
  list(dat = dat, indepvars = kept_vars)
}
```
!END_MODIFICATION drop_perfect_predictors.R

!MODIFICATION stata_to_r_code_fixest to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "stata_to_r_code_fixest"
description = "Update drop perfect predictors call to explicitly pass verbose=TRUE."
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

!MODIFICATION stata_to_r_code_mfx to_r_mfx.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_mfx.R"
function_name = "stata_to_r_code_mfx"
description = "Update drop perfect predictors call to explicitly pass verbose=TRUE in mfx translation."
---
```r
stata_to_r_code_mfx = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_mfx")

  # Ignore dropped regvars (if they are nor part of an interaction)
  #regvar = filter(regvar, !is_dropped | ia_cterm != cterm)

  # Currently we just use the fixest formula
  formula = regvar_to_formula_fixest(regvar,regxvar, cmdpart, reg = reg)

  cmd = reg$cmd
  if (cmd=="dprobit") {
    rcmd = "probitmfx"
  } else {
    stop("Cannot yet translate Stata command ", cmd)
  }

  # The exclude='select' arguments avoids overwriting
  # of dplyr's select function
  library_code = "library(MASS, exclude='select')\nlibrary(mfx)\n  "
  rcmd_code = paste0('rcmd = "',rcmd,'"')
  # We use the default ssc arguments since they are closest to the
  # Stata defaults
  formula_code = paste0('formula = ', formula)

  data_code = r_listwise_deletion_code(regvar)
  
  is_binary = reg$cmd %in% c("logit", "xtlogit", "probit", "xtprobit", "dprobit", "clogit", "logistic", "exlogistic")
  if (is_binary && isTRUE(opts$drop_perfect_predictors)) {
    mod_depvars = regvar$cterm[regvar$role=="dep"]
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

  # mfx
  arg_str = NULL
  if (reg$se_category == "robust") {
    arg_str = "robust = true"
  } else if (reg$se_category == "cluster") {
    clustervar = extract_clustervar_from_se_args(reg$se_args)
    arg_str = paste0('clustervar1 = "', clustervar[1],'"')
    if (reg$se_type == "twoway") {
      arg_str = c(arg_str, paste0('clustervar2 = "', clustervar[2],'"'))
    }
  }
  arg_str = c(
    paste0("formula = formula"),
    paste0('data = dat'),
    arg_str
  )

  reg_code = paste0('reg = ', rcmd,'(', paste0(arg_str, collapse=","),")")
  code_df = tibble(part = c("library", "rcmd","data","formula","reg"), code = c(library_code, rcmd_code,data_code,formula_code,reg_code))
  code_df = code_df[code_df$code != "", ]

  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=FALSE, use_conf_int=TRUE)
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}
```
!END_MODIFICATION stata_to_r_code_mfx to_r_mfx.R
