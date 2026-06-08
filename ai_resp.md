### Diagnosis of the Discrepancy

Your suspicion is completely correct! The discrepancy arises from a fundamental difference in how Stata constructs the estimation sample (`e(sample)`) compared to how `feols` (or `lm`) drops missing values via formulas.

**What happens in Stata:**
1. Stata identifies *all* variables requested in the original model (`logdiff`, `myavailratio`, and `older15`).
2. It performs **listwise deletion** on the data, dropping any row that contains an `NA` (missing) for *any* of those variables.
3. *After* the estimation sample is locked in, Stata evaluates the variables for collinearity and variance. If `year == 2012` restricted the data such that the remaining non-missing `older15` values are all `0`, Stata quietly omits `1.older15` from the estimation output. However, the rows where `older15` was `NA` remain dropped.

**What happens in our R Translation Pipeline:**
1. During `mrb_run_r_base`, we peek at Stata's results to see exactly which factor dummies were kept (to ensure reference groups match perfectly). 
2. We see that `1.older15` was omitted by Stata, so we set `in_regcoef = FALSE` for it in `regxvar`.
3. Our `regvar_to_formula_fixest()` function constructs the `feols` formula and deliberately leaves out `1.older15` because it was flagged as omitted. The R formula becomes simply `logdiff ~ myavailratio`.
4. R's `feols` evaluates `logdiff ~ myavailratio`. Because `older15` is nowhere to be found in the formula, `feols` has absolutely no reason to drop rows where `older15` is `NA`. Thus, R runs the regression on more observations than Stata did, leading to mismatched coefficients and standard errors.

---

### Can this behavior be replicated?

**Yes, very easily.** The pipeline already possesses all the necessary metadata to resolve this. 

The `regvar` parcel contains the canonical representation of all the original variables called in the command (the *base* terms), regardless of whether they were expanded into dummies or later dropped. 

We can replicate Stata's behavior precisely by generating a snippet of R code that explicitly performs listwise deletion (e.g., via `complete.cases()`) on all original `cterm`s found in `regvar` *before* the regression is executed.

### Pros and Cons of doing it

**Pros:**
* **Exact Replication:** By exactly mirroring Stata's `e(sample)`, we guarantee that $N$, coefficients, and standard errors align perfectly.
* **Pipeline Robustness:** It eliminates a major class of silent sample mismatches that cause downstream failures in `regcheck`.
* **Zero Configuration:** We don't need to guess missingness logic; we just use the original base variables from the parsed Stata command.

**Cons:**
* **Slight code bloat:** The translated R scripts will contain an extra step to explicitly filter `NAs` (e.g., `dat = dat[complete.cases(...) , ]`). 
* **Less "idiomatic" R:** A standard R user relies on the formula to automatically handle `na.action`. Explicitly filtering the data before regression feels slightly unnatural for human-written R code, but for a high-fidelity translation pipeline, this exactness is a necessary trade-off.

---

### Implementation

I have provided modifications to `regtranslate` below. We add a small helper `r_listwise_deletion_code()` to `to_r.R`, and then we inject its output into the `data_code` section of every translator (`to_r_fixest.R`, `to_r_lm.R`, etc.).

!MODIFICATION r_listwise_deletion_code in /home/rstudio/repbox/regtranslate/R/to_r.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r.R"
insert_bottom = true
description = "Helper function to generate R code for Stata-like listwise deletion based on all original regression variables."
---
```R

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
    "cc_cols = intersect(cc_cols, colnames(dat))\n",
    "dat = dat[complete.cases(dat[, cc_cols, drop=FALSE]), ]"
  )
}
```
!END_MODIFICATION r_listwise_deletion_code in /home/rstudio/repbox/regtranslate/R/to_r.R


!MODIFICATION stata_to_r_code_fixest in /home/rstudio/repbox/regtranslate/R/to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "stata_to_r_code_fixest"
description = "Add explicit listwise deletion to emulate Stata's e(sample) matching."
---
```R
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
  if (reg$cmd == "ppmlhdfe") {
    command = "fepos"
  } else if (reg$cmd %in% c("logit","xtlogit")) {
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
!END_MODIFICATION stata_to_r_code_fixest in /home/rstudio/repbox/regtranslate/R/to_r_fixest.R


!MODIFICATION stata_to_r_code_lm in /home/rstudio/repbox/regtranslate/R/to_r_lm.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_lm.R"
function_name = "stata_to_r_code_lm"
description = "Add explicit listwise deletion to emulate Stata's e(sample) matching."
---
```R
stata_to_r_code_lm = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_lm")

  org_depvars = regvar$cterm[regvar$role=="dep"]
  mod_depvars = replace_cterm_special_symbols(org_depvars)

  formula = regvar_to_formula_fixest(regvar, regxvar, cmdpart, reg = reg)

  command = "lm"
  arg_str = c(
    paste0("formula = formula"),
    paste0('data = dat')
  )

  rcmd_code = paste0('rcmd = "',command,'"')
  # We use the default ssc arguments since they are closest to the
  # Stata defaults
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

  # Apply dynamic weights via centralized helper
  wt = r_weight_code(reg, template = "dat[['%s']]")
  if (nzchar(wt$data_code)) {
    data_code = if (nzchar(data_code)) paste0(data_code, "\n", wt$data_code) else wt$data_code
  }
  if (nzchar(wt$weight_arg)) {
    arg_str = c(arg_str, wt$weight_arg)
  }

  formula_code = paste0('formula = ', formula)
  reg_code = paste0('reg = ', command, "(", paste0(arg_str, collapse=","),")")

  code_df = tibble(part = c("rcmd","data","formula", "reg"), code = c(rcmd_code,data_code, formula_code, reg_code))


  use_summary=FALSE
  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=use_summary, use_conf_int=TRUE)
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}
```
!END_MODIFICATION stata_to_r_code_lm in /home/rstudio/repbox/regtranslate/R/to_r_lm.R


!MODIFICATION stata_to_r_code_mfx in /home/rstudio/repbox/regtranslate/R/to_r_mfx.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_mfx.R"
function_name = "stata_to_r_code_mfx"
description = "Add explicit listwise deletion to emulate Stata's e(sample) matching."
---
```R
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
!END_MODIFICATION stata_to_r_code_mfx in /home/rstudio/repbox/regtranslate/R/to_r_mfx.R


!MODIFICATION stata_to_r_code_quantreg in /home/rstudio/repbox/regtranslate/R/to_r_quantreg.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_quantreg.R"
function_name = "stata_to_r_code_quantreg"
description = "Add explicit listwise deletion to emulate Stata's e(sample) matching."
---
```R
stata_to_r_code_quantreg = function(reg, regvar,regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_quantreg")

  # Ignore dropped regvars (if they are nor part of an interaction)
  #regvar = filter(regvar, !is_dropped | ia_cterm != cterm)


  # Currently we just use the fixest formula
  formula = regvar_to_formula_fixest(regvar, regxvar, cmdpart, reg = reg)

  rcmd = "rq"

  library_code = paste0("library(quantreg)")
  rcmd_code = paste0('rcmd = "',rcmd,'"')
  # We use the default ssc arguments since they are closest to the
  # Stata defaults
  formula_code = paste0('formula = ', formula)

  arg_str = NULL
  if (reg$se_category != "iid") {
    stop("Currently stata_to_r_code_quantreg is only implemented for iid standard errors. ")
  }
  arg_str = c(
    paste0("formula = formula"),
    paste0('data = dat'),
    arg_str
  )

  data_code = r_listwise_deletion_code(regvar)

  # Apply dynamic weights via centralized helper
  wt = r_weight_code(reg, template = "dat[['%s']]")
  if (nzchar(wt$data_code)) {
    data_code = if (nzchar(data_code)) paste0(data_code, "\n", wt$data_code) else wt$data_code
  }
  if (nzchar(wt$weight_arg)) {
    arg_str = c(arg_str, wt$weight_arg)
  }

  opts_df = cmdpart_to_opts_df(cmdpart)
  opt_row = which(opts_df$opt=="quantile")
  if (length(opt_row)>0) {
    arg_str = c(arg_str, paste0("tau = ", opts_df$opt_arg[opt_row]))
  }


  reg_code = paste0('reg = suppressWarnings(', rcmd,'(', paste0(arg_str, collapse=","),"))")

  code_df = tibble(part = c("library", "rcmd","data","formula","reg"), code = c(library_code, rcmd_code,data_code,formula_code,reg_code))
  code_df = code_df[code_df$code != "", ]

  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=FALSE, use_conf_int=TRUE)
    code_df = bind_rows(code_df, tibble(part="ct_mod",code='
ct = mutate(ct, std.error=NA_real_, statistic= NA_real_,  p.value = NA_real_)
if ("logLik" %in% names(glance)) {
  glance$logLik = as.numeric(glance$logLik)
}
'))
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}
```
!END_MODIFICATION stata_to_r_code_quantreg in /home/rstudio/repbox/regtranslate/R/to_r_quantreg.R


!MODIFICATION stata_to_r_code_tobit in /home/rstudio/repbox/regtranslate/R/to_r_tobit.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_tobit.R"
function_name = "stata_to_r_code_tobit"
description = "Add explicit listwise deletion to emulate Stata's e(sample) matching."
---
```R
stata_to_r_code_tobit = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_mfx")

  # Ignore dropped regvars (if they are nor part of an interaction)
  #regvar = filter(regvar, !is_dropped | ia_cterm != cterm)

  # Currently we just use the fixest formula
  formula = regvar_to_formula_fixest(regvar, regxvar, cmdpart, reg = reg)

  rcmd = "tobit"

  library_code = paste0("library(AER)")
  rcmd_code = paste0('rcmd = "',rcmd,'"')
  # We use the default ssc arguments since they are closest to the
  # Stata defaults
  formula_code = paste0('formula = ', formula)

  arg_str = NULL
  if (reg$se_category == "robust") {
    arg_str = "robust = true"
  } else if (reg$se_category == "cluster") {
    clustervar = extract_clustervar_from_se_args(reg$se_args)
    arg_str = paste0('cluster = "', clustervar[1],'"')
    if (reg$se_type == "twoway") {
      stop("Two way clusters not yet implemented for tobit in R")
    }
  }
  arg_str = c(
    paste0("formula = formula"),
    paste0('data = dat'),
    arg_str
  )

  data_code = r_listwise_deletion_code(regvar)

  # Apply dynamic weights via centralized helper
  wt = r_weight_code(reg, template = "dat[['%s']]")
  if (nzchar(wt$data_code)) {
    data_code = if (nzchar(data_code)) paste0(data_code, "\n", wt$data_code) else wt$data_code
  }
  if (nzchar(wt$weight_arg)) {
    arg_str = c(arg_str, wt$weight_arg)
  }

  reg_code = paste0('reg = ', rcmd,'(', paste0(arg_str, collapse=","),")\nclass(reg) = 'survreg'")

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
!END_MODIFICATION stata_to_r_code_tobit in /home/rstudio/repbox/regtranslate/R/to_r_tobit.R
