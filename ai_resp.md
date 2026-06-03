### Discussion on Complexity and Strategy

Implementing R translation for the Stata `newey` command is remarkably straightforward because the `metaregBase` and `regtranslate` packages are structurally prepared for it.

The primary estimation engine currently used in R, `fixest::feols()`, natively supports Newey-West standard errors through its `vcov = "NW"` (and `vcov = NW(lag) ~ panelid + timeid`) syntax. Moreover, `metaregBase::se_stata_to_repdb()` already extracts standard error arguments for `newey` into `se_type = "nw"` and parses the `lag(#)` argument successfully into `se_args = "lag=#"`. 

To complete the end-to-end translation, we only need a few localized code modifications:

1.  **Command Registration:** Add `"newey"` to the registry of supported commands (`stata_to_r_cmds_df`) in `regtranslate/R/to_r.R` so the pipeline knows `newey` defaults to the `fixest` (or optionally `lm`) engine.
2.  **Pass Metadata (`reg`):** In `stata_to_r_code_fixest()`, we need to pass the full regression parcel (`reg`) downwards to the covariance formula builder, because Newey-West standard errors require knowledge of the `timevar` and `panelvar` assigned during the `tsset`/`xtset` phase. 
3.  **Construct NW vcov Formula:** Update `fixest_vcov_code_from_regdb()` to handle `vcov_type == "NW"`. We extract the `lag` value from `se_args` and correctly format the result. Since `fixest` accepts standard error specifications as unquoted R formulas (`NW(lag) ~ panelvar + timevar` or `NW(lag) ~ timevar`), we construct this dynamically based on the available metadata. 

Here are the proposed code changes applying these modifications.

---

!MODIFICATION stata_to_r_cmds_df /home/rstudio/repbox/regtranslate/R/to_r.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r.R"
function_name = "stata_to_r_cmds_df"
description = "Add newey to the list of commands translated to fixest and lm"
---
```r
stata_to_r_cmds_df = function(cmd) {
  li = list(
    # If you add commands here make to sure that you update the to_r_fixest.R code
    fixest.. = c("regress","reg", "ivregress","ivreg","ivreg2","xtivreg", "xtivreg2", "reghdfe","ivreghdfe", "xtreg","areg","ppmlhdfe","logit","xtlogit","probit","xtprobit","dprobit", "newey"),
    lm.. = c("regress","reg", "newey"),
    ivreg.. = c("ivregress","ivreg","ivreg2", "xtivreg", "xtivreg2"),
    quantreg.. = c("qreg"),
    # Note that parmest does return coefficients not marginal
    # effects for dprobit. So we don't use mfx
    #mfx... = c("dprobit"),
    tobit.. = c("tobit")
  )
  stata_cmd = unlist(li)
  r_cmd = str.left.of(names(stata_cmd),"..")
  data.frame(stata_cmd=stata_cmd,r_cmd=r_cmd,row.names = NULL)
}
```
!END_MODIFICATION stata_to_r_cmds_df /home/rstudio/repbox/regtranslate/R/to_r.R

---

!MODIFICATION stata_to_r_code_fixest /home/rstudio/repbox/regtranslate/R/to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "stata_to_r_code_fixest"
description = "Pass the reg object to fixest_vcov_code_from_regdb to provide panel/time variables"
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
!END_MODIFICATION stata_to_r_code_fixest /home/rstudio/repbox/regtranslate/R/to_r_fixest.R

---

!MODIFICATION fixest_vcov_code_from_regdb /home/rstudio/repbox/regtranslate/R/to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "fixest_vcov_code_from_regdb"
description = "Implement proper formula formatting for NW and DK vcov in fixest based on timevar/panelvar from reg object and parsed lags."
---
```r
fixest_vcov_code_from_regdb = function(se_type, se_args, vcov_type=fixest_vcov_type_from_regdb(se_type,se_args), quote=TRUE, reg=NULL) {
  restore.point("fixest_vcov_code_from_regdb")

  if (vcov_type %in% c("cluster","twoway")) {
    clustervar = extract_clustervar_from_se_args(se_args)
    # Return as a formula (~ var1 + var2) natively supported by fixest
    code = paste0("~ ", paste0("`", clustervar, "`", collapse = " + "))
    return(code)
  }
  if (vcov_type %in% c("DK","NW")) {
    lag = NA_character_
    if (!is.na(se_args) && nzchar(se_args)) {
      args = regdb_parse_se_args(se_args)
      if ("lag" %in% names(args)) lag = args["lag"]
    }
    
    timevar = if (!is.null(reg) && !is.na(reg$timevar[1]) && nzchar(reg$timevar[1])) reg$timevar[1] else ""
    panelvar = if (!is.null(reg) && !is.na(reg$panelvar[1]) && nzchar(reg$panelvar[1])) reg$panelvar[1] else ""
    
    p_and_t = ""
    if (panelvar != "" && timevar != "") {
      p_and_t = paste0("`", panelvar, "` + `", timevar, "`")
    } else if (timevar != "") {
      p_and_t = paste0("`", timevar, "`")
    } else if (panelvar != "") {
      p_and_t = paste0("`", panelvar, "`")
    }
    
    lag_str = ""
    if (!is.na(lag) && lag != "") {
      lag_str = paste0("(", lag, ")")
    }
    
    if (p_and_t != "") {
      return(paste0(vcov_type, lag_str, " ~ ", p_and_t))
    } else {
      if (lag_str == "") {
        if (quote) return(paste0('"', vcov_type, '"'))
        return(vcov_type)
      } else {
        return(paste0(vcov_type, lag_str))
      }
    }
  }
  if (quote) return(paste0('"',vcov_type,'"'))
  return(vcov_type)
}
```
!END_MODIFICATION fixest_vcov_code_from_regdb /home/rstudio/repbox/regtranslate/R/to_r_fixest.R
