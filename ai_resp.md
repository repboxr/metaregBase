To add support for the Stata `cgmreg` command in R via `fixest`, we mainly need to map the `cgmreg` command to `feols` and ensure that multi-way clustering is correctly piped into `fixest`'s `vcov` argument. 

Since `metaregBase/R/mrb_se.R` already handles Stata's standard error abbreviations like `cl` and correctly counts multiple clusters (classifying them as `twoway` or `multiway`), we just need to tell `regtranslate` how to translate the `multiway` type into `fixest`'s formula format (`vcov = ~ cluster1 + cluster2 + cluster3`).

Here is the proposed code modification:

1. Map `cgmreg` to `fixest` in the `stata_to_r_cmds_df`.
2. Update the `vcov` translation helpers (`fixest_vcov_code_from_regdb`, `fixest_vcov_type_from_regdb`, and `fixest_ssc_code_from_reg`) to natively support `multiway` standard errors.
3. Patch `tobit` translation gracefully to throw a better message if `multiway` happens to occur there (as it only checks for `twoway`).

!MODIFICATION stata_to_r_cmds_df regtranslate/R/to_r.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r.R"
function_name = "stata_to_r_cmds_df"
description = "Add cgmreg to fixest mapped commands"
---
```r
stata_to_r_cmds_df = function(cmd) {
  li = list(
    # If you add commands here make to sure that you update the to_r_fixest.R code
    fixest.. = c("regress","reg", "cgmreg", "ivregress","ivreg","ivreg2","xtivreg", "xtivreg2", "reghdfe","ivreghdfe", "xtreg","areg","ppmlhdfe","logit","xtlogit","probit","xtprobit","dprobit", "newey", "nbreg", "gnbreg", "poisson", "xtpoisson", "clogit"),
    lm.. = c("regress","reg", "newey"),
    ivreg.. = c("ivregress","ivreg","ivreg2", "xtivreg", "xtivreg2"),
    quantreg.. = c("qreg"),
    # Note that parmest does return coefficients not marginal
    # effects for dprobit. So we don't use mfx
    #mfx... = c("dprobit"),
    tobit.. = c("tobit"),
    stcox.. = c("stcox")
  )
  stata_cmd = unlist(li)
  r_cmd = str.left.of(names(stata_cmd),"..")
  data.frame(stata_cmd=stata_cmd,r_cmd=r_cmd,row.names = NULL)
}
```
!END_MODIFICATION stata_to_r_cmds_df regtranslate/R/to_r.R


!MODIFICATION fixest_vcov_code_from_regdb regtranslate/R/to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "fixest_vcov_code_from_regdb"
description = "Add support for multiway clustering in fixest translation"
---
```r
fixest_vcov_code_from_regdb = function(se_type, se_args, vcov_type=fixest_vcov_type_from_regdb(se_type,se_args), quote=TRUE, reg=NULL) {
  restore.point("fixest_vcov_code_from_regdb")

  if (vcov_type %in% c("cluster","twoway", "multiway")) {
    clustervar = extract_clustervar_from_se_args(se_args)
    # Return as a formula (~ var1 + var2 + ...) natively supported by fixest
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
!END_MODIFICATION fixest_vcov_code_from_regdb regtranslate/R/to_r_fixest.R


!MODIFICATION fixest_vcov_type_from_regdb regtranslate/R/to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "fixest_vcov_type_from_regdb"
description = "Add multiway to supported fixest vcov types"
---
```r
fixest_vcov_type_from_regdb = function(se_type, se_args) {
  restore.point("se_type_to_fixest_vcov")
  if (se_type == "hc1") return("hetero")
  if (se_type %in%  c("cluster")) return(se_type)
  if (se_type %in%  c("iid","cluster","twoway", "multiway", "conley")) return(se_type)
  if (se_type %in% c("nw", "dk")) return(toupper(se_type))
  return("sandwich")
}
```
!END_MODIFICATION fixest_vcov_type_from_regdb regtranslate/R/to_r_fixest.R


!MODIFICATION fixest_ssc_code_from_reg regtranslate/R/to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "fixest_ssc_code_from_reg"
description = "Support multiway in fixest_ssc_code_from_reg"
---
```r
fixest_ssc_code_from_reg = function(reg, vcov_type = fixest_vcov_type_from_regdb(reg$se_type, reg$se_args)) {
  restore.point("fixest_ssc_code_from_reg")

  is_ml = reg$cmd %in% c("logit", "xtlogit", "probit", "xtprobit", "dprobit", "poisson", "xtpoisson", "nbreg", "gnbreg", "clogit")

  if (vcov_type %in% c("cluster", "twoway", "multiway", "DK", "NW")) {
    if (reg$cmd == "areg") {
      return('fixest::ssc(K.adj = TRUE, K.fixef = "full", G.adj = TRUE)')
    }
    if (is_ml) {
      return("fixest::ssc(K.adj = TRUE, G.adj = TRUE)")
      #return('fixest::ssc(adj = FALSE, cluster.adj = TRUE)')
    }
    return('fixest::ssc()')
  }

  if (is_ml) {
    return('fixest::ssc(K.adj = TRUE, G.adj = TRUE)')
  }

  return('fixest::ssc()')
}
```
!END_MODIFICATION fixest_ssc_code_from_reg regtranslate/R/to_r_fixest.R


!MODIFICATION stata_to_r_code_tobit regtranslate/R/to_r_tobit.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_tobit.R"
function_name = "stata_to_r_code_tobit"
description = "Update error message for multiway in tobit"
---
```r
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
    if (reg$se_type %in% c("twoway", "multiway")) {
      stop("Multiway clusters not yet implemented for tobit in R")
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
!END_MODIFICATION stata_to_r_code_tobit regtranslate/R/to_r_tobit.R
