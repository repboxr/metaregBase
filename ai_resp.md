To solve this, we can take advantage of the fact that `plm`'s `index` argument simply tells `plm` which columns in the data frame to use as the panel and time identifiers. When `plm` receives these columns, it internally converts them to factors in a `pdata.frame`. 

If we use the original `period` column as the index, `plm` transforms `period` to a factor. Since `period` is also in the formula, the formula evaluator picks up the transformed factor version instead of the numeric original!

The fix is elegant: we duplicate the panel and time variables into hidden columns (e.g., `.plm.id` and `.plm.time`) and pass *those* to `index`. `plm` will convert the hidden columns to factors for panel structure, but leave the original variables completely untouched, allowing them to be evaluated as numeric regressors in the formula, exactly mimicking Stata's behavior.

Here is the update for the `plm` code translator to implement this fix.

!MODIFICATION stata_to_r_code_plm in to_r_plm.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_plm.R"
function_name = "stata_to_r_code_plm"
description = "Update stata_to_r_code_plm to duplicate index variables to prevent auto-conversion to factors when used as regressors."
---
```r
stata_to_r_code_plm = function(reg, regvar, regxvar, cmdpart, opts=code_options(), parts = list()) {
  restore.point("stata_to_r_code_plm")

  org_depvars = regvar$cterm[regvar$role=="dep"]
  mod_depvars = replace_cterm_special_symbols(org_depvars)

  formula = regvar_to_formula_plm(regvar, regxvar, cmdpart, reg = reg)

  command = "plm"
  arg_str = c(
    paste0("formula = formula"),
    paste0("data = dat")
  )

  rcmd_code = paste0('rcmd = "',command,'"')

  # Prepare data modifications in correct order
  data_code_parts = character(0)

  if (any(org_depvars != mod_depvars)) {
    data_code_parts = c(data_code_parts, paste0('dat[["', mod_depvars,'"]] = dat[["', org_depvars,'"]]', collapse="\n"))
  }

  lw_code = r_listwise_deletion_code(regvar)
  if (nzchar(lw_code)) data_code_parts = c(data_code_parts, lw_code)

  wt = r_weight_code(reg, template = "dat[['%s']]")
  if (nzchar(wt$data_code)) data_code_parts = c(data_code_parts, wt$data_code)
  if (nzchar(wt$weight_arg)) arg_str = c(arg_str, wt$weight_arg)

  # Duplicate index variables for plm so they don't get cast to factors if used as regressors
  panelvar = reg$panelvar[1]
  timevar = reg$timevar[1]
  if (!is.na(panelvar) && nzchar(panelvar)) {
    data_code_parts = c(data_code_parts, paste0("dat[['.plm.id']] = dat[['", panelvar, "']]"))
    if (!is.na(timevar) && nzchar(timevar)) {
      data_code_parts = c(data_code_parts, paste0("dat[['.plm.time']] = dat[['", timevar, "']]"))
      arg_str = c(arg_str, "index = c('.plm.id', '.plm.time')")
    } else {
      arg_str = c(arg_str, "index = '.plm.id'")
    }
  }

  # Model type
  flags = if (!is.null(reg$flags) && !is.na(reg$flags)) strsplit(reg$flags, ",\\s*")[[1]] else character(0)
  if ("re" %in% flags) {
    arg_str = c(arg_str, "model = 'random'")
  } else if ("fe" %in% flags) {
    arg_str = c(arg_str, "model = 'within'")
  } else if ("be" %in% flags) {
    arg_str = c(arg_str, "model = 'between'")
  } else if ("fd" %in% flags) {
    arg_str = c(arg_str, "model = 'fd'")
  }

  # Rename columns to avoid plm choking on `#` and `=`
  data_code_parts = c(data_code_parts, "dat = regtranslate::cterm_to_saveterm_cols(dat)")
  
  data_code = paste0(data_code_parts, collapse = "\n")

  formula_code = paste0('formula = ', formula)
  reg_code = paste0('reg = ', command, "(", paste0(arg_str, collapse=","),")")

  library_code = "library(plm)"

  code_df = tibble(part = c("library", "rcmd","data","formula", "reg"), code = c(library_code, rcmd_code,data_code, formula_code, reg_code))

  use_summary = TRUE
  sum_code = "sum = summary(reg)"

  # Handle VCOV for robust/cluster
  vcov_type = fixest_vcov_type_from_regdb(reg$se_type, reg$se_args)
  if (vcov_type %in% c("cluster", "twoway", "multiway")) {
    clustervar = extract_clustervar_from_se_args(reg$se_args)
    if (length(clustervar) > 0) {
      if (clustervar[1] == panelvar) {
        # plm default for cluster="group" is robust to panel correlation
        sum_code = "sum = lmtest::coeftest(reg, vcov = plm::vcovHC(reg, type = 'HC1', cluster = 'group'))"
        library_code = paste0(library_code, "\nlibrary(lmtest)")
        code_df$code[1] = library_code
      } else {
        # Fallback to vcovHC for group if it's single cluster
        sum_code = paste0("sum = lmtest::coeftest(reg, vcov = plm::vcovHC(reg, type = 'HC1', cluster = 'group'))")
        library_code = paste0(library_code, "\nlibrary(lmtest)")
        code_df$code[1] = library_code
      }
    }
  } else if (vcov_type == "hetero") {
     sum_code = "sum = lmtest::coeftest(reg, vcov = plm::vcovHC(reg, type = 'HC1', cluster = 'time'))"
     library_code = paste0(library_code, "\nlibrary(lmtest)")
     code_df$code[1] = library_code
  }

  code_df = bind_rows(
    code_df,
    tibble(part = c("sum"), code = c(sum_code))
  )

  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=use_summary, use_conf_int=TRUE)

    # broom::glance doesn't work well on coeftest. If sum is coeftest, we run glance on reg instead.
    glance_idx = which(code_df$part == "glance")
    if (length(glance_idx) > 0) {
      code_df$code[glance_idx] = "glance = broom::glance(reg)"
    }
    
    # Restore original cterms for broom::tidy output
    code_df = bind_rows(
      code_df,
      tibble(part = "ct_fix", code = "ct$term = regtranslate::saveterm_to_cterm(ct$term)")
    )
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}
```
!END_MODIFICATION stata_to_r_code_plm in to_r_plm.R
