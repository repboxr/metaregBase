Yes, keeping `cmdpart` in the function signatures is definitely the right call. It ensures we don't paint ourselves into a corner if we need raw Stata syntax for future features (e.g., parsing weird weighting schemes or specific model options) that we don't necessarily want to standardize into `reg$flags`.

I have refined the code modifications I generated earlier. Specifically, I updated the string matching to handle `nocon` (since Stata allows abbreviating `noconstant` all the way down to `nocon`, not just `nocons`) and ensured the RHS of the formula is built cleanly to avoid any trailing `+` signs.

Here are all the modifications you should apply:

### 1. Update `metaregBase` to populate `reg$flags`

!MODIFICATION mrb_run_r_base_step in metaregBase/R/mrb_r_base.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_base.R"
function_name = "mrb_run_r_base_step"
description = "Add flags column to reg parcel capturing noconst option"
---
```r
#' Process a single regression, expand syntax, and format standard parcels
mrb_run_r_base_step = function(mrb, pid, with_try = isTRUE(mrb$with_try), continue_on_error=FALSE) {
  restore.point("mrb_run_r_base_step")
  if (with_try) {
    restore.point("mrb_run_r_base_step_with_try")
    res = repboxUtils::try_catch_repbox_problems(mrb_run_r_base_step(mrb,pid, with_try=FALSE, continue_on_error = continue_on_error),project_dir = mrb$project_dir,runid = pid,msg_prefix = "mrb_run_r_base_step: ", err_val=NULL)
    return(res$value)
  }

  project_dir = mrb$project_dir
  runid = pid

  xtvar = mrb$parcels$xtvar
  xtvar = xtvar[xtvar$runid==pid,]
  if (NROW(xtvar)==0) {
    xtvar = list(timevar=NA, panelvar=NA, tdelta=NA_integer_)
  }

  # 0. Load Data & Expand Syntax
  dat = repboxDRF::drf_get_data(pid, drf = mrb$drf,continue_on_error = continue_on_error)

  # NULL means problem in data loading
  if (is.null(dat)) return(list())


  # 1. Base Components
  run_obj = mrb$drf$run_df %>% filter(runid == pid)
  cmd = run_obj$cmd[1]

  all_cmdpart = mrb$parcels$reg_cmdpart
  cmdpart = all_cmdpart %>% filter(runid == pid)
  if (NROW(cmdpart) == 0) stop(paste0("No cmdpart stored for runid = ", pid))

  # 2. Extract specific Stata outcomes for this step (metaregBase 'sb')
  stata_ct = if (!is.null(mrb$stata_ct_sb)) mrb$stata_ct_sb %>% filter(runid == pid) else NULL
  stata_scalars = if (!is.null(mrb$stata_scalars)) mrb$stata_scalars %>% filter(runid == pid) else NULL
  stata_macros = if (!is.null(mrb$stata_macros)) mrb$stata_macros %>% filter(runid == pid) else NULL


  org_dat = dat
  cmdpart = cmdpart_expand_vars(cmdpart, colnames(dat))

  # 4. Extract Options, SE, and build initial regvar
  opts_df = cmdpart_to_opts_df(cmdpart)
  se_info = se_stata_to_repdb(cmd, opts_df)
  regvar = cmdpart_to_regvar(cmdpart, dat, opts_df, se_info)

  # xtreg, fe absorbs the xtset panel variable. This variable is stored in
  # xtvar/reg metadata, not in the command varlist. Add it to regvar as an
  # absorbed fixed effect so regvar_to_formula_fixest() creates "| panelvar".
  regvar = mrb_add_xtreg_fe_regvar(
    regvar = regvar,
    reg = run_obj,
    opts_df = opts_df,
    xtvar = xtvar,
    dat = dat
  )

  depvar = regvar$cterm[regvar$role == "dep"]

  # 5. Data Mutations & Stats
  ct_cterms = unique(c(depvar, regvar$var, regvar$cterm, regvar$ia_cterm)) %>% setdiff(c("(Intercept)",""))

  # Keep the full expanded dataset so make_regxvar can access generated
  # time-series columns.
  wide_dat_full = create_cterm_cols(dat, ct_cterms, timevar=xtvar$timevar, panelvar=xtvar$panelvar, tdelta=xtvar$tdelta)
  wide_dat = wide_dat_full[, ct_cterms, drop=FALSE]

  reg_types = bind_rows(
    regvar %>% select(term = cterm, reg_type = var_reg_type),
    regvar %>% select(term = ia_cterm, reg_type = ia_reg_type)
  ) %>% unique()

  colstats = make_colstats(ct_cterms, wide_dat, wide_dat, reg_types)

  #####################
  # Create step parcels
  #####################

  step_parcels = list()

  # A. REGCOEF (Parsed Stata Coefficients from metaregBase runs)
  if (!is.null(stata_ct) && nrow(stata_ct) > 0) {
    co_all = ct_to_regcoef(stata_ct, artid = mrb$artid)
    co_parcels = regcoef_split_variant_parcels(
      co_all,
      base_variant = "sb",
      base_parcel = "regcoef"
    )
    step_parcels[names(co_parcels)] = co_parcels

    regcoef_main = if (!is.null(step_parcels$regcoef)) {
      regcoef_keep_default_eq(step_parcels$regcoef)
    } else {
      tibble()
    }
  } else {
    step_parcels$regcoef = tibble()
    regcoef_main = tibble()
  }


  # A2. REGCOEF_SO (Parsed Stata Coefficients from Original DRF run 'so')
  step_parcels$regcoef_so = tibble()
  if (!is.null(mrb$regtab_so)) {
    rt_row = mrb$regtab_so %>% filter(runid == pid)
    if (nrow(rt_row) > 0 && !is.null(rt_row$ct[[1]])) {
      so_df = rt_row$ct[[1]]
      if (nrow(so_df) > 0) {
        so_df$runid = pid
        step_parcels$regcoef_so = ct_to_regcoef(so_df, variant = "so", artid = mrb$artid)
      }
    }
  }

  # B. REGVAR (Variables with prefixes and dropping info)
  dropped_cterms = if (nrow(regcoef_main) > 0) {
    regcoef_main %>% filter(is.na(coef)) %>% pull(cterm)
  } else {
    character(0)
  }

  step_parcels$regvar = regvar %>%
    mutate(
      artid = mrb$artid,
      runid = runid,
      variant = "sb",
      basevar = basevar,
      ia_source_expr = ia_expr,
      var_source_expr = var_expr,
      prefix_type = tolower(substring(prefix, 1, 1)),
      prefix_num = trimws(substring(prefix, 2)),
      prefix_num = ifelse(prefix_num == "", 1, as_integer(prefix_num)),
      transform = prefix_type,
      transform_par = ifelse(transform %in% c("", "log"), "", change_val(prefix_num, "", "1")),
      is_dropped = (cterm %in% dropped_cterms) & (role %in% c("exo", "endo"))
    )

  # C. REGXVAR
  # Absorbed fixed effects, including xtreg panel FE, are excluded inside
  # make_regxvar(). Explicit non-factor command variables are marked as
  # in_regcoef unless Stata reports them as dropped.
  step_parcels$regxvar = make_regxvar(step_parcels$regvar, wide_dat_full, regcoef_main)

  # D. REGSCALAR & REGSTRING
  if (!is.null(stata_scalars) && nrow(stata_scalars) > 0) {
    step_parcels$regscalar = stata_scalars %>%
      rename(scalar_name = var, scalar_val = val) %>%
      mutate(variant = "sb", runid = runid)

    stats_wide = stata_scalars %>% pivot_wider(names_from = var, values_from = val)
  } else {
    step_parcels$regscalar = tibble()
    stats_wide = tibble()
  }

  if (!is.null(stata_macros) && nrow(stata_macros) > 0) {
    step_parcels$regstring = stata_macros %>%
      rename(string_name = var, string_val = val) %>%
      mutate(variant = "sb", runid = runid)
  } else {
    step_parcels$regstring = tibble()
  }

  # E. COLSTAT
  step_parcels$colstat_numeric = if (nrow(colstats$colstat_numeric) > 0) {
    colstats$colstat_numeric %>% mutate(artid = mrb$artid, variant = "sb", runid = runid, cterm = col)
  } else {
    tibble()
  }

  step_parcels$colstat_dummy = if (nrow(colstats$colstat_dummy) > 0) {
    colstats$colstat_dummy %>% mutate(artid = mrb$artid, variant = "sb", runid = runid, cterm = col)
  } else {
    tibble()
  }

  step_parcels$colstat_factor = if (nrow(colstats$colstat_factor) > 0) {
    colstats$colstat_factor %>% mutate(artid = mrb$artid, variant = "sb", runid = runid, cterm = col)
  } else {
    tibble()
  }

  # F. REG & REGSOURCE
  nobs_val = if ("N" %in% names(stats_wide)) as.numeric(stats_wide$N) else NA_real_
  r2_val = if ("r2" %in% names(stats_wide)) as.numeric(stats_wide$r2) else if ("r2_p" %in% names(stats_wide)) as.numeric(stats_wide$r2_p) else NA_real_

  flags_vec = character()
  if (any(startsWith(tolower(opts_df$opt), "nocon"))) {
    flags_vec = c(flags_vec, "noconst")
  }
  flags_str = paste0(flags_vec, collapse = ", ")

  reg_dat = tibble(
    runid = pid,
    variant = "sb",
    base_variant = "sb",
    lang = "stata",
    source_lang = "stata",
    cmd = cmd,
    cmdline = run_obj$cmdline[1],
    timevar = xtvar$timevar,
    panelvar = xtvar$panelvar,
    tdelta = as_integer(xtvar$tdelta[1]),
    se_category = se_info$se_category,
    se_type = se_info$se_type,
    se_args = se_info$se_args,
    ncoef = if (nrow(step_parcels$regcoef) > 0) nrow(step_parcels$regcoef) else NA_integer_,
    iv_code = any(step_parcels$regvar$role == "instr"),
    nobs = nobs_val,
    nobs_org = NROW(org_dat),
    r2 = r2_val,
    flags = flags_str,
    error_in_r = FALSE
  )

  step_parcels$reg = reg_dat
  step_parcels$colinfo = repbox_compute_col_info(runid, project_dir, dat, org_dat, reg_dat)

  return(step_parcels)
}
```
!END_MODIFICATION mrb_run_r_base_step in metaregBase/R/mrb_r_base.R


### 2. Pass `reg` to the formula builders

!MODIFICATION reg_stata_to_r_formula in regtranslate/R/to_r.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r.R"
function_name = "reg_stata_to_r_formula"
description = "Pass reg parcel to regvar_to_formula_* functions."
---
```r
reg_stata_to_r_formula = function(reg, regvar, regxvar, cmdpart, prefer="fixest", opts=code_options()) {
  restore.point("reg_stata_to_r_formula")

  r_cmd = get_stata_to_r_cmd(reg$cmd, prefer)
  if (isTRUE(r_cmd == "no_trans")) {
    return(NULL)
  } else if (is.na(r_cmd)) {
    stop(paste0("The Stata command ", reg$cmd, " is neither implemented for translation nor specified in stata_cmds_without_r_translation()"))
  }

  args = list(regvar=regvar,regxvar=regxvar, cmdpart=cmdpart, reg=reg)
  fun = paste0("regvar_to_formula_",r_cmd)
  res = do.call(fun, args)
  as.formula(res)
}
```
!END_MODIFICATION reg_stata_to_r_formula in regtranslate/R/to_r.R


### 3. Update the `fixest` formula builders

!MODIFICATION regvar_to_formula_fixest in regtranslate/R/to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "regvar_to_formula_fixest"
description = "Update formula creation to respect noconst flag from reg parcel."
---
```r
# Ideally this is independent of the original language from
# which regvar was generated. E.g. it would be create
# if we could translate both a stata command and an R command to
# fixest
regvar_to_formula_fixest = function(regvar, regxvar, cmdpart, reg = NULL) {
  restore.point("regvar_to_formula_fixest")

  add_main_effects = TRUE

  rv = regvar %>% filter(role == "dep" | absorbed_fe)
  # Update: for variables starting with _I (typically generated by xi)
  # in_regcoef does not always work proper.
  # Example:   artid = "aejmac_3_3_5"; steps = 3
  # So we assume they are part of the formula
  # possibly a problem arises if they are dropped...
  # at some point I need to think on more robust handling
  rxv = regxvar %>% filter(role !="exo" | in_regcoef | startsWith(cterm,"_I"))

  rv = rv %>% mutate(
    prefix = str.left.of(cterm,"@", not.found=rep("", length(cterm))) %>% tolower()
  )

  rxv = rxv %>% mutate(
    prefix = str.left.of(cterm,"@", not.found=rep("", length(cterm))) %>% tolower()
  )

  depvars = rv$cterm[rv$role=="dep"]
  depvars = replace_cterm_special_symbols(depvars)
  form = paste0(paste0("`",depvars,"`", collapse=" + "), " ~ ")

  omit_constant = FALSE
  if (!is.null(reg) && "flags" %in% names(reg)) {
    omit_constant = stringi::stri_detect_fixed(reg$flags, "noconst")
  } else if (!is.null(cmdpart)) {
    omit_constant = any(cmdpart$part=="opt" & startsWith(tolower(cmdpart$content), "nocon"))
  }

  rhs_terms = character()
  if (omit_constant) {
    rhs_terms = c(rhs_terms, "0")
  }

  # Exogeneous x that are no FE
  rows = which(rxv$role == "exo")
  if (sum(rows)>0) {
    rhs_terms = c(rhs_terms, paste0("`",rxv$cterm[rows],"`"))
  } else {
    if (!omit_constant) {
      rhs_terms = c(rhs_terms, "1")
    }
  }

  form = paste0(form, paste0(rhs_terms, collapse= " + "))

  # Exogeneous x as FE
  rows = which(rv$role == "exo" & rv$absorbed_fe)
  if (sum(rows)>0) {
    fe_terms = rv[rows, ] %>%
      group_by(ia_cterm) %>%
      arrange(ia_pos) %>%
      summarize(
        fe_expr = {
          if (first(ia_reg_type) == "factor_numeric" && n() == 2) {
            f_idx = which(var_reg_type %in% c("factor", "fe"))[1]
            n_idx = which(!var_reg_type %in% c("factor", "fe"))[1]
            if (is.na(f_idx)) f_idx = 1
            if (is.na(n_idx)) n_idx = 2
            paste0("`", cterm[f_idx], "`[`", cterm[n_idx], "`]")
          } else {
            paste0("`", cterm, "`", collapse = "^")
          }
        }
      ) %>%
      pull(fe_expr)
    form = paste0(form, " | ", paste0(fe_terms, collapse = " + "))
  }

  # Endogeneous x and instruments (never FE)
  rows = which(rxv$role == "endo")
  if (sum(rows)>0) {
    form = paste0(form, " | ",paste0("`",rxv$cterm[rows],"`", collapse= " + "))
  }
  rows = which(rxv$role == "instr")
  if (sum(rows)>0) {
    form = paste0(form, " ~ ",paste0("`",rxv$cterm[rows],"`", collapse= " + "))
  }
  form
}
```
!END_MODIFICATION regvar_to_formula_fixest in regtranslate/R/to_r_fixest.R

!MODIFICATION regvar_to_formula_fixest_noregxvar in regtranslate/R/to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "regvar_to_formula_fixest_noregxvar"
description = "Update formula creation to respect noconst flag from reg parcel for noregxvar variant."
---
```r
# Ideally this is independent of the original language from
# which regvar was generated. E.g. it would be create
# if we could translate both a stata command and an R command to
# fixest
regvar_to_formula_fixest_noregxvar = function(regvar, regxvar, cmdpart, reg = NULL) {
  restore.point("regvar_to_formula_fixest")

  add_main_effects = TRUE

  rv = regvar
  rv = rv %>% mutate(
    prefix = str.left.of(cterm,"@", not.found=rep("", length(cterm))) %>% tolower(),
    is_ia = ia_num > 1
  )

  # We replace prefix @ with .
  #rv = replace_regvar_prefix_sep(rv, "@",".")

  depvars = rv$cterm[rv$role=="dep"]
  form = paste0(paste0(depvars, collapse=" + "), " ~ ")

  omit_constant = FALSE
  if (!is.null(reg) && "flags" %in% names(reg)) {
    omit_constant = stringi::stri_detect_fixed(reg$flags, "noconst")
  } else if (!is.null(cmdpart)) {
    omit_constant = any(cmdpart$part=="opt" & startsWith(tolower(cmdpart$content), "nocon"))
  }

  rhs_terms = character()
  if (omit_constant) {
    rhs_terms = c(rhs_terms, "0")
  }

  # In stata x variables starting with o. like o.var will be omitted
  rv = rv %>% filter(prefix!="o")

  # TO DO: Specify whether in interaction A*B also A and B
  #        should be included or not.
  if (sum(rv$is_ia) >0 ) {
    ia = rv %>%
      filter(is_ia) %>%
      group_by(is_ia, ia_cterm, role, ia_num, main_pos) %>%
      arrange(desc(var_reg_type=="factor"), desc(var_reg_type=="dummy"), ia_pos) %>%
      summarize(
        absorbed_fe = first(absorbed_fe),
        ia_type = case_when(
          all(var_reg_type=="dummy") ~ "dummies",
          all(var_reg_type=="numeric") ~ "numeric",
          all(var_reg_type %in% c("dummy","factor")) ~ "factor",
          ia_num == 2 & var_reg_type[1] == "dummy" & (var_reg_type[2]=="numeric") ~ "dummy_numeric",
          ia_num == 2 & var_reg_type[1] %in% c("factor", "fe") & var_reg_type[2] == "numeric" ~ "factor_numeric",
          TRUE ~ "unknown"
        )[1],
        fe_expr = case_when(
          ia_type == "factor_numeric" ~ {
            f_idx = which(var_reg_type %in% c("factor", "fe"))[1]
            n_idx = which(!var_reg_type %in% c("factor", "fe"))[1]
            if (is.na(f_idx)) f_idx = 1
            if (is.na(n_idx)) n_idx = 2
            paste0("`", cterm[f_idx], "`[`", cterm[n_idx], "`]")
          },
          TRUE ~ paste0("`", cterm, "`", collapse="^")
        )[1],
        x_expr = paste0(
          ifelse(!var_reg_type %in% c("factor", "fe"), paste0("`", cterm, "`"), paste0("factor(`",cterm,"`)")),
          collapse= if (isTRUE(first(add_main_effects))) "*" else ":"
        )[1]
      ) %>%
      ungroup()
  } else {
    ia = NULL
  }

  no_ia = rv %>%
    filter(!is_ia) %>%
    mutate(
      fe_expr = paste0("`", cterm, "`"),
      x_expr = case_when(
        var_reg_type %in% c("factor", "fe") ~ paste0("factor(`", cterm,"`)"),
        TRUE ~ paste0("`", cterm, "`")
      )
    )

  terms = bind_rows(ia, no_ia) %>%
    arrange(main_pos)

  # Exogeneous x that are no FE
  rows = which(terms$role == "exo" & !terms$absorbed_fe)
  if (sum(rows)>0) {
    rhs_terms = c(rhs_terms, terms$x_expr[rows])
  } else {
    if (!omit_constant) {
      rhs_terms = c(rhs_terms, "1")
    }
  }

  form = paste0(form, paste0(rhs_terms, collapse= " + "))

  # Exogeneous x as FE
  rows = which(terms$role == "exo" & terms$absorbed_fe)
  if (sum(rows)>0) {
    form = paste0(form, " | ",paste0(terms$fe_expr[rows], collapse= " + "))
  }

  # Endogeneous x and instruments (never FE)
  rows = which(terms$role == "endo")
  if (sum(rows)>0) {
    form = paste0(form, " | ",paste0(terms$x_expr[rows], collapse= " + "))
  }
  rows = which(terms$role == "instr")
  if (sum(rows)>0) {
    form = paste0(form, " ~ ",paste0(terms$x_expr[rows], collapse= " + "))
  }
  form
}
```
!END_MODIFICATION regvar_to_formula_fixest_noregxvar in regtranslate/R/to_r_fixest.R


### 4. Pass `reg` through all code generators

!MODIFICATION stata_to_r_code_fixest in regtranslate/R/to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "stata_to_r_code_fixest"
description = "Pass reg to regvar_to_formula_fixest"
---
```r
# Replace stata_to_r_code_fixest and fixest_vcov_code_from_regdb
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
    reg_vcov = fixest_vcov_code_from_regdb(reg$se_type, reg$se_args, vcov_type, quote=FALSE)
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

  weight_var = regvar$cterm[regvar$role == "weight"]
  if (length(weight_var)>0) {
    arg_str = c(arg_str, paste0("weights = ~", paste0(weight_var, collapse="+")))
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
!END_MODIFICATION stata_to_r_code_fixest in regtranslate/R/to_r_fixest.R


!MODIFICATION regvar_to_formula_lm in regtranslate/R/to_r_lm.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_lm.R"
function_name = "regvar_to_formula_lm"
description = "Pass reg to regvar_to_formula_fixest"
---
```r
regvar_to_formula_lm = function(regvar, regxvar, cmdpart, reg = NULL) {
  regvar_to_formula_fixest(regvar, regxvar, cmdpart, reg = reg)
}
```
!END_MODIFICATION regvar_to_formula_lm in regtranslate/R/to_r_lm.R

!MODIFICATION stata_to_r_code_lm in regtranslate/R/to_r_lm.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_lm.R"
function_name = "stata_to_r_code_lm"
description = "Pass reg to regvar_to_formula_fixest"
---
```r
# TO DO: VCOV
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

  weight_var = regvar$cterm[regvar$role == "weight"]
  if (length(weight_var)>0) {
    arg_str = c(arg_str, paste0("weights = dat$", paste0(weight_var, collapse="+")))
  }

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
  formula_code = paste0('formula = ', formula)
  reg_code = paste0('reg = ', command, "(", paste0(arg_str, collapse=","),")")

  code_df = tibble(part = c("rcmd","data","formula", "reg"), code = c(rcmd_code,data_code, formula_code, reg_code))


  # vcov_type = fixest_vcov_type_from_regdb(reg$se_type, reg$se_args)
  # use_sandwich = TRUE
  # use_summary = use_sandwich | opts$prefer_summary
  #
  # if (use_sandwich) {
  #   reg_vcov = "iid"
  #   vcov = regdb_se_to_sandwich(reg$se_category, reg$se_type, reg$se_args)
  # }
  #
  #
  # if (use_summary) {
  #   sum_vcov_code = paste0('sum_vcov = ', quote_arg(vcov))
  #   sum_code = 'sum = summary(reg, vcov = sum_vcov)'
  #   code_df = bind_rows(code_df,
  #     tibble(part = c("sum_vcov","sum"), code = c(sum_vcov_code, sum_code))
  #   )
  # }
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
!END_MODIFICATION stata_to_r_code_lm in regtranslate/R/to_r_lm.R

!MODIFICATION stata_to_r_code_mfx in regtranslate/R/to_r_mfx.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_mfx.R"
function_name = "stata_to_r_code_mfx"
description = "Pass reg to regvar_to_formula_fixest"
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
  library_code = "library(MASS, exclude='select')
library(mfx)
  "
  rcmd_code = paste0('rcmd = "',rcmd,'"')
  # We use the default ssc arguments since they are closest to the
  # Stata defaults
  formula_code = paste0('formula = ', formula)

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
  code_df = tibble(part = c("library", "rcmd","formula","reg"), code = c(library_code, rcmd_code,formula_code,reg_code))

  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=FALSE, use_conf_int=TRUE)
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}
```
!END_MODIFICATION stata_to_r_code_mfx in regtranslate/R/to_r_mfx.R

!MODIFICATION stata_to_r_code_quantreg in regtranslate/R/to_r_quantreg.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_quantreg.R"
function_name = "stata_to_r_code_quantreg"
description = "Pass reg to regvar_to_formula_fixest"
---
```r
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

  weight_var = regvar$cterm[regvar$role == "weight"]
  if (length(weight_var)==1) {
    arg_str = c(arg_str, paste0('weights = dat[["',weight_var,'"]]'))
  } else if (length(weight_var)>1) {
    stop("Cannot deal with multiple weight variables.")
  }

  opts_df = cmdpart_to_opts_df(cmdpart)
  opt_row = which(opts_df$opt=="quantile")
  if (length(opt_row)>0) {
    arg_str = c(arg_str, paste0("tau = ", opts_df$opt_arg[opt_row]))
  }


  reg_code = paste0('reg = suppressWarnings(', rcmd,'(', paste0(arg_str, collapse=","),"))")

  code_df = tibble(part = c("library", "rcmd","formula","reg"), code = c(library_code, rcmd_code,formula_code,reg_code))

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
!END_MODIFICATION stata_to_r_code_quantreg in regtranslate/R/to_r_quantreg.R

!MODIFICATION stata_to_r_code_tobit in regtranslate/R/to_r_tobit.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_tobit.R"
function_name = "stata_to_r_code_tobit"
description = "Pass reg to regvar_to_formula_fixest"
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
    if (reg$se_type == "twoway") {
      stop("Two way clusters not yet implemented for tobit in R")
    }
  }
  arg_str = c(
    paste0("formula = formula"),
    paste0('data = dat'),
    arg_str
  )

  weight_var = regvar$cterm[regvar$role == "weight"]
  if (length(weight_var)==1) {
    arg_str = c(arg_str, paste0('weights = dat[["',weight_var,'"]]'))
  } else if (length(weight_var)>1) {
    stop("Cannot deal with mutliple weight variables.")
  }


  reg_code = paste0('reg = ', rcmd,'(', paste0(arg_str, collapse=","),")
class(reg) = 'survreg'")

  code_df = tibble(part = c("library", "rcmd","formula","reg"), code = c(library_code, rcmd_code,formula_code,reg_code))

  if (opts$add_broom) {
    code_df = add_reg_broom_code(code_df, use_summary=FALSE, use_conf_int=TRUE)
  }
  if (opts$add_function) {
    code_df = add_reg_function_code(code_df)
  }
  code_df
}
```
!END_MODIFICATION stata_to_r_code_tobit in regtranslate/R/to_r_tobit.R
