Both recommendations are excellent. 

1. **Centralizing weight translation**: I have created a new helper file `weights.R` inside the `regtranslate` package. It exports `r_weight_code()`, which processes the `reg` parcel and returns a clean list with `$data_code` (the `.weight = with(dat, ...)` generation line) and `$weight_arg` (the argument to inject into the `feols` or `lm` call). This slims down the individual translation files drastically.
2. **Regex Fast-Path**: I added `stringi::stri_detect_regex(weight_str, "[^A-Za-z0-9_]")`. Stata variable names only allow alphanumeric characters and underscores. If it contains *anything else* (math operators, parentheses, or even decimals for constant weights like `1.5`), it evaluates as an expression. This completely bypasses the `try(parse(...))` overhead for 90% of regressions.

Here are the complete modifications implementing this refactoring.

!MODIFICATION cmdpart_to_regvar /home/rstudio/repbox/metaregBase/R/mrb_reg_tools.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_reg_tools.R"
function_name = "cmdpart_to_regvar"
description = "Extract weight components only when necessary, using a regex fast-path to detect expressions."
---
```r
#' Create the regvar (vi) table strictly from the expanded cmdpart, opts_df, and se_info
cmdpart_to_regvar = function(cmdpart, dat, opts_df, se_info) {
  restore.point("cmdpart_to_regvar")

  # 1. Collect all terms mapped by role
  term_list = list()

  # Standard variables (dep, exo, endo, instr)
  v_df = cmdpart %>% dplyr::filter(part == "v")
  if (nrow(v_df) > 0) {
    # Replace tag names with role names (depvar -> dep, others stay same)
    v_df$role = ifelse(v_df$tag == "depvar", "dep", v_df$tag)
    term_list[[1]] = dplyr::tibble(ia_expr = v_df$content, role = v_df$role, option = "")
  }

  # Weights
  w_df = cmdpart %>% dplyr::filter(part == "weight_var")
  if (nrow(w_df) > 0) {
    w_expr = w_df$content[1]
    is_expr = stringi::stri_detect_regex(w_expr, "[^A-Za-z0-9_]")
    
    if (is_expr) {
      vars = try(all.vars(parse(text = w_expr)), silent = TRUE)
      if (!inherits(vars, "try-error") && length(vars) > 0) {
        term_list[[2]] = dplyr::tibble(ia_expr = vars, role = "weight_comp", option = "")
      }
    } else {
      term_list[[2]] = dplyr::tibble(ia_expr = w_expr, role = "weight_comp", option = "")
    }
  }

  # Absorb (from reghdfe / areg)
  absorb_opts = opts_df %>% dplyr::filter(opt %in% c("absorb", "a", "ab", "abs", "abso", "absor"))
  if (nrow(absorb_opts) > 0) {
    abs_vars = strsplit(shorten.spaces(paste0(absorb_opts$opt_arg, collapse = " ")), " ", fixed = TRUE)[[1]]
    term_list[[3]] = dplyr::tibble(ia_expr = abs_vars, role = "exo", option = "absorb")
  }

  # FE (from xtreg)
  if (any(opts_df$opt == "fe")) {
    # xtreg assumes panelvar is already set via xtset, we'll append it later if needed,
    # or rely on the drf run_obj panelvar injection.
  }

  # Cluster / SE
  if (!is.null(se_info$se_args) && se_info$se_args != "") {
    se_args_parsed = repdb_parse_se_args(se_info$se_args, as_df = TRUE)
    cluster_vars = se_args_parsed$arg_val[startsWith(se_args_parsed$arg_name, "cluster")]
    if (length(cluster_vars) > 0) {
      term_list[[4]] = dplyr::tibble(ia_expr = cluster_vars, role = "cluster", option = "se")
    }
  }

  vi = dplyr::bind_rows(term_list) %>% dplyr::mutate(main_pos = seq_len(dplyr::n()))

  # 2. Process Interaction Effects and Prefixes
  vi$is_ia = grepl("(\\|)|(#)|(\\*)", vi$ia_expr)
  vi$var_expr = as.list(vi$ia_expr)

  # Unnest interactions
  rows = which(vi$is_ia)
  vi$var_expr[rows] = strsplit(vi$ia_expr[rows], "(##)|(#)|(\\|)|(\\*)")

  vi = vi %>%
    tidyr::unnest(var_expr) %>%
    dplyr::group_by(ia_expr) %>%
    dplyr::mutate(ia_num = dplyr::n(), ia_pos = seq_len(dplyr::n())) %>%
    dplyr::ungroup()

  # Extract Prefix (L1., F., i., c., etc.) - split at LAST dot
  prefix_start = stringi::stri_locate_last_fixed(vi$var_expr, ".")[, 1]
  vi$prefix = ifelse(
    is.na(prefix_start),
    "",
    stringi::stri_sub(vi$var_expr, 1, prefix_start - 1) %>% stringi::stri_replace_all_fixed(".", "")
  )
  vi$var = ifelse(is.na(prefix_start), vi$var_expr, stringi::stri_sub(vi$var_expr, prefix_start + 1))

  # Normalize specific prefixes
  vi = vi %>%
    dplyr::mutate(prefix = dplyr::case_when(
      startsWith(tolower(prefix), "ib") ~ paste0("b", substring(prefix, 3)),
      TRUE ~ prefix
    ))

  # 3. Incorporate column stats info
  cols_info = make_cols_small_info(dat)
  vi = vi %>% dplyr::left_join(cols_info, by = c("var" = "col"))

  # 4. Determine Types and Classes
  vi = vi %>%
    dplyr::mutate(
      is_factor = class %in% c("character", "factor"),
      fe_type = dplyr::case_when(
        startsWith(tolower(prefix), "c") ~ "",
        startsWith(tolower(prefix), "i") ~ "i",
        startsWith(tolower(prefix), "b") ~ "b",
        option %in% c("absorb", "fe") ~ option,
        is_factor ~ class,
        TRUE ~ ""
      ),
      absorbed_fe = option %in% c("absorb", "fe"),
      is_fe = fe_type != "",
      varclass = class,
      class = ifelse(is_fe & !is_factor, "fe", class),
      add_main_effects = is_ia & (has.substr(ia_expr, "##") | has.substr(ia_expr, "*"))
    )

  # 5. Build Canonical Terms
  vi$ia_cterm = stata_expr_to_cterm(vi$ia_expr)
  vi$cterm = stata_expr_to_cterm(vi$var_expr)
  vi$basevar = stata_expr_to_cterm(vi$var)

  # If a variable is xi-generated (_I...) and the cached data still carries the
  # original Stata variable label, use that label to canonicalize the term.
  # This keeps regvar/regxvar/R output aligned with Stata regcoef parcels.
  var_labels = vapply(dat, function(v) {
    lab = attr(v, "label")
    if (is.null(lab) || length(lab) == 0 || is.na(lab[[1]])) {
      return("")
    }
    as.character(lab[[1]])
  }, character(1))

  xi_rows = startsWith(vi$var, "_I")
  if (any(xi_rows)) {
    xi_labels = unname(var_labels[vi$var])
    xi_has_label = xi_rows & !is.na(xi_labels) & stringi::stri_detect_fixed(xi_labels, "==")

    if (any(xi_has_label)) {
      vi$cterm[xi_has_label] = canonical.output.terms.stata.xi(
        terms = vi$var[xi_has_label],
        labels = xi_labels[xi_has_label]
      )
    }
  }

  # Rebuild ia_cterm from the updated component cterms so interactions with xi
  # variables also become canonical.
  vi = vi %>%
    dplyr::group_by(main_pos) %>%
    dplyr::mutate(
      ia_cterm = {
        if (dplyr::n() == 1) {
          cterm
        } else {
          rep(
            split_and_sort(
              paste0(cterm, collapse = "#"),
              split = "#",
              k = dplyr::n()
            )[[1]],
            dplyr::n()
          )
        }
      }
    ) %>%
    dplyr::ungroup()

  # basevar should refer to the underlying source variable, not the raw _I name
  vi$basevar = stringi::stri_replace_first_regex(vi$cterm, "^.*@", "")
  vi$basevar = stringi::stri_replace_first_regex(vi$basevar, "=.*$", "")

  vi$class = ifelse(has.substr(vi$cterm, "="), "dummy", vi$class)

  # 6. Apply interaction types & Reg Types
  vi = vi_add_ia_type(vi)

  vi = vi %>% dplyr::mutate(
    var_org_type = varclass %>% change_val(c("fe", "character"), "factor"),
    var_reg_type = class %>% change_val(c("fe", "character"), "factor") %>% change_val("logical", "dummy"),
    ia_reg_type = ia_type %>%
      change_val("fe", "factor") %>%
      change_val("fe_numeric", "factor_numeric") %>%
      change_val("fe_logical", "factor_dummy")
  ) %>% dplyr::mutate(
    var_reg_type = ifelse(role == "cluster", "factor", var_reg_type),
    ia_reg_type = ifelse(role == "cluster", "factor", ia_reg_type)
  )

  # Ensure column order is clean
  vi = vi %>% dplyr::select(
    ia_expr, var_expr, var, role, prefix, option, class, fe_type, is_fe,
    distinct_num, ia_num, ia_pos, main_pos, ia_cterm, cterm, basevar, dplyr::everything()
  )

  return(vi)
}
```
!END_MODIFICATION cmdpart_to_regvar /home/rstudio/repbox/metaregBase/R/mrb_reg_tools.R


!MODIFICATION mrb_run_r_base_step /home/rstudio/repbox/metaregBase/R/mrb_r_base.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_base.R"
function_name = "mrb_run_r_base_step"
description = "Store weights string and detect weight expressions via fast regex in the reg parcel."
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

  w_df = cmdpart %>% filter(part == "weight_var")
  w_type_df = cmdpart %>% filter(part == "weight_type")
  weights_val = NA_character_

  if (nrow(w_df) > 0) {
    weights_val = w_df$content[1]
    w_type = if (nrow(w_type_df) > 0) tolower(w_type_df$content[1]) else ""
    
    if (w_type %in% c("fw", "pw", "iw")) {
      flags_vec = c(flags_vec, w_type)
    }
    
    # Fast regex check: If it contains anything non-alphanumeric, it is an expression.
    is_expr = stringi::stri_detect_regex(weights_val, "[^A-Za-z0-9_]")
    
    if (is_expr) {
      vars = try(all.vars(parse(text = weights_val)), silent = TRUE)
      if (inherits(vars, "try-error")) {
        flags_vec = c(flags_vec, "weights_non_parseable")
        repbox_problem(paste0("Cannot parse weight expression: ", weights_val), type = "weights_non_parseable", fail_action = "msg")
      } else {
        flags_vec = c(flags_vec, "weights_expr")
      }
    }
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
    weights = weights_val,
    flags = flags_str,
    error_in_r = FALSE
  )

  step_parcels$reg = reg_dat
  step_parcels$colinfo = repbox_compute_col_info(runid, project_dir, dat, org_dat, reg_dat)

  return(step_parcels)
}
```
!END_MODIFICATION mrb_run_r_base_step /home/rstudio/repbox/metaregBase/R/mrb_r_base.R


!MODIFICATION weights.R /home/rstudio/repbox/regtranslate/R/weights.R
scope = "file"
file = "/home/rstudio/repbox/regtranslate/R/weights.R"
description = "Create new centralized helper function to resolve weight arguments and data manipulation steps."
---
```r
#' Translate regression weight configurations into R syntax.
#'
#' Evaluates the `weights` and `flags` fields from a `reg` parcel and 
#' constructs two snippets of code: 
#' 1. `data_code`: A string setting up `.weight` if the weight is a mathematical expression.
#' 2. `weight_arg`: The exact string to inject into the `weights = ...` argument of the regression call.
#'
#' @param reg The single-row dataframe of the regression metadata parcel.
#' @param template A format string denoting how the final weight should be injected.
#'                 Use `"~ %s"` for fixest/formulas, or `"dat[['%s']]"` for lm/rq/tobit vectors.
#'
#' @return A list with `$data_code` and `$weight_arg`
r_weight_code = function(reg, template = "~ %s") {
  res = list(data_code = "", weight_arg = "")

  if (!"weights" %in% names(reg)) return(res)
  
  w_str = reg$weights
  if (is.na(w_str) || !nzchar(w_str)) return(res)

  flags = if (!is.null(reg$flags) && !is.na(reg$flags)) {
    strsplit(reg$flags, ",\\s*")[[1]]
  } else {
    character(0)
  }

  if ("weights_non_parseable" %in% flags) {
    res$weight_arg = "# Weights skipped: non-parseable expression"
    return(res)
  }

  if ("weights_expr" %in% flags) {
    res$data_code = paste0("dat[['.weight']] = with(dat, ", w_str, ")")
    res$weight_arg = paste0("weights = ", sprintf(template, ".weight"))
  } else {
    res$weight_arg = paste0("weights = ", sprintf(template, w_str))
  }
  
  return(res)
}
```
!END_MODIFICATION weights.R /home/rstudio/repbox/regtranslate/R/weights.R


!MODIFICATION stata_to_r_code_fixest /home/rstudio/repbox/regtranslate/R/to_r_fixest.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_fixest.R"
function_name = "stata_to_r_code_fixest"
description = "Use the centralized r_weight_code helper to handle weighted fixest regressions."
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


!MODIFICATION stata_to_r_code_lm /home/rstudio/repbox/regtranslate/R/to_r_lm.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_lm.R"
function_name = "stata_to_r_code_lm"
description = "Use the centralized r_weight_code helper to handle weighted lm regressions."
---
```r
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
!END_MODIFICATION stata_to_r_code_lm /home/rstudio/repbox/regtranslate/R/to_r_lm.R


!MODIFICATION stata_to_r_code_quantreg /home/rstudio/repbox/regtranslate/R/to_r_quantreg.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_quantreg.R"
function_name = "stata_to_r_code_quantreg"
description = "Use the centralized r_weight_code helper to handle weighted quantreg regressions."
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

  data_code = ""

  # Apply dynamic weights via centralized helper
  wt = r_weight_code(reg, template = "dat[['%s']]")
  if (nzchar(wt$data_code)) {
    data_code = wt$data_code
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
!END_MODIFICATION stata_to_r_code_quantreg /home/rstudio/repbox/regtranslate/R/to_r_quantreg.R


!MODIFICATION stata_to_r_code_tobit /home/rstudio/repbox/regtranslate/R/to_r_tobit.R
scope = "function"
file = "/home/rstudio/repbox/regtranslate/R/to_r_tobit.R"
function_name = "stata_to_r_code_tobit"
description = "Use the centralized r_weight_code helper to handle weighted tobit regressions."
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

  data_code = ""
  
  # Apply dynamic weights via centralized helper
  wt = r_weight_code(reg, template = "dat[['%s']]")
  if (nzchar(wt$data_code)) {
    data_code = wt$data_code
  }
  if (nzchar(wt$weight_arg)) {
    arg_str = c(arg_str, wt$weight_arg)
  }

  reg_code = paste0('reg = ', rcmd,'(', paste0(arg_str, collapse=","),")
class(reg) = 'survreg'")

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
!END_MODIFICATION stata_to_r_code_tobit /home/rstudio/repbox/regtranslate/R/to_r_tobit.R
