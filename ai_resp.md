Based on your description, the likely culprit is an issue with the order of operations concerning time-series operators (lags and leads like `L1.` and `F1.`) and `if`/`in` filter conditions (like `if year>1978`).

### The Root Cause
In Stata, when you run a regression with time-series operators and an `if` condition:
```stata
xi: xtivreg2 cashdef (F1.bailind=gbailind L1.gbailind) taxbt ... if year>1978, fe cl(code)
```
Stata first evaluates the lags (`L1.gbailind`) and leads (`F1.bailind`) on the **entire contiguous panel dataset**, and only *then* applies the `if year>1978` filter to restrict the estimation sample. This ensures that the 1979 observation correctly looks back at the 1978 data to fetch its lag.

In the `metaregBase` R translation pipeline, `repboxDRF::drf_get_data` is called to fetch the dataset for the regression. By default, this function returns the data **after** the regression `if` filter has already been applied. As a result:
1. `mrb_get_regression_data` and `mrb_run_r_base_step` receive a dataset where 1978 and earlier years are completely missing.
2. The pipeline then calls `create_cterm_cols` (which internally uses `collapse::flag`) to compute `L1.` and `F1.`.
3. For the 1979 row, `collapse::flag` attempts to look up the 1978 lag, but because the 1978 data was filtered out, it evaluates to `NA`.
4. Finally, `fixest::feols` drops the 1979 observations entirely due to missing values (listwise deletion).

This completely changes the estimation sample and causes the coefficients in R to differ substantially from those in Stata.

### The Fix
To solve this, we must fetch the **unfiltered** data (`filtered = FALSE`), create the TS columns so they can access prior/future periods, and only *then* manually apply the regression filter using `repboxDRF::drf_get_filter_code`. 

Below are the modifications to correctly sequence the filtering logic in the pipeline.

!MODIFICATION mrb_run_r_base_step metaregBase/R/mrb_r_base.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_base.R"
function_name = "mrb_run_r_base_step"
description = "Fetch unfiltered data, evaluate TS operators, and apply filter subsequently."
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
  # UNFILTERED: Retrieve data before regression filters are applied so TS operators (lags/leads) can be computed correctly.
  dat = repboxDRF::drf_get_data(pid, drf = mrb$drf, filtered = FALSE, continue_on_error = continue_on_error)

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


  cmdpart = cmdpart_expand_vars(cmdpart, colnames(dat))

  # 4. Extract Options, SE, and build initial regvar
  opts_df = cmdpart_to_opts_df(cmdpart)

  panelvar = mrb_get_panelvar(run_obj, opts_df, xtvar)
  se_info = se_stata_to_repdb(cmd, opts_df, panelvar = panelvar)

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
  
  # NOW apply the regression filter to ensure TS operator values are matched to the correct estimation sample.
  filter_code = repboxDRF::drf_get_filter_code(pid, mrb$drf, parcels = mrb$parcels)
  if (length(filter_code) > 0 && any(nzchar(filter_code))) {
    for (code in filter_code) {
      if (nzchar(code)) {
        dat = wide_dat_full
        eval(parse(text = code))
        wide_dat_full = dat
      }
    }
  }

  dat = wide_dat_full
  org_dat = wide_dat_full
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
!END_MODIFICATION mrb_run_r_base_step metaregBase/R/mrb_r_base.R

!MODIFICATION mrb_get_regression_data metaregBase/R/mrb_r_reg.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_reg.R"
function_name = "mrb_get_regression_data"
description = "Update mrb_get_regression_data to fetch unfiltered data, run TS operators, and apply filter via code."
---
```r
#' Get and prepare regression data (creates cterms and regxvar columns)
mrb_get_regression_data = function(runid, drf, reg=NULL, regvar, regxvar = NULL, continue_on_error=FALSE, parcels=NULL) {
  restore.point("mrb_get_regression_data")

  # Fetch UNFILTERED data to correctly evaluate time-series lags and leads
  dat = repboxDRF::drf_get_data(runid, drf = drf, filtered = FALSE, continue_on_error = continue_on_error)

  # Extract panel/time variables if available
  timevar = NA; panelvar = NA; tdelta = NA
  if (!is.null(reg) && nrow(reg) > 0) {
    timevar = reg$timevar[1]
    panelvar = reg$panelvar[1]
    tdelta = reg$tdelta[1]
  }

  if (!is.null(regvar) && nrow(regvar) > 0) {
    dat = create_cterm_cols(dat, unique(regvar$cterm),timevar = timevar,panelvar = panelvar, tdelta = tdelta)
  }

  if (!is.null(regxvar) && nrow(regxvar) > 0) {
    dat = make_regxvar_cols(dat, regxvar)
  }

  # Apply the regression filter AFTER generating the time-series variables
  if (!is.null(parcels)) {
    filter_code = repboxDRF::drf_get_filter_code(runid, drf, parcels = parcels)
    if (length(filter_code) > 0 && any(nzchar(filter_code))) {
      for (code in filter_code) {
        if (nzchar(code)) {
          eval(parse(text = code))
        }
      }
    }
  }

  return(dat)
}
```
!END_MODIFICATION mrb_get_regression_data metaregBase/R/mrb_r_reg.R

!MODIFICATION mrb_run_r_reg_step metaregBase/R/mrb_r_reg.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_reg.R"
function_name = "mrb_run_r_reg_step"
description = "Pass 'parcels' down to mrb_get_regression_data so it can access filter code dependencies."
---
```r
#' Process a single regression, expand syntax, and format standard parcels
mrb_run_r_reg_step = function(mrb, pid, continue_on_error=FALSE) {
  restore.point("mrb_run_r_reg_step")

  project_dir = mrb$project_dir
  parcels = mrb$parcels
  artid = mrb$artid
  runid = pid

  reg = parcel_for_runid(parcels$reg, runid)
  regvar = parcel_for_runid(parcels$regvar, runid)
  regxvar = if (!is.null(parcels$regxvar)) parcel_for_runid(parcels$regxvar, runid) else tibble()
  cmdpart = parcel_for_runid(parcels$reg_cmdpart, runid)

  stata_co = parcel_for_runid(parcels$regcoef, runid)
  stata_co = stata_co[stata_co$variant == "sb", , drop = FALSE]
  stata_so = parcel_for_runid(parcels$regcoef_so, runid)
  default_eq = regcoef_default_eq(stata_co)

  step_parcels = list()

  diff_sb_so = NULL
  if (NROW(stata_co) > 0 && NROW(stata_so) > 0) {
     if (!"variant" %in% names(stata_co)) stata_co$variant = "sb"
     if (!"variant" %in% names(stata_so)) stata_so$variant = "so"
     diff_tab = coef_diff_table(stata_co, stata_so)
     if (!is.null(diff_tab)) {
        diff_sb_so = coef_diff_summary(diff_tab, compare_what=c("all","coef"))
     }
  }

  if (NROW(regvar) == 0 || NROW(cmdpart) == 0) {
    # Cannot translate if base parcels are empty, but we must return whatever diff we have
    step_parcels$regcoef_diff = diff_sb_so
    return(step_parcels)
  }

  library(regtranslate)
  opts = code_options(add_function = TRUE, add_broom = TRUE)
  code_df = try(reg_stata_to_r_code(reg, regvar, regxvar, cmdpart, prefer="fixest", opts=opts), silent=TRUE)

  reg_rb = reg %>% mutate(variant = "rb", error_in_r = FALSE, error_msg = "")

  if (is(code_df, "try-error") || any(grepl("# Stata command .* not fully translated", code_df$code)) || any(grepl("# Translation failed", code_df$code))) {
     reg_rb$error_in_r = TRUE
     reg_rb$error_msg = "Stata regression could not be translated to R."
     step_parcels$reg_rb = reg_rb
     step_parcels$regcoef_diff = diff_sb_so
     return(step_parcels)
  }

  code = paste0(code_df$code, collapse="\n")
  reg_fun_code = paste0("reg_fun = ", code)

  reg_fun = try(eval(parse(text=reg_fun_code)), silent=TRUE)
  if (is(reg_fun, "try-error")) {
     reg_rb$error_in_r = TRUE
     reg_rb$error_msg = "Error parsing translated R code."
     step_parcels$reg_rb = reg_rb
     step_parcels$regcoef_diff = diff_sb_so
     return(step_parcels)
  }

  # Fetch and prepare the data using our new refactored helper function
  dat = try(mrb_get_regression_data(runid, drf = mrb$drf, reg=reg, regvar = regvar, regxvar = regxvar, continue_on_error=continue_on_error, parcels=parcels), silent=TRUE)
  if (is(dat, "try-error")) {
     reg_rb$error_in_r = TRUE
     reg_rb$error_msg = "Error preparing regression data."
     step_parcels$reg_rb = reg_rb
     step_parcels$regcoef_diff = diff_sb_so
     return(step_parcels)
  }

  results = try(reg_fun(dat), silent=TRUE)
  if (is(results, "try-error")) {
     reg_rb$error_in_r = TRUE
     reg_rb$error_msg = as.character(attr(results, "condition")$message)
     step_parcels$reg_rb = reg_rb
     step_parcels$regcoef_diff = diff_sb_so
     return(step_parcels)
  }


  # Process R results
  ct = results$ct
  diff_sb_rb = NULL

  if (!is.null(ct) && nrow(ct) > 0) {
    ct$cterm = cterm_of_r_coefs(ct$term, regvar, dot_to_at = TRUE)

    co_df = ct_to_regcoef(ct, lang="r", variant="rb", artid=artid, default_eq=default_eq)
    co_df$runid = runid

    co_parcels = regcoef_split_variant_parcels(
      co_df,
      base_variant = "rb",
      base_parcel = "regcoef_rb"
    )
    step_parcels[names(co_parcels)] = co_parcels

    # Comparison sb vs rb
    if (!is.null(stata_co) && nrow(stata_co) > 0) {
      if (!"variant" %in% names(stata_co)) stata_co$variant = "sb"
      diff_tab_rb = coef_diff_table(stata_co, co_df)
      if (!is.null(diff_tab_rb)) {
        diff_sb_rb = coef_diff_summary(diff_tab_rb, compare_what=c("all","coef"))
      }
    }
  }

  step_parcels$regcoef_diff = dplyr::bind_rows(diff_sb_so, diff_sb_rb)

  glance = results$glance

  if (!is.null(glance)) {
    glance$runid = runid
    glance$variant = "r"
    glance$artid = artid

    res_scalars = mrb_stats_to_regscalar_regstring(glance, variant="rb")
    if (!is.null(res_scalars$regscalar)) {
      step_parcels$regscalar_rb = res_scalars$regscalar
    }
    if (!is.null(res_scalars$regstring)) {
      step_parcels$regstring_rb = res_scalars$regstring
    }

    # Manual extraction of stats for reg table update
    stats = glance
    if ("r.squared" %in% names(stats)) stats$r2 = as.numeric(stats$r.squared)
    if ("adj.r.squared" %in% names(stats)) stats$adj_r2 = as.numeric(stats$adj.r.squared)
    if ("df.residual" %in% names(stats)) stats$df_r = as.numeric(stats$df.residual)
    if ("statistic" %in% names(stats)) stats$F = as.numeric(stats$statistic)

    stat_cols = intersect(c("r2", "adj_r2", "df_r", "F"), names(stats))
    for(col in stat_cols) {
      reg_rb[[col]] = stats[[col]]
    }
  }
  reg_rb$cmd = results$rcmd
  reg_rb$lang = "r"

  step_parcels$reg_rb = reg_rb

  return(step_parcels)
}
```
!END_MODIFICATION mrb_run_r_reg_step metaregBase/R/mrb_r_reg.R

!MODIFICATION mrb_test_data_preview_text metaregBase/R/mrb_test_data.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_test_data.R"
function_name = "mrb_test_data_preview_text"
description = "Pass 'parcels' down to mrb_get_regression_data."
---
```r
mrb_test_data_preview_text = function(runid, drf, parcels, n = 5, opts=mrb_test_opts()) {
  restore.point("mrb_test_data_preview_text")

  if (!opts$show_org_data & !opts$show_reg_data) return("")

  # Get the path for this runid to find the first step (the data load state)
  path_df = drf$path_df %>% filter(pid == !!runid, runid <= !!runid) %>% arrange(runid)
  if (NROW(path_df) == 0) return("")

  first_runid = path_df$runid[1]

  format_df_sample = function(df, title, put_reg_cols_first = TRUE) {
    restore.point("format_df_sample")
    if (inherits(df, "try-error") || is.null(df)) return(paste0("Could not load ", title, "."))
    if (NROW(df) == 0) return(paste0(title, " is empty."))

    # Put regression columns first
    if (put_reg_cols_first && NROW(regvar) > 0) {
      cols = unique(c(regvar$basevar, regvar$cterm, regxvar$cterm))
      cols = intersect(cols, colnames(df))
      df = df[, union(cols, names(df))]
    }

    # Using tibble prints nicely across terminal widths truncating extra cols safely
    df_tibble = tibble::as_tibble(df)


    make_txt = function(df) {
      w = getOption("width")
      options(width=opts$data_width)
      txt = paste0(capture.output(print(df,n = Inf, width=opts$data_width)), collapse = "\n")
      options(width=w)
      txt
    }


    out_head = out_tail = ""

    if (opts$data_head_rows + opts$data_tail_rows >= NROW(df_tibble)) {
      txt = paste0("##", title, " (complete)\n```\n", make_txt(df_tibble),"\n```")
    } else if (opts$data_head_rows>0 & opts$data_tail_rows > 0) {
      txt = paste0("##", title, " (head & tail)\n```\n",
        make_txt(head(df_tibble, opts$data_head_rows)), "\n...", NROW(df_tibble)-opts$data_head_rows-opts$data_tail_rows, " rows ommited ...\n",make_txt(tail(df_tibble, opts$data_tail_rows)),
        "\n```")
    } else if (opts$data_head_rows>0) {
      txt = paste0("##", title, " (head)\n```\n", make_txt(head(df_tibble, opts$data_head_rows)),"\n```")
    } else if (opts$data_tail_rows>0) {
      txt = paste0("##", title, " (tail)\n```\n", make_txt(tail(df_tibble, opts$data_tail_rows)),"\n```")
    } else {
      txt = "opts$data_head_rows=0 and opts$data_tail_rows=0, so no rows shown"
    }

    txt
  }
  if (opts$data_add_org_row) {
    org_row_opt = repboxDRF::drf_set_add_org_row(TRUE)
  }

  res = ""
  if (opts$show_reg_data) {
    # Fetch Regression Data
    regvar = parcel_for_runid(parcels$regvar, runid)
    regxvar = if (!is.null(parcels$regxvar)) parcel_for_runid(parcels$regxvar, runid) else tibble()
    reg = parcel_for_runid(parcels$reg, runid)
    dat = try(mrb_get_regression_data(runid, drf, reg=reg, regvar=regvar, regxvar=regxvar, parcels=parcels), silent = TRUE)
    res = c(res, format_df_sample(dat, "## Regression Data (fully prepared)", put_reg_cols_first = TRUE), "")
  }
  if (opts$show_pre_reg_data) {
    dat = try(repboxDRF::drf_get_data(runid=runid, drf = drf, before=TRUE,filtered = TRUE), silent = TRUE)
    res = c(res, format_df_sample(dat, "## Data before regression command (filtered)", put_reg_cols_first = TRUE), "")
  }

  if (opts$show_org_data) {
    # 1. Fetch Original Data
    dat = try(repboxDRF::drf_get_data(first_runid, drf = drf, before=FALSE), silent = TRUE)
    res = c(res, format_df_sample(dat, "## Original Data (First Step)"), "")
  }

  if (opts$data_add_org_row) {
    repboxDRF::drf_set_add_org_row(org_row_opt)
  }
  paste0(res, collapse = "\n")
}
```
!END_MODIFICATION mrb_test_data_preview_text metaregBase/R/mrb_test_data.R

!MODIFICATION mrb_test_reg_data_prep_code metaregBase/R/mrb_test_code_path.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_test_code_path.R"
function_name = "mrb_test_reg_data_prep_code"
description = "Pass 'parcels' down to mrb_get_regression_data to match new signature."
---
```r
mrb_test_reg_data_prep_code = function(project_dir, runid, parcels = list()) {
  restore.point("mrb_test_reg_data_prep_code")

  need = c("reg", "regvar", "regxvar")
  missing = need[!need %in% names(parcels)]

  load_call = if (length(missing) == 0) {
    'parcels = parcels'
  } else {
    paste0(
      'parcels = repboxDB::repdb_load_parcels(project_dir, c(',
      paste0('"', missing, '"', collapse = ", "),
      '), parcels = parcels)'
    )
  }

  lines = c(
    paste0("runid = ", runid),
    "if (!exists(\"parcels\")) parcels = list()",
    load_call,
    "drf = repboxDRF::drf_load(project_dir, parcels = parcels)",
    "reg = parcels$reg[parcels$reg$runid == runid, , drop = FALSE]",
    "regvar = parcels$regvar[parcels$regvar$runid == runid, , drop = FALSE]",
    "regxvar = if (!is.null(parcels$regxvar)) parcels$regxvar[parcels$regxvar$runid == runid, , drop = FALSE] else tibble::tibble()",
    "",
    "# dat is the regression-ready data, including the DRF path, filtering,",
    "# generated cterm columns, and regxvar columns",
    "dat = metaregBase:::mrb_get_regression_data(runid = runid, drf = drf, reg = reg, regvar = regvar, regxvar = regxvar, parcels = parcels)"
  )

  paste0(lines, collapse = "\n")
}
```
!END_MODIFICATION mrb_test_reg_data_prep_code metaregBase/R/mrb_test_code_path.R
