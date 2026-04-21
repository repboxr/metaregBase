Here is the complete implementation of the repair pipeline. 

It adds the new repair function `mrb_repair_failed_runs()`, updates the data loaders so they intercept and prioritize the newly generated Stata `.dta` caches (which bypasses any potentially problematic R data-prep/filtering steps), and tracks everything cleanly via partial-updates and the new `rb_repair_level` field in `regcheck`.

### 1. The Repair Function
We create the new repair script that generates Stata code to save the perfectly filtered regression data using `keep if e(sample)`.

!MODIFICATION mrb_repair.R
scope = "file"
file = "/home/rstudio/repbox/metaregBase/R/mrb_repair.R"
description = "New file with mrb_repair_failed_runs to generate Stata caches and re-run R replications."
---
```r
# FILE: /home/rstudio/repbox/metaregBase/R/mrb_repair.R

#' Automatically repair metaregBase R failures by fetching Stata's exact regression data.
#' 
#' Generates a Stata script for failed runs that executes the data-prep path, runs the 
#' regression quietly, keeps exactly the estimation sample `e(sample)`, and saves it. 
#' It then forces the R reproduction to load this exact cache, sidestepping R 
#' data translation bugs.
mrb_repair_failed_runs = function(mrb) {
  restore.point("mrb_repair_failed_runs")
  
  regcheck = mrb$parcels$regcheck
  if (is.null(regcheck)) {
    cat("\nNo regcheck parcel found. Run mrb_make_regcheck_parcel() first.\n")
    return(mrb)
  }
  
  # 1. Identify failed runs
  # Failure criteria: sb ran, but rb failed or coefficients mismatch
  failed_pids = regcheck$runid[regcheck$sb_did_run & (!regcheck$rb_did_run | !regcheck$rb_sb_coef_same)]
  
  if (length(failed_pids) == 0) {
    cat("\nNo failed runs to repair.\n")
    return(mrb)
  }
  
  cat("\nAttempting repair for runids: ", paste(failed_pids, collapse=", "), "\n")
  
  repair_dir = file.path(mrb$project_dir, "metareg/base/repair_cache")
  dir.create(repair_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 2. Generate Stata script to save e(sample) filtered caches
  sc_df = repboxDRF::drf_stata_code_df(mrb$drf, runids = failed_pids, path_merge = "none")
  
  for (pid in failed_pids) {
    row = which(sc_df$pid == pid & sc_df$is_target)
    if (length(row) > 0) {
      cache_file = file.path(repair_dir, paste0("repair_", pid, ".dta"))
      cmd = sc_df$code[row]
      
      # Replace the regression with a quiet execution, then keep if e(sample)
      new_code = paste0(
        "capture noisily quietly: ", cmd, "\n",
        "capture keep if e(sample)\n",
        "capture save \"", cache_file, "\", replace\n"
      )
      sc_df$code[row] = new_code
    }
  }
  
  script_file = file.path(mrb$project_dir, "metareg/base/stata_code/mrb_repair.do")
  drf_code_write(sc_df, script_file)
  
  cat("\nRunning Stata repair script...\n")
  mrb_run_stata_script(mrb, do_file = script_file)
  
  # 3. Set flags and run R pipelines
  # Using the repair cache bypasses drf_get_data and entirely skips R filtering.
  mrb$use_repair_cache = TRUE
  mrb$repair_level = 1
  
  # Rerun base to reconstruct regvar (in case xi variables were dumped into cache)
  mrb = mrb_run_r_base(mrb, just_pids = failed_pids)
  
  # Rerun reg
  mrb = mrb_run_r_reg(mrb, just_pids = failed_pids)
  
  # Re-evaluate regcheck for the whole dataset with the updated level
  mrb = mrb_make_regcheck_parcel(mrb)
  
  # Reset flags
  mrb$use_repair_cache = FALSE
  mrb$repair_level = 0
  
  return(mrb)
}
```
!END_MODIFICATION mrb_repair.R

### 2. Loading the Base Data directly
Instead of letting `mrb` fall through to standard `drf_get_data`, we create a helper to intercept requests when a repair cache is available.

!MODIFICATION mrb_load_base_data in metaregBase/R/mrb_r_base.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_base.R"
insert_before_fun = "mrb_run_r_base_step"
description = "Add helper to load base data, checking for repair cache first."
---
```r
mrb_load_base_data = function(runid, mrb) {
  if (isTRUE(mrb$use_repair_cache)) {
    cache_file = file.path(mrb$project_dir, "metareg/base/repair_cache", paste0("repair_", runid, ".dta"))
    if (file.exists(cache_file)) {
      return(haven::read_dta(cache_file))
    }
  }
  repboxDRF::drf_get_data(runid, drf = mrb$drf)
}
```
!END_MODIFICATION mrb_load_base_data in metaregBase/R/mrb_r_base.R

### 3. Integrating the Interceptor

!MODIFICATION mrb_run_r_base_step in metaregBase/R/mrb_r_base.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_base.R"
function_name = "mrb_run_r_base_step"
description = "Update to use mrb_load_base_data instead of drf_get_data directly."
---
```r
#' Process a single regression, expand syntax, and format standard parcels
mrb_run_r_base_step = function(mrb, pid) {
  restore.point("mrb_run_r_base_step")

  project_dir = mrb$project_dir
  runid = pid

  xtvar = mrb$parcels$xtvar
  if (!is.null(xtvar) && NROW(xtvar) > 0) {
    xtvar = xtvar[xtvar$runid==pid,]
  }
  if (is.null(xtvar) || NROW(xtvar)==0) {
    xtvar = list(timevar=NA, panelvar=NA, tdelta=NA_integer_)
  }

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

  # 3. Load Data & Expand Syntax
  dat = mrb_load_base_data(pid, mrb)
  org_dat = dat
  cmdpart = cmdpart_expand_vars(cmdpart, colnames(dat)) # From previous refactor

  # 4. Extract Options, SE, and build initial regvar
  opts_df = cmdpart_to_opts_df(cmdpart)
  se_info = se_stata_to_repdb(cmd, opts_df)
  regvar = cmdpart_to_regvar(cmdpart, dat, opts_df, se_info)

  depvar = regvar$cterm[regvar$role == "dep"]

  # 5. Data Mutations & Stats
  ct_cterms = unique(c(depvar, regvar$var, regvar$cterm, regvar$ia_cterm)) %>% setdiff(c("(Intercept)",""))

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

  # A. REGCOEF (Parsed Stata Coefficients from metaregBase 'sb' run)
  if (!is.null(stata_ct) && nrow(stata_ct) > 0) {
    step_parcels$regcoef = ct_to_regcoef(stata_ct, variant = "sb", artid = mrb$artid)
    regcoef_main = regcoef_keep_default_eq(step_parcels$regcoef)
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
  } else { character(0) }

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
  } else { tibble() }

  step_parcels$colstat_dummy = if (nrow(colstats$colstat_dummy) > 0) {
    colstats$colstat_dummy %>% mutate(artid = mrb$artid, variant = "sb", runid = runid, cterm = col)
  } else { tibble() }

  step_parcels$colstat_factor = if (nrow(colstats$colstat_factor) > 0) {
    colstats$colstat_factor %>% mutate(artid = mrb$artid, variant = "sb", runid = runid, cterm = col)
  } else { tibble() }

  # F. REG & REGSOURCE
  nobs_val = if ("N" %in% names(stats_wide)) as.numeric(stats_wide$N) else NA_real_
  r2_val = if ("r2" %in% names(stats_wide)) as.numeric(stats_wide$r2) else if ("r2_p" %in% names(stats_wide)) as.numeric(stats_wide$r2_p) else NA_real_


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
    error_in_r = FALSE
  )

  step_parcels$reg = reg_dat
  step_parcels$colinfo = repbox_compute_col_info(runid, project_dir, dat, org_dat, reg_dat)

  return(step_parcels)
}
```
!END_MODIFICATION mrb_run_r_base_step in metaregBase/R/mrb_r_base.R

!MODIFICATION mrb_get_regression_data in metaregBase/R/mrb_r_reg.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_reg.R"
function_name = "mrb_get_regression_data"
description = "Update to accept the mrb object directly so we can check for our injected interceptor dataset."
---
```r
#' Get and prepare regression data (creates cterms and regxvar columns)
mrb_get_regression_data = function(runid, mrb, reg=NULL, regvar, regxvar = NULL) {
  restore.point("mrb_get_regression_data")

  dat = mrb_load_base_data(runid, mrb)

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

  return(dat)
}
```
!END_MODIFICATION mrb_get_regression_data in metaregBase/R/mrb_r_reg.R

!MODIFICATION mrb_run_r_reg_step in metaregBase/R/mrb_r_reg.R
scope = "function"
file = "/home/rstudio/repbox/metaregBase/R/mrb_r_reg.R"
function_name = "mrb_run_r_reg_step"
description = "Pass mrb downward instead of drf to mrb_get_regression_data."
---
```r
#' Process a single regression, expand syntax, and format standard parcels
mrb_run_r_reg_step = function(mrb, pid) {
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
  dat = try(mrb_get_regression_data(runid, mrb = mrb, reg=reg, regvar = regvar, regxvar = regxvar), silent=TRUE)
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
    step_parcels$regcoef_rb = co_df

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
!END_MODIFICATION mrb_run_r_reg_step in metaregBase/R/mrb_r_reg.R

### 4. Create the comprehensive `regcheck` parcel. 
It cleanly maps out errors or deviations and increments `rb_repair_level` during partial repair runs.

!MODIFICATION mrb_regcheck.R
scope = "file"
file = "/home/rstudio/repbox/metaregBase/R/mrb_regcheck.R"
description = "Create regcheck parcel with partial updates and rb_repair_level tracking."
---
```r
# FILE: /home/rstudio/repbox/metaregBase/R/mrb_regcheck.R

#' Assemble the 'regcheck' parcel checking cross-language replication success
#' 
#' Evaluates the success of regression outputs and maps any mismatches
#' to a standardized `regcheck` parcel. 
mrb_make_regcheck_parcel = function(mrb, save = TRUE) {
  restore.point("mrb_make_regcheck_parcel")

  parcels = mrb$parcels
  need = c("reg", "reg_rb", "regcoef", "regcoef_so", "regcoef_rb")
  missing = setdiff(need, names(parcels))
  if (length(missing) > 0) {
    parcels = repboxDB::repdb_load_parcels(mrb$project_dir, missing, parcels = parcels)
    mrb$parcels = parcels
  }

  if (isTRUE(mrb$is_partial_run)) {
    pids = mrb$partial_pids
  } else {
    pids = unique(c(
      if (!is.null(parcels$reg)) parcels$reg$runid else integer(),
      if (!is.null(parcels$reg_rb)) parcels$reg_rb$runid else integer(),
      if (!is.null(parcels$regcoef_so)) parcels$regcoef_so$runid else integer(),
      if (!is.null(mrb$drf$pids)) mrb$drf$pids else integer()
    ))
  }

  if (length(pids) == 0) return(mrb)

  repair_level = if (!is.null(mrb$repair_level)) as.integer(mrb$repair_level) else 0L

  res_li = lapply(pids, function(pid) {
    so_did_run = !is.null(parcels$regcoef_so) && pid %in% parcels$regcoef_so$runid
    sb_did_run = !is.null(parcels$reg) && pid %in% parcels$reg$runid
    
    rb_did_run = FALSE
    error_msg = ""
    if (!is.null(parcels$reg_rb) && pid %in% parcels$reg_rb$runid) {
      row = parcels$reg_rb[parcels$reg_rb$runid == pid, , drop = FALSE][1,]
      rb_did_run = !isTRUE(row$error_in_r)
      error_msg = if (!is.na(row$error_msg)) row$error_msg else ""
    }

    sb_so_identical = NA
    rb_sb_coef_same = NA
    rb_sb_coef_max_dev = NA_real_
    rb_sb_se_same = NA
    rb_sb_se_max_dev = NA_real_
    problem = ""
    comment = ""

    if (sb_did_run && so_did_run) {
      co_sb = parcels$regcoef[parcels$regcoef$runid == pid, , drop = FALSE]
      co_so = parcels$regcoef_so[parcels$regcoef_so$runid == pid, , drop = FALSE]
      diff_so = coef_diff_table(co_sb, co_so)
      if (!is.null(diff_so) && NROW(diff_so) > 0) {
        sb_so_identical = all(diff_so$identical, na.rm = TRUE)
      } else {
        sb_so_identical = FALSE
      }
    }

    if (sb_did_run && rb_did_run) {
      co_sb = parcels$regcoef[parcels$regcoef$runid == pid, , drop = FALSE]
      co_rb = parcels$regcoef_rb[parcels$regcoef_rb$runid == pid, , drop = FALSE]
      diff_rb = coef_diff_table(co_sb, co_rb)
      if (!is.null(diff_rb) && NROW(diff_rb) > 0) {
        rb_sb_coef_max_dev = max_empty_na(diff_rb$rel_err_coef, na.rm = TRUE)
        rb_sb_se_max_dev = max_empty_na(diff_rb$rel_err_se, na.rm = TRUE)
        rb_sb_coef_same = isTRUE(rb_sb_coef_max_dev <= 0.01)
        rb_sb_se_same = isTRUE(rb_sb_se_max_dev <= 0.01)
      } else {
        rb_sb_coef_same = FALSE
        rb_sb_se_same = FALSE
      }
    }

    if (!rb_did_run) {
      problem = paste0("R replication failed: ", error_msg)
    } else if (!sb_did_run) {
      problem = "Stata base replication failed."
    } else if (!so_did_run) {
      problem = "Original Stata reproduction results missing."
    } else if (isTRUE(!rb_sb_coef_same)) {
      problem = "R and Stata base coefficients differ by > 1%."
    } else if (isTRUE(!sb_so_identical)) {
      problem = "Stata base differs from Stata original."
    }

    reg_ok = isTRUE(so_did_run) && isTRUE(sb_did_run) && isTRUE(rb_did_run) &&
             isTRUE(sb_so_identical) && isTRUE(rb_sb_coef_same) && isTRUE(rb_sb_se_same)

    dplyr::tibble(
      runid = as.integer(pid),
      reg_ok = reg_ok,
      so_did_run = so_did_run,
      sb_did_run = sb_did_run,
      rb_did_run = rb_did_run,
      sb_so_identical = sb_so_identical,
      rb_sb_coef_same = rb_sb_coef_same,
      rb_sb_coef_max_dev = rb_sb_coef_max_dev,
      rb_sb_se_same = rb_sb_se_same,
      rb_sb_se_max_dev = rb_sb_se_max_dev,
      rb_repair_level = repair_level,
      problem = problem,
      comment = comment
    )
  })

  new_regcheck = dplyr::bind_rows(res_li)

  if (isTRUE(mrb$is_partial_run) && !is.null(parcels$regcheck)) {
    old_regcheck = parcels$regcheck
    old_kept = old_regcheck[!old_regcheck$runid %in% pids, , drop = FALSE]
    regcheck = dplyr::bind_rows(old_kept, new_regcheck)
  } else {
    regcheck = new_regcheck
  }

  if (save) {
    repboxDB::repdb_save_parcels(list(regcheck = regcheck), file.path(mrb$project_dir, "repdb"), check = FALSE)
  }
  
  mrb$parcels$regcheck = regcheck
  return(mrb)
}
```
!END_MODIFICATION mrb_regcheck.R
