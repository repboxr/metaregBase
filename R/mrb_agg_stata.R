# TO DO: Transform to parcels
mrb_agg_stata = function(mrb, skip_if_has = TRUE) {
  restore.point("mrb_agg_stata")
  if (skip_if_has & !is.null(mrb[["stata_ct_sb"]]))
    return(mrb)
  mrb$stata_ct_sb = mrb_agg_stata_regcoef(mrb)
  mrb$stata_scalars = mrb_agg_stata_reg_scalars(mrb)
  mrb$stata_macros = mrb_agg_stata_reg_macros(mrb)
  mrb$stata_ct_dprobit = mrb_agg_add_dprobit_coef(mrb,mrb$stata_ct_sb)
  mrb
}

mrb_run_r_base_step = function(mrb, pid) {
  restore.point("mrb_run_r_base_step")

  project_dir = mrb$project_dir
  runid = pid

  xtvar = mrb$parcels$xtvar
  xtvar = xtvar[xtvar$runid==pid,]
  if (NROW(xtvar)==0) {
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
  dat = repboxDRF::drf_get_data(pid, drf = mrb$drf)
  org_dat = dat
  cmdpart = cmdpart_expand_vars(cmdpart, colnames(dat)) # From previous refactor

  # 4. Extract Options, SE, and build initial regvar
  opts_df = cmdpart_to_opts_df(cmdpart)
  se_info = se_stata_to_repdb(cmd, opts_df)
  regvar = cmdpart_to_regvar(cmdpart, dat, opts_df, se_info)

  depvar = regvar$cterm[regvar$role == "dep"]

  # 5. Data Mutations & Stats
  ct_cterms = unique(c(depvar, regvar$var, regvar$cterm, regvar$ia_cterm)) %>% setdiff(c("(Intercept)",""))

  # NEW: Keep the full expanded dataset so make_regxvar can access generated time-series columns!
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
  # NEW: Pass wide_dat_full instead of dat!
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




# Aggregates regression statistics stored with svret
mrb_agg_stata_reg_scalars = function(mrb, file_prefix="regscalar_", dir = file.path(mrb$mrb_dir, "stata_reg_out")) {
  restore.point("mr_agg_stata_reg_scalars")
  glob = paste0(file_prefix, "*",".txt")
  files = list.files(dir, glob2rx(glob), full.names=TRUE)
  if (length(files)==0) return(NULL)

  li = lapply(files, function(file) {
    df = read_var_equal_val_file(file,as.numeric = TRUE)
    base = basename(file)
    if (!is.null(df)) {
      runid = as_integer(str.between(base, file_prefix, "__"))
      variant = str.between(base, "__", ".txt")
      df$runid = rep(runid, NROW(df))
      df$variant = rep(variant, NROW(df))
    }
    return(df)
  })
  res = bind_rows(li)
  res
}

# Aggregates regression statistics stored with svret
mrb_agg_stata_reg_macros = function(mrb, file_prefix="regmacro_", dir = file.path(mrb$mrb_dir, "stata_reg_out")) {
  restore.point("mr_agg_stata_reg_macros")
  glob = paste0(file_prefix, "*",".txt")
  files = list.files(dir, glob2rx(glob), full.names=TRUE)
  if (length(files)==0) return(NULL)

  li = lapply(files, function(file) {
    df = read_var_equal_val_file(file,as.numeric = FALSE)
    base = basename(file)
    if (!is.null(df)) {
      runid = as_integer(str.between(base, file_prefix, "__"))
      variant = str.between(base, "__", ".txt")
      df$runid = rep(runid, NROW(df))
      df$variant = rep(variant, NROW(df))
    }
    return(df)
  })
  res = bind_rows(li)
  res
}

# Extract marginal effects for dprobit commands
mrb_agg_add_dprobit_coef = function(mrb, stata_ct, dir = file.path(mrb$mrb_dir, "stata_reg_out")) {
  restore.point("mr_agg_add_dprobit_coef")
  glob = paste0("dprobit_", "*",".csv")
  files = list.files(dir, glob2rx(glob), full.names=TRUE)
  if (length(files)==0) return(stata_ct)

  df_list = lapply(files, function(file) {
    df = read.csv(file)
    base = basename(file)
    if (!is.null(df)) {
      runid = as_integer(str.between(base, "dprobit_", ".csv"))
      df$runid = rep(runid, NROW(df))
      df$variant = rep("sb_mfx", NROW(df))
    }
    return(df)
  })
  df = bind_rows(df_list)
  if (nrow(df) == 0) return(stata_ct)

  df$t = df$coef / df$se
  df$ci_low = df$ci_up = NA_real_
  df$cmd = "dprobit"

  # p-value is the same as for the original coefficient
  if (!is.null(stata_ct) && nrow(stata_ct) > 0) {
    df = left_join(df, stata_ct %>% filter(variant=="sb") %>% select(runid, var,p,label), by=c("runid","var"))
  } else {
    df$p = NA_real_
    df$label = ""
  }

  return(df)
}

read_var_equal_val_file = function(file, as.numeric=FALSE, wide = FALSE) {
  restore.point("read_var_equal_file")
  txt = readLines(file)
  if (length(txt) == 0) return(tibble())

  pos = stringi::stri_locate_first_fixed(txt,"=")[,1]

  var = stringi::stri_sub(txt,1,pos-1)
  val = stringi::stri_sub(txt,pos+1)
  if (as.numeric) {
    val = suppressWarnings(as.numeric(val))
  }

  if (wide) {
    li = as.list(val)
    names(li) = var
    res = as_tibble(li)
  } else {
    res = tibble(
      var = var,
      val = val
    )
  }

  res
}

mrb_agg_stata_regcoef = function(mrb, file_prefix="reg_", dir = file.path(mrb$mrb_dir, "stata_reg_out")) {
  restore.point("mrb_agg_stata_regcoef")
  glob = paste0(file_prefix, "*",".dta")
  run_df = mrb$drf$run_df
  files = list.files(dir, glob2rx(glob), full.names=TRUE)
  if (length(files)==0) return(NULL)

  file = files[1]
  old.cols = c("eq","parm","label","estimate","stderr","dof", "z","p","min95","max95")
  new.cols = c("eq","var","label", "coef","se","dof", "t","p","ci_low","ci_up")

  li = lapply(files, function(file) {
    restore.point("kahkdhskdhk")
    df = haven::read_dta(file)
    df = rename.cols(df, old.cols, new.cols)
    df = df[,intersect(new.cols, colnames(df)), drop=FALSE]
    if (!"eq" %in% colnames(df)) {
      df$eq = rep("", NROW(df))
    }
    base = basename(file)
    if (!is.null(df)) {
      has.variant = has.substr(base,"__")
      if (has.variant) {
        runid = as_integer(str.between(base, file_prefix, "__"))
        variant = str.between(base, "__", ".dta")
      } else {
        runid = as_integer(str.between(base, file_prefix, ".dta"))
        variant = ""
      }
      df$runid = rep(runid, NROW(df))
      df$variant = rep(variant, NROW(df))
      df$cmd = run_df$cmd[runid]
    }
    return(df)
  })
  res = bind_rows(li)
  res
}

