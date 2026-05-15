# metaregBase R run that
# aggregates all information from Stata base run
# and creates corresponding parcels
# regvar, regxvar, reg, colinfo, colstat_???, etc.
#
# The functions here do not yet run the regressions in R.
# that will be done in mrb_r_reg.R
example = function() {
  library(metaregBase)
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"
  project_dir = "~/repbox/projects/test"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  drf = drf_load(project_dir)
  mrb = mrb_init(project_dir, drf=drf)
  #mrb = mrb_full_stata_script(mrb)
  #mrb = mrb_run_stata_script(mrb)
  mrb = mrb_agg_stata(mrb)
  mrb = mrb_run_r_base(mrb, just_pid = drf$pid[1])
}


#' Process a single regression, expand syntax, and format standard parcels
mrb_run_r_base_step = function(mrb, pid, with_try = isTRUE(mrb$with_try)) {
  restore.point("mrb_run_r_base_step")
  if (with_try) {
    restore.point("mrb_run_r_base_step_with_try")
    res = repboxUtils::try_catch_repbox_problems(mrb_run_r_base_step(mrb,pid, with_try=FALSE),project_dir = mrb$project_dir,runid = pid,msg_prefix = "mrb_run_r_base_step: ", err_val=NULL)
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
  dat = repboxDRF::drf_get_data(pid, drf = mrb$drf)

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

#' Extract Stata metaregBase results and create corresponding metaregBase parcels
mrb_run_r_base = function(mrb, just_pids=NULL, make_parcels=TRUE) {
  restore.point("mrb_run_r")

  mrb$artid = basename(mrb$project_dir)
  mrb$parcels = repboxDB::repdb_load_parcels(mrb$project_dir, c("reg_cmdpart", "xtvar"))

  pids = mrb$drf$pids
  if (length(pids) == 0) {
    cat("\nNo pids to process.\n")
    return(mrb)
  }

  all_pids = pids
  if (!is.null(just_pids)) {
    pids = just_pids
    mrb$is_partial_run = TRUE
    mrb$partial_pids = just_pids
  } else {
    mrb$is_partial_run = FALSE
  }

  mrb = mrb_agg_stata(mrb, skip_if_has = TRUE)

  all_step_parcels = list()

  cat("\nmrb_r_base processing runids: ")
  for (pid in pids) {
    cat(paste0(pid," "))
    step_parcels = mrb_run_r_base_step(mrb, pid)
    all_step_parcels[[as.character(pid)]] = step_parcels
  }
  cat("\n")

  mrb$all_step_parcels = all_step_parcels
  if (make_parcels) {
    mrb = mrb_make_r_base_parcels(mrb)
  }

  mrb
}

# The step parcels are generated in mrb_r
# The step parcels are generated in mrb_r
mrb_make_r_base_parcels = function(mrb, save=TRUE, is_partial_run = isTRUE(mrb$is_partial_run)) {
  restore.point("mrb_make_r_base_parcels")

  all_step_parcels = mrb$all_step_parcels
  if (is.null(all_step_parcels)) {
    cat("\nmrb_save_step_parcels: mrb$all_step_parcels were not yet generated. Make sure mrb_run_r_base is called beforehand.\n")
    return(mrb)
  }

  step_fields = unique(unlist(lapply(all_step_parcels, names), use.names = FALSE))
  extra_regcoef_fields = grep("^regcoef_", step_fields, value = TRUE)
  extra_regcoef_fields = setdiff(
    extra_regcoef_fields,
    c("regcoef_so", "regcoef_rb", "regcoef_diff")
  )
  extra_regcoef_fields = sort(extra_regcoef_fields)

  if (is_partial_run) {
    mrb$parcels = repdb_load_parcels(
      mrb$project_dir,
      c(
        "reg", "regcoef", "regvar", "regxvar",
        "colstat_numeric", "colstat_dummy", "colstat_factor",
        "colinfo", "regscalar", "regstring",
        extra_regcoef_fields
      ),
      mrb$parcels
    )
  }

  parcels = list()

  combine_steps = function(field) {
    res_list = lapply(all_step_parcels, function(x) x[[field]])
    res_list = res_list[!sapply(res_list, is.null)]

    if (length(res_list) == 0) {
      new_data = tibble()
    } else {
      new_data = bind_rows(res_list)
    }

    if (isTRUE(is_partial_run) && !is.null(mrb$parcels[[field]])) {
      old_data = mrb$parcels[[field]]
      if (NROW(old_data) > 0 && NROW(new_data) > 0) {
        old_kept = old_data[!old_data$runid %in% mrb$partial_pids, , drop = FALSE]
        new_data = bind_rows(old_kept, new_data)
      } else if (NROW(old_data) > 0 && NROW(new_data) == 0) {
        new_data = old_data
      }
    }

    new_data
  }

  # reg
  parcels$reg = combine_steps("reg")

  # Coefs and variables. regcoef_so is intentionally not generated here.
  # It is generated independently by mrb_make_so_parcels().
  parcels$regcoef = combine_steps("regcoef")

  for (field in extra_regcoef_fields) {
    parcels[[field]] = combine_steps(field)
  }

  parcels$regvar = combine_steps("regvar")
  parcels$regxvar = combine_steps("regxvar")

  # Column Stats
  parcels$colstat_numeric = combine_steps("colstat_numeric")
  parcels$colstat_dummy = combine_steps("colstat_dummy")
  parcels$colstat_factor = combine_steps("colstat_factor")

  parcels$colinfo = combine_steps("colinfo")

  # Scalars and Macros
  parcels$regscalar = combine_steps("regscalar")
  parcels$regstring = combine_steps("regstring")

  # regsource parcel is just a combination of existing parcels
  mrb$parcels = repdb_load_parcels(mrb$project_dir, c("stata_file", "stata_cmd"), parcels = mrb$parcels)
  run_df = mrb$drf$run_df

  if (NCOL(parcels$reg)>0 & !is.null(parcels$reg)) {
    regsource = parcels$reg %>%
      select(runid) %>%
      left_join(run_df %>% select(runid, file_path, line), by="runid") %>%
      left_join(mrb$parcels$stata_cmd %>% select(file_path, line, code_line_start=orgline_start, code_line_end = orgline_end), by = c("file_path", "line")) %>%
      left_join(mrb$parcels$stata_file, by="file_path") %>%
      rename(script_path = file_path, script_name = file_name,script_type = file_type) %>%
      mutate(script_file = basename(script_path))

    parcels$regsource = regsource
  } else {
    parcels$regsource = tibble()
  }

  if (save) {
    repdb_dir = file.path(mrb$project_dir, "repdb")

    static_parcels = parcels[setdiff(names(parcels), extra_regcoef_fields)]
    repboxDB::repdb_save_parcels(static_parcels, repdb_dir, check = TRUE)

    # Dynamic variant parcels use the regcoef schema but have dynamic names,
    # so they are saved without table-name based checking.
    if (length(extra_regcoef_fields) > 0) {
      extra_parcels = parcels[extra_regcoef_fields]
      repboxDB::repdb_save_parcels(extra_parcels, repdb_dir, check = FALSE)
    }
  }

  mrb$parcels[names(parcels)] = parcels
  return(mrb)
}






