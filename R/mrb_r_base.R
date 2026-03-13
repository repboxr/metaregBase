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
  mrb = mrb_run_r_base(mrb, just_pid = drf$pids[1])
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
  if (!is.null(just_pids)) {
    pids = just_pids
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
    mrb_make_r_base_parcels(mrb)
  }

  mrb
}


# ==============================================================================
# PER-STEP ORCHESTRATOR (`mrb_run_r_step`)
# ==============================================================================

#' Process a single regression, expand syntax, and format standard parcels
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

  # 2. Extract specific Stata outcomes for this step
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
  wide_dat = create_cterm_cols(dat, ct_cterms, timevar = xtvar$timevar, panelvar=xtvar$panelvar, tdelta=xtvar$tdelta)[, ct_cterms, drop=FALSE]

  reg_types = bind_rows(
    regvar %>% select(term = cterm, reg_type = var_reg_type),
    regvar %>% select(term = ia_cterm, reg_type = ia_reg_type)
  ) %>% unique()

  colstats = make_colstats(ct_cterms, wide_dat, wide_dat, reg_types)

  #####################
  # Create step parcels
  #####################

  step_parcels = list()

  # A. REGCOEF (Parsed Stata Coefficients)
  if (!is.null(stata_ct) && nrow(stata_ct) > 0) {
    step_parcels$regcoef = ct_to_regcoef(stata_ct, variant = "sb", artid = mrb$artid)
  } else {
    step_parcels$regcoef = tibble()
  }

  # B. REGVAR (Variables with prefixes and dropping info)
  dropped_cterms = if (nrow(step_parcels$regcoef) > 0) {
    step_parcels$regcoef %>% filter(is.na(coef)) %>% pull(cterm)
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
  step_parcels$regxvar = make_regxvar(step_parcels$regvar, dat, step_parcels$regcoef)

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


# The step parcels are generated in mrb_r
mrb_make_r_base_parcels = function(mrb, save=TRUE) {
  restore.point("mrb_make_r_base_parcels")

  parcels = list()
  all_step_parcels = mrb$all_step_parcels
  if (is.null(all_step_parcels)) {
    cat("\nmrb_save_step_parcels: mrb$all_step_parcels were not yet generated. Make sure mrb_run_r_base is called beforehand.\n")
    return(mrb)
  }

  combine_steps = function(field) {
    bind_rows(lapply(all_step_parcels, function(x) x[[field]]))
  }


  # reg
  parcels$reg = combine_steps("reg")

  # Coefs & Variables
  parcels$regcoef = combine_steps("regcoef")
  parcels$regvar = combine_steps("regvar")
  parcels$regxvar = combine_steps("regxvar")

  # Column Stats
  parcels$colstat_numeric = combine_steps("colstat_numeric")
  parcels$colstat_dummy = combine_steps("colstat_dummy")
  parcels$colstat_factor = combine_steps("colstat_factor")

  parcels$colinfo = combine_steps("colinfo")


  # Scalars & Macros
  parcels$regscalar = combine_steps("regscalar")
  parcels$regstring = combine_steps("regstring")

  # regsource parcel is just a combination of existing parcels
  mrb$parcels = repdb_load_parcels(mrb$project_dir, c("stata_file", "stata_cmd"),parcels = mrb$parcels)
  run_df = mrb$drf$run_df

  regsource = parcels$reg %>%
    select(runid) %>%
    left_join(run_df %>% select(runid, file_path, line), by="runid") %>%
    left_join(mrb$parcels$stata_cmd %>% select(file_path, line, code_line_start=orgline_start, code_line_end = orgline_end), by = c("file_path", "line")) %>%
    left_join(mrb$parcels$stata_file, by="file_path") %>%
    rename(script_path = file_path, script_name = file_name,script_type = file_type) %>%
    mutate(script_file = basename(script_path))

  parcels$regsource = regsource


  # TO DO:
  # regcoef_diff
  # regcoef_so
  # regcoef_rb
  # regcheck

  # Save everything directly into the repdb directory
  if (save) {
    repdb_dir = file.path(mrb$project_dir, "repdb")
    repboxDB::repdb_save_parcels(parcels, repdb_dir, check = TRUE)
  }

  mrb$parcels[names(parcels)] = parcels
  return(mrb)
}
