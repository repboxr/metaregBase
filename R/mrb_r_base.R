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

  # 0. Load Data UNFILTERED to allow TS operators to look across time correctly
  dat_full = repboxDRF::drf_get_data(pid, drf = mrb$drf, filtered = FALSE, continue_on_error = continue_on_error)
  if (is.null(dat_full) || inherits(dat_full, "try-error")) return(list())

  # Protect against instances where dat_full is a list without matrix names
  data_cols = names(dat_full)
  if (is.null(data_cols)) data_cols = character(0)

  # 1. Base Components
  run_obj = mrb$drf$run_df %>% filter(runid == pid)
  cmd = run_obj$cmd[1]

  all_cmdpart = mrb$parcels$reg_cmdpart
  cmdpart = all_cmdpart %>% filter(runid == pid)
  if (NROW(cmdpart) == 0) stop(paste0("No cmdpart stored for runid = ", pid))

  stata_ct = if (!is.null(mrb$stata_ct_sb)) mrb$stata_ct_sb %>% filter(runid == pid) else NULL
  stata_scalars = if (!is.null(mrb$stata_scalars)) mrb$stata_scalars %>% filter(runid == pid) else NULL
  stata_macros = if (!is.null(mrb$stata_macros)) mrb$stata_macros %>% filter(runid == pid) else NULL

  cmdpart = cmdpart_expand_vars(cmdpart, data_cols)
  opts_df = cmdpart_to_opts_df(cmdpart)
  panelvar = mrb_get_panelvar(run_obj, opts_df, xtvar)
  se_info = se_stata_to_repdb(cmd, opts_df, panelvar = panelvar,cmdpart = cmdpart)

  # 3. Create TS columns on UNFILTERED data
  # We build a lightweight, temporary regvar just to discover which cterms need evaluating.
  regvar_tmp = cmdpart_to_regvar(cmdpart, dat_full, opts_df, se_info)
  regvar_tmp = mrb_add_xtreg_fe_regvar(regvar_tmp, run_obj, opts_df, xtvar, dat_full)

  tmp_depvar = regvar_tmp$cterm[regvar_tmp$role == "dep"]
  ct_cterms_tmp = unique(c(tmp_depvar, regvar_tmp$var, regvar_tmp$cterm, regvar_tmp$ia_cterm)) %>% setdiff(c("(Intercept)",""))
  dat_full = create_cterm_cols(dat_full, ct_cterms_tmp, timevar=xtvar$timevar, panelvar=xtvar$panelvar, tdelta=xtvar$tdelta)

  # 4. Apply filter safely
  data = dat_full # The evaluated filter code expects the variable to be named 'data'

  pid_load_code = repboxDRF::drf_get_dependency_load_code(pid, mrb$drf)
  filter_code = repboxDRF::drf_get_filter_code(pid, mrb$drf, parcels = mrb$parcels)

  scalar_code = NULL
  if (pid %in% mrb$drf$scalar_code$runid) {
    rows = which(mrb$drf$scalar_code$runid == pid)
    scalar_code = mrb$drf$scalar_code$scalar_r_code[rows]
  }

  all_codes = c(scalar_code, pid_load_code, filter_code)
  library(stata2r)
  if (length(all_codes) > 0 && any(nzchar(all_codes))) {
    for (code in all_codes) {
      if (nzchar(code)) {
        eval(parse(text = code))
      }
    }
  }
  dat = data
  org_dat = data

  # 5. Build proper regvar with correct metadata based on FILTERED data
  regvar = cmdpart_to_regvar(cmdpart, dat, opts_df, se_info)
  regvar = mrb_add_xtreg_fe_regvar(regvar, run_obj, opts_df, xtvar, dat)

  depvar = regvar$cterm[regvar$role == "dep"]
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

  step_parcels$regxvar = make_regxvar(step_parcels$regvar, wide_dat_full, regcoef_main)

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

  step_parcels$colstat_datetime = if (nrow(colstats$colstat_datetime) > 0) {
    colstats$colstat_datetime %>% mutate(artid = mrb$artid, variant = "sb", runid = runid, cterm = col)
  } else {
    tibble()
  }

  nobs_val = if ("N" %in% names(stats_wide)) as.numeric(stats_wide$N) else NA_real_
  r2_val = if ("r2" %in% names(stats_wide)) as.numeric(stats_wide$r2) else if ("r2_p" %in% names(stats_wide)) as.numeric(stats_wide$r2_p) else NA_real_

  flags_vec = character()

  if (any(tolower(opts_df$opt) %in% c("noc","noco","nocon","nocons","noconst"))) {
    flags_vec = c(flags_vec, "noconst")
  }

  if (cmd %in% c("xtreg", "xtivreg", "xtivreg2")) {
    if (any(opts_df$opt == "re")) {
      flags_vec = c(flags_vec, "re")
    } else if (any(opts_df$opt == "fe")) {
      flags_vec = c(flags_vec, "fe")
    } else if (any(opts_df$opt == "be")) {
      flags_vec = c(flags_vec, "be")
    } else if (any(opts_df$opt == "pa")) {
      flags_vec = c(flags_vec, "pa")
    } else if (any(opts_df$opt == "mle")) {
      flags_vec = c(flags_vec, "mle")
    } else if (any(opts_df$opt %in% c("fd", "sd"))) {
      flags_vec = c(flags_vec, opts_df$opt[opts_df$opt %in% c("fd", "sd")][1])
    } else if (cmd != "xtivreg2") {
      # Stata default for xtreg and xtivreg is re
      flags_vec = c(flags_vec, "re")
    }
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

#' Extract Stata metaregBase results and create corresponding metaregBase parcels
mrb_run_r_base = function(mrb, just_pids=NULL, make_parcels=TRUE, continue_on_error=FALSE) {
  restore.point("mrb_run_r")

  repbox_problem_set_project_dir(mrb$project_dir)

  mrb$drf = drf_apply_loop_ignore(mrb$drf)

  mrb$artid = basename(mrb$project_dir)
  mrb$parcels = repboxDB::repdb_load_parcels(mrb$project_dir, c("reg_cmdpart", "xtvar"))

  pids = mrb$drf$pids
  run_df = mrb$drf$run_df
  if (!is.null(run_df) && "cmd_type" %in% names(run_df)) {
    pids = intersect(pids, run_df$runid[run_df$cmd_type == "reg"])
  }

  if (length(pids) == 0) {
    cat("\nNo reg pids to process.\n")
    return(mrb)
  }

  all_pids = pids
  if (!is.null(just_pids)) {
    pids = intersect(just_pids, pids)
    mrb$is_partial_run = TRUE
    mrb$partial_pids = pids
  } else {
    mrb$is_partial_run = FALSE
  }

  if (length(pids) == 0) {
    cat("\nNo reg pids to process in just_pids.\n")
    return(mrb)
  }

  mrb = mrb_agg_stata(mrb, skip_if_has = TRUE)

  all_step_parcels = list()

  pid = pids[1]
  cat("\nmrb_r_base processing runids: ")
  for (pid in pids) {
    cat(paste0(pid," "))
    step_parcels = mrb_run_r_base_step(mrb, pid, continue_on_error=continue_on_error)
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
        "colstat_numeric", "colstat_dummy", "colstat_factor", "colstat_datetime",
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
  parcels$colstat_datetime = combine_steps("colstat_datetime")

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


# Make sure to clear stuff needed for a clean re-run of mrb_base
mrb_clean_for_rerun = function(project_dir) {
  drf_clear_r_err_runids(project_dir)

  rm_parcels = c("regrepair","regcheck")
  rm_parcel_files = paste0(project_dir, "/repdb/",rm_parcels, ".Rds")
  rm_parcel_files = rm_parcel_files[file.exists(rm_parcel_files)]

  if (length(rm_parcel_files)>0)
    file.remove(rm_parcel_files)

}



