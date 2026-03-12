
mrb_to_repdb = function(mrb = NULL, project_dir=mrb$project_dir, repdb_dir = mrb$repdb_dir) {
  restore.point("metareg_base_to_repdb")
  stop()

  stata_ct_sb = mrb_agg_stata_regcoef(mrb)
  stata_ct_dprobit = mrb_agg_add_dprobit_coef(mrb,mrb$stata_ct)
  reg_scalars = mrb_agg_stata_reg_scalars(mrb)
  reg_macros = mrb_agg_stata_reg_macros(mrb)


  artid = basename(project_dir)
  parcels = list()

  project = artid = .artid = basename(project_dir)
  parcels = list()

  if (NROW(agg$regs)==0 + NROW(agg$error_regs)) {
    cat("\nNo regression successfully analyzed and stored via base metareg.\n")
    return(invisible(parcels))

  }

  repdb.dir = mr$repdb_out_dir
  #dap = readRDS(file.path(project_dir, "repbox/stata/dap.Rds"))

  step.df = mr$step.df
  dotab = readRDS(file.path(project_dir, "repbox/stata/dotab.Rds"))



  # 1 a) Create reg and regsource entries ####

  regs = agg$regs

  reg_dat = agg$org_regs %>%
    mutate(
      artid = .artid,
      step = step,
      variant = "sb",
      lang = "stata",
      script_type = "do",
      script_file = paste0(doid,".do"),
      script_num = donum
    )

  stats = agg$stata_scalars %>%
    pivot_wider(names_from = var,values_from = val) %>%
    add_coalesce("nobs",c("N")) %>%
    add_coalesce("nobs_org") %>% # As NA
    add_coalesce("r2",c("r2","r2_p"))
  #add_coalesce("df_r",c("df_r")) %>%
  #add_coalesce("adj_r2",c("r2_a","ar2")) %>%
  #add_coalesce("F",c("F"))


  regs$iv_code = !sapply(regs$instr_parts, is.null)

  stats = stats %>%
    join_coalesce(regs, by="step",c("nobs_org","iv_code","se_category","se_type","se_args","ncoef"))

  fields = repdb_field_names("reg")
  dat = reg_dat %>%
    left_join(select(step.df, step, runid),by="step") %>%
    left_join_overwrite(stats[,intersect(fields, names(stats))],by="step",yfields=fields)

  dat$script_path = dotab$file[dat$donum] %>% str.right.of("/mod/")
  dat$code_line_start = dat$orgline
  dat$code_line_end = NA_integer_
  dat$source_lang = dat$lang
  dat$tdelta = as_integer(dat$tdelta)

  dat$base_variant = "sb"

  dat$error_in_r = !(dat$step %in% agg$regs$step)

  repdb_check_data(dat,"reg")

  parcels$reg_core = list(reg=dat)

  #repdb$reg = repdb_save_rds(dat,repdb.dir,"reg")

  repdb_check_data(dat,"regsource")
  parcels$regsource = list(regsource=dat)

  #repdb_check_data(dat,"stepinfo")
  #parcels$reg_core$stepinfo = dat

  # 1b) Save cmdpart #####################

  reg.df = dat
  cp.df = bind_rows(regs$cmdpart)

  #cp.df = cmdparts_of_stata_reg(reg.df$cmdline)
  #cp.df$step = reg.df$step[cp.df$str_row]
  #cp.df$artid = project

  repdb_check_data(cp.df,"cmdpart")
  parcels$cmdpart = list(cmdpart = cp.df)

  # 1c) Save regcoef_diff_summary ####################

  dat = bind_rows(agg$diff_org_sum, agg$diff_r_sum)
  repdb_check_data(dat,"regcoef_diff")

  parcels$reg_core$regcoef_diff = dat


  # 1d) Save header
  repdb_check_data(agg$header,"header")
  parcels$reg_core$header = agg$header


  # 2. Save regcoef ######################

  repdb_check_data(agg$stata_co,"regcoef")
  repdb_check_data(agg$org_co,"regcoef")

  parcels$regcoef = list(regcoef = agg$stata_co)
  parcels$org_regcoef = list(regcoef =c(agg$org_co))

  regcoef = agg$stata_co

  # 3. a) Save regvar #######################

  restore.point("ejd_to_repdb.3")


  colnames(agg$vi_df)
  vi = agg$vi_df %>%
    mutate(
      artid = project,
      variant = "sb",
      basevar = basevar,
      ia_source_expr = ia_expr,
      var_source_expr = var_expr,

      # Variables with time series operators. See
      # See https://www.stata.com/manuals/u11.pdf#u11.4.4
      prefix.type = tolower(substring(prefix,1,1)),
      prefix.num = trimws(substring(prefix,2)),
      prefix.num = ifelse(prefix.num=="", 1, as_integer(trimws(prefix.num))),
      transform = case_when(
        TRUE  ~ prefix.type
      ),
      # transform = case_when(
      #   prefix.type == "L" ~ "lag",
      #   prefix.type == "F" ~ "lead",
      #   prefix.type == "D" ~ "diff",
      #   prefix.type == "S" ~ "sdiff",
      #   TRUE  ~ ""
      # ),
      transform_par = case_when(
        transform %in% c("","log") ~ "",
        TRUE ~ prefix.num %>% change_val("","1")
      )
    )

  drop_df = regcoef %>%
    filter(is.na(regcoef$coef)) %>%
    select(step, cterm) %>%
    unique() %>%
    mutate(is_dropped = rep(TRUE,n()))

  vi = left_join_overwrite(vi, drop_df, by=c("step","cterm"))
  vi$is_dropped = is.true(vi$is_dropped) & vi$role %in% c("exo","endo")

  repdb_check_data(vi,"regvar")

  parcels$regvar = list(regvar = vi)

  # 3b) Save regxvar

  repdb_check_data(agg$regxvar,"regxvar")

  parcels$regxvar = list(regxvar = agg$regxvar)


  # 4. Save colstat ##############

  parcels$colstat = list()
  if (NROW(agg$colstat_numeric)>0) {
    colstat = agg$colstat_numeric %>%
      mutate(
        artid = rep(project,n()),
        variant = rep("sb",n()),
        cterm = col
      )
    repdb_check_data(colstat,"colstat_numeric")
    parcels$colstat$colstat_numeric = colstat
  }

  if (NROW(agg$colstat_dummy)>0) {
    colstat = agg$colstat_dummy %>%
      mutate(
        artid = rep(project,n()),
        variant = rep("sb",n()),
        cterm = col
      )
    repdb_check_data(colstat,"colstat_dummy")
    parcels$colstat$colstat_dummy = colstat
  }
  if (NROW(agg$colstat_factor)>0) {
    colstat = agg$colstat_factor %>%
      mutate(
        artid = rep(project,n()),
        variant = rep("sb",n()),
        cterm = col
      )
    repdb_check_data(colstat,"colstat_factor")
    parcels$colstat$colstat_factor = colstat
  }

  # 5. regscalar and regstring #####

  agg$org_regs$er

  er_df = bind_rows_with_parent_fields(agg$org_regs,"er", "step")

  res = repdb_stats_to_regscalar_regstring(er_df, variant="sb", artid=artid)

  parcels$regstring=list(regstring=res$regstring)
  parcels$regscalar=list(regscalar=res$regscalar)

  # 6. regcheck ######
  repdb_check_data(agg$regcheck,"regcheck")
  parcels$reg_core$regcheck = agg$regcheck

  # 8. Store extra regressions ######
  #    These are e.g. marginal effects.
  #    Stored in a separate parcel
  extra = agg$extra
  if (NROW(extra$ct)>0) {
    restore.point("ijsfhksdhfusfi")
    base_extra_reg = list()
    base_extra_reg$regcoef = extra$ct %>% add_col(artid=artid)
    base_extra_reg$regscalar = extra$regscalar %>% add_col(artid=artid)
    base_extra_reg$regmacro = extra$regmacro %>% add_col(artid=artid)
    parcels$base_extra_reg = base_extra_reg
    repdb_check_data(base_extra_reg$regcoef, "regcoef")
  }

  # 7. Save parcels #####

  repdb_save_parcels(parcels, repdb.dir, check=TRUE)
  invisible(parcels)

}


repdb_stats_to_regscalar_regstring = function(stats, step=NULL, variant = NULL, artid=NULL, omit_strings = c("cmdline","cmd","depvar","variant","artid"), omit_scalars = NULL) {
  restore.point("repdb_split_regscalar_regstring")
  stats = as_tibble(stats)

  cols = names(stats)
  char_cols = cols[sapply(stats, is.character)]
  num_cols = setdiff(cols, c(char_cols,"step", omit_scalars))
  char_cols = setdiff(char_cols, omit_strings)

  if (length(char_cols)>0) {
    regstring = stats[,c("step", char_cols)] %>%
      tidyr::pivot_longer(all_of(char_cols), names_to="string_name", values_to="string_val")
  } else {
    regstring = NULL
  }


  if (length(num_cols)>0) {
    stats[num_cols] = lapply(stats[num_cols], as.numeric)
    regscalar = stats[,c("step", num_cols)] %>%
      tidyr::pivot_longer(all_of(num_cols), names_to="scalar_name", values_to="scalar_val")
  } else {
    regscalar = NULL
  }


  if (!"step" %in% colnames(stats) & !is.null(step)) {
    if (NROW(regstring)>0) regstring$step = step
    if (NROW(regscalar)>0) regscalar$step = step
  }


  if (!"variant" %in% colnames(stats) & !is.null(variant)) {
    if (NROW(regstring)>0) regstring$variant = variant
    if (NROW(regscalar)>0) regscalar$variant = variant
  }

  if (!"artid" %in% colnames(stats) & !is.null(artid)) {
    if (NROW(regstring)>0) regstring$artid = artid
    if (NROW(regscalar)>0) regscalar$artid = artid
  }


  if (NROW(regstring)==0) regstring = NULL
  if (NROW(regscalar)==0) regscalar = NULL

  list(regstring=regstring, regscalar=regscalar)

}

repdb_glance_to_reg_stats = function(glance) {
  stats = glance %>%
    add_coalesce("r2",c("r.squared")) %>%
    add_coalesce("adj_r2",c("adj.r.squared")) %>%
    add_coalesce("df_r",c("df.residual")) %>%
    add_coalesce("F",c("F","statistic"))
  stats[,intersect(c("artid","variant", "step", "r2","adj_r2","df_r","F"), colnames(stats))]
}

extract_reg_stats_from_regscalar = function(regscalar) {
  restore.point("extract_reg_stats_from_regscalar")

  stats = regscalar[, intersect(c("step","artid","variant"), colnames(regscalar))] %>%
    unique()

  stats
}




mrb_to_repdb = function(mrb = NULL, project_dir=mrb$project_dir, repdb_dir = mrb$repdb_dir) {
  restore.point("metareg_base_to_repdb")

  stata_ct_sb = mrb_agg_stata_regcoef(mrb)
  stata_ct_dprobit = mrb_agg_add_dprobit_coef(mrb,mrb$stata_ct)
  reg_scalars = mrb_agg_stata_reg_scalars(mrb)
  reg_macros = mrb_agg_stata_reg_macros(mrb)


  artid = basename(project_dir)
  parcels = list()

  dotab = readRDS(file.path(project_dir, "repbox/stata/dotab.Rds"))

  # 1 a) Create reg and regsource entries ####
  regs = agg$regs

  # Note: We use org_regs from the original run here to get basic script metadata (like donum, orgline)
  # and execution metadata (like has.data, runerr) which belongs to the original context but is required
  # for building a complete row in the metareg regsource table mappings.
  reg_dat = agg$org_regs %>%
    mutate(
      artid = .artid,
      step = step,
      variant = "sb",
      lang = "stata",
      script_type = "do",
      script_file = paste0(doid,".do"),
      script_num = donum
    )

  # Metareg run stats
  stats = agg$stata_scalars %>%
    pivot_wider(names_from = var,values_from = val) %>%
    add_coalesce("nobs",c("N")) %>%
    add_coalesce("nobs_org") %>% # As NA
    add_coalesce("r2",c("r2","r2_p"))

  regs$iv_code = !sapply(regs$instr_parts, is.null)

  stats = stats %>%
    join_coalesce(regs, by="step",c("nobs_org","iv_code","se_category","se_type","se_args","ncoef"))

  fields = repdb_field_names("reg")
  dat = reg_dat %>%
    left_join(select(step.df, step, runid),by="step") %>%
    left_join_overwrite(stats[,intersect(fields, names(stats))],by="step",yfields=fields)

  dat$script_path = dotab$file[dat$donum] %>% str.right.of("/mod/")
  dat$code_line_start = dat$orgline
  dat$code_line_end = NA_integer_
  dat$source_lang = dat$lang
  dat$tdelta = as_integer(dat$tdelta)

  dat$base_variant = "sb"
  dat$error_in_r = !(dat$step %in% agg$regs$step)

  repdb_check_data(dat,"reg")
  parcels$reg_core = list(reg=dat)

  repdb_check_data(dat,"regsource")
  parcels$regsource = list(regsource=dat)

  # 1b) Save cmdpart #####################
  cp.df = bind_rows(regs$cmdpart)
  repdb_check_data(cp.df,"cmdpart")
  parcels$cmdpart = list(cmdpart = cp.df)

  # 1c) Save regcoef_diff_summary ####################
  dat_diff = bind_rows(agg$diff_org_sum, agg$diff_r_sum)
  repdb_check_data(dat_diff,"regcoef_diff")
  parcels$reg_core$regcoef_diff = dat_diff

  # 1d) Save header
  repdb_check_data(agg$header,"header")
  parcels$reg_core$header = agg$header

  # 2. Save regcoef ######################
  repdb_check_data(agg$stata_co,"regcoef")
  parcels$regcoef = list(regcoef = agg$stata_co)
  regcoef = agg$stata_co

  # 3. a) Save regvar #######################
  restore.point("metareg_base_to_repdb.3")

  vi = agg$vi_df %>%
    mutate(
      artid = project,
      variant = "sb",
      basevar = basevar,
      ia_source_expr = ia_expr,
      var_source_expr = var_expr,

      # Variables with time series operators. See
      # See https://www.stata.com/manuals/u11.pdf#u11.4.4
      prefix.type = tolower(substring(prefix,1,1)),
      prefix.num = trimws(substring(prefix,2)),
      prefix.num = ifelse(prefix.num=="", 1, as_integer(trimws(prefix.num))),
      transform = case_when(
        TRUE  ~ prefix.type
      ),
      transform_par = case_when(
        transform %in% c("","log") ~ "",
        TRUE ~ prefix.num %>% change_val("","1")
      )
    )

  drop_df = regcoef %>%
    filter(is.na(regcoef$coef)) %>%
    select(step, cterm) %>%
    unique() %>%
    mutate(is_dropped = rep(TRUE,n()))

  vi = left_join_overwrite(vi, drop_df, by=c("step","cterm"))
  vi$is_dropped = is.true(vi$is_dropped) & vi$role %in% c("exo","endo")

  repdb_check_data(vi,"regvar")
  parcels$regvar = list(regvar = vi)

  # 3b) Save regxvar
  repdb_check_data(agg$regxvar,"regxvar")
  parcels$regxvar = list(regxvar = agg$regxvar)

  # 4. Save colstat ##############
  parcels$colstat = list()
  if (NROW(agg$colstat_numeric)>0) {
    colstat = agg$colstat_numeric %>%
      mutate(
        artid = rep(project,n()),
        variant = rep("sb",n()),
        cterm = col
      )
    repdb_check_data(colstat,"colstat_numeric")
    parcels$colstat$colstat_numeric = colstat
  }

  if (NROW(agg$colstat_dummy)>0) {
    colstat = agg$colstat_dummy %>%
      mutate(
        artid = rep(project,n()),
        variant = rep("sb",n()),
        cterm = col
      )
    repdb_check_data(colstat,"colstat_dummy")
    parcels$colstat$colstat_dummy = colstat
  }
  if (NROW(agg$colstat_factor)>0) {
    colstat = agg$colstat_factor %>%
      mutate(
        artid = rep(project,n()),
        variant = rep("sb",n()),
        cterm = col
      )
    repdb_check_data(colstat,"colstat_factor")
    parcels$colstat$colstat_factor = colstat
  }

  # 5. regscalar and regstring #####
  # Here we use the outputs harvested from metareg's rerun and parse them appropriately

  if (NROW(agg$stata_scalars) > 0) {
    regscalar = agg$stata_scalars %>%
      rename(scalar_name = var, scalar_val = val) %>%
      mutate(artid = artid, variant = "sb")
    parcels$regscalar = list(regscalar = regscalar)
  }

  if (NROW(agg$stata_macros) > 0) {
    regstring = agg$stata_macros %>%
      rename(string_name = var, string_val = val) %>%
      mutate(artid = artid, variant = "sb")
    parcels$regstring = list(regstring = regstring)
  }

  # 6. regcheck ######
  repdb_check_data(agg$regcheck,"regcheck")
  parcels$reg_core$regcheck = agg$regcheck

  # 8. Store extra regressions ######
  #    These are e.g. marginal effects.
  #    Stored in a separate parcel
  extra = agg$extra
  if (NROW(extra$ct)>0) {
    restore.point("metareg_base_to_repdb_extra")
    base_extra_reg = list()
    base_extra_reg$regcoef = extra$ct %>% add_col(artid=artid)
    if (NROW(extra$scalars) > 0) {
      base_extra_reg$regscalar = extra$scalars %>% rename(scalar_name = var, scalar_val = val) %>% add_col(artid=artid)
    }
    if (NROW(extra$macros) > 0) {
      base_extra_reg$regmacro = extra$macros %>% rename(string_name = var, string_val = val) %>% add_col(artid=artid)
    }
    parcels$base_extra_reg = base_extra_reg
    repdb_check_data(base_extra_reg$regcoef, "regcoef")
  }

  # 7. Save parcels #####
  repdb_save_parcels(parcels, repdb_dir, check=TRUE)
  invisible(parcels)
}
