# Basic metareg run that generates basic entries into repdb database
# Checks also if
# orginal stata run and metareg stata run are identical
# r implementation leads same / similar results as Stata run

example = function() {
  library(repboxRun)

  # set.stata.paths(stata.dir="C:/programs/Stata17",ado.dirs = c(plus = "C:/libraries/repbox/ado/plus"))
  # check.stata.paths()
  #
  # project = "testsupp"
  # project_dir = file.path("C:/libraries/repbox/projects_reg/",project)

  project = "testsupp"
  project_dir = file.path("~/repbox/projects_reg",project)
  #run.df = readRDS("~/repbox/projects_reg/testsupp/repbox/stata/repbox_results.Rds")$run.df
  #all(run.df$timevar=="")
  steps = repbox_run_steps_from(reproduction=TRUE,art = FALSE,html = TRUE)
  repbox_run_project(project_dir,steps = steps)
  rstudioapi::filesPaneNavigate(project_dir)

  options(warn=2)
  mr = mr_base_run_study(project_dir, stop.on.error = TRUE,create.repdb = TRUE)

  step.df = mr$step.df
  regcheck = mr$results$agg$regcheck


  mr$results
}


mr_base_run_study = function(project_dir, run_stata=TRUE, astep = NULL, extra.cache=is.null(astep), stop.on.error = FALSE, create.repdb = TRUE, stata_version = NA) {
  restore.point("mr_base_run_study")

  #project_dir = file.path("~/repbox/projects_reg/testsupp")
  #project_dir = file.path("~/repbox/projects_reg/aer_103_3_14")
  project = basename(project_dir)

  res = dap_and_cache_check_outdated(project_dir)
  if (!res$ok) {
    stop(res$msg)
  }

  dap = get.project.dap(project_dir,make.if.missing = TRUE, add.run.df = TRUE)
  if (!has.col(dap, "reg_ok")) {
    dap = dap_add_reg_ok(project_dir, dap=dap)
  }

  step.df = dap$step.df
  #plot.dap(dap)

  opts = mr_opts(load.extra.cache = extra.cache, extra.cache=extra.cache, stop.on.error = stop.on.error,pass.internal.info = TRUE, pass.repdb.info = FALSE, stata.preserve.always=TRUE)

  mr = mr_init_study(project_dir,
    metaid="base", version=0,
    stata_code_fun=mr_base_stata_code_fun,
    step_run_fun = mr_base_step_run_fun_outer,
    stata_agg_fun = mr_base_stata_agg_fun,
    study_agg_fun = mr_base_study_agg_fun,
    dap=dap,opts = opts,
    stata_version=stata_version)

  if (mr_has_problem(mr)) {
    return(mr_finish(mr))
  }

  mr$create.repdb = create.repdb

  # remove previous error steps
  # error_dir = file.path(mr$project_dir,"metareg","base","error_steps")
  # error.files =list.files(error_dir, glob2rx("error_*.txt"), full.names = TRUE)
  # if (length(error.files)>0) {
  #   file.remove(error.files)
  # }


  if (is.null(astep)) {
    mr = mr_run(mr, run_stata = run_stata, clear_old_step_results = run_stata)
  } else {
    mr = mr_run(mr, run_stata = run_stata, clear_old_step_results = FALSE,asteps = astep)
  }


  agg = mr_get_result(mr, "agg")
  if (!(run_stata) & NROW(agg$stata_ct)==0) {
    cat("\n\nWARNING: You have set run_stata = FALSE but no previous Stata results existed.\n")
  }


  mr_finish(mr)
}

# mr_base_adapt_dap_stata_cmds = function(mr) {
#   restore.point("mr_base_adapt_dap_stata_cmds")
#   step.df = mr$step.df
#
#   # Replace regressions like
#   # reg y _Imyvar*
#   # with
#   # xi: reg y i.myvar*
#
#   # May not always work, but hopefully often
#
#   rows = step.df$step_type=="a" & has.substr(step.df$stata_code," _I")
#   if (length(rows)==0) return(mr)
#   step.df$stata_code[rows] = stri_replace_all_fixed(step.df$stata_code[rows]," _I"," i.") %>% paste0("xi: ", .)
#   mr$step.df = step.df
#
#   mr
# }

# Sometimes the Stata coefficient table shows additional
# rows that are no regression coefficients. E.g. in a tobit
# regression we saw "var(e.totalsaving)". We remove such
# rows as they will create errors in further processing
remove_non_standard_stata_coefs = function(ct) {
  if (NROW(ct)==0) return(ct)
  ignore.rows = which(has.substr(ct$var,"(")|has.substr(ct$var,"/"))
  if (length(ignore.rows)>0) {
    ct = ct[-ignore.rows,]
  }
  ct
}

mr_base_stata_agg_fun = function(mr, stata_check_df, ...) {
  restore.point("mr_base_stata_agg_fun")

  # Regression results from original run
  regtab.file = file.path(mr$project_dir,"repbox/stata/regtab.Rds")
  org_regs = readRDS.or.null(regtab.file)

  ct = NULL
  for (i in seq_len(NROW(org_regs))) {
    ct = org_regs$ct[[i]]
    ct = remove_non_standard_stata_coefs(ct)
    ct$cterm =  canonical.stata.output.terms(ct$var, ct$label)
    org_regs$ct[[i]] = ct
  }

  # Map step
  org_regs = left_join(org_regs,select(mr$step.df, step, donum, line, counter), by=c("donum","line","counter"))

  # Stata results from metareg run
  stata_scalars = mr_agg_stata_reg_scalars(mr,"regscalar_")
  stata_macros = mr_agg_stata_reg_macros(mr, "regmacro_")

  stata_ct = bind_rows(
    mr_agg_stata_parmest(mr,"reg_"),
    mr_agg_estout_tsv(mr,"reg_")
  )  %>% remove_non_standard_stata_coefs()

  stata_ct = mr_agg_add_dprobit_coef(mr, stata_ct)

  if (NROW(stata_ct)==0 & NROW(ct)>0) {
    repbox_problem(type="stata_ct_empty",msg = "No coefficients extracted from re-run of regression in mr_base metareg sandbox. There seems to be something in the original stata code that our sandbox still misses.", fail_action="error")

    # For some applications like data editor tool
    # we may want to proceed despite this problem
    # So we set stata_ct to ct
    #stata_ct = ct
    #stata_ct$variant = "sb"
  } else if (NROW(stata_ct) < NROW(ct)) {
    repbox_problem(type="stata_ct_too_few_rows",msg = "stata_ct from metareg sandbox has fewer rows than origina run ct.", fail_action = "msg")
  }


  cterms = canonical.stata.output.terms(stata_ct$var, stata_ct$label)
  stata_ct = bind_cols(cterm=cterms, stata_ct)

  extra = list()
  extra$ct = filter(stata_ct, variant != "sb")
  if (NROW(extra$ct)>0) {
    stata_ct = filter(stata_ct, variant == "sb")
    extra$ct = ct_to_regcoef(extra$ct, artid=mr$artid)
  }

  extra$scalars = filter(stata_scalars, variant != "sb")
  if (NROW(extra$scalars)>0) {
    stata_scalars = filter(stata_scalars, variant == "sb")
  }
  extra$macros = filter(stata_macros, variant != "sb")
  if (NROW(extra$macros)>0) {
    stata_macros = filter(stata_macros, variant == "sb")
  }


  stata_agg = list(org_regs = org_regs, stata_ct = stata_ct,  stata_scalars = stata_scalars, stata_macros=stata_macros, extra=extra)

  mr = mr_set_result(mr, stata_agg = stata_agg)
  mr
}


mr_base_study_agg_fun = function(mr, ...) {
  restore.point("mr_base_study_agg_fun")
  project = basename(mr$project_dir)
  project_dir = mr$project_dir

  stata_agg = mr_get_result(mr, "stata_agg")
  org_regs = stata_agg$org_regs

  # R results from metareg run
  error_regs = mr_get_steps_result(mr, "reg_error", add_step=FALSE)

  regs = mr_get_steps_result(mr, "reg", add_step=FALSE)

  #regs = mr_agg_df_rds(mr, "reg_*.Rds", file_col="file")
  #regs$step = as_integer(str.between(regs$file, "_", "."))


  # Create all regcoef
  stata_co =  stata_agg$stata_ct %>%
    ct_to_regcoef("stata", variant="sb", artid=project)

  org_co = bind_rows_with_parent_fields(org_regs, "ct", "step") %>%
    ct_to_regcoef("stata", variant="so", artid=project)

  colnames(org_co)


  # Comparison of regcoef
  diff_org_tab = coef_diff_table(org_co, stata_co)
  diff_org_sum = coef_diff_summary(diff_org_tab,c("all","coef"))

  # Combine the variable information to a data frame
  vi_df = bind_rows_with_parent_fields(regs, "vi", "step")
  vi_df$project = project

  colstat_numeric = bind_rows_with_parent_fields(regs, "colstat_numeric", "step")
  colstat_dummy = bind_rows_with_parent_fields(regs, "colstat_dummy", "step")
  colstat_factor = bind_rows_with_parent_fields(regs, "colstat_factor", "step")


  if (NROW(error_regs)>0) {
    rows = match(error_regs$step, mr$check_df$step)
    if (length(rows)>0) {
      mr$check_df$did_run[rows]=FALSE
      mr$check_df$problem[rows]="analysis in R failed"
    }
  }
  regcheck = mr_get_regcheck_after_run(mr, variant="sb", add_stata_check=TRUE,tolerable_deviation = 1e-12)
  diff = diff_org_sum %>% filter(compare_what=="all")
  regcheck = left_join_overwrite(regcheck, select(diff,step, deviation= max_deviation), by="step",drop_in = "x")


  missing.org.reg.steps = setdiff(regcheck$step, org_regs$step)
  if (length(missing.org.reg.steps)>0) {
    rows = match(missing.org.reg.steps, regcheck$step)
    regcheck$problem[rows] = paste0("org reg had no coef table; ", regcheck$problem[rows])
  }

  mr = mr_set_header(mr, regcheck)

  agg = list(
    project = basename(mr$project_dir),
    header = mr_get_header(mr),
    regcheck = regcheck,
    vi_df = vi_df,
    regxvar = suppressWarnings(if (is.null(regs$regxvar)) NULL else bind_rows(regs$regxvar)),
    metaid = mr$metaid,
    version = mr$version,
    timestamp = Sys.time(),
    colstat_numeric = colstat_numeric,
    colstat_dummy = colstat_dummy,
    colstat_factor = colstat_factor,

    org_regs = org_regs,
    regs = regs[setdiff(names(regs), c("ct","vi"))],
    error_regs = error_regs,
    stata_scalars = stata_agg$stata_scalars,
    stata_macros = stata_agg$stata_macros,

    extra = stata_agg$extra,

    org_co = org_co,
    stata_co = stata_co,

    diff_org_sum = diff_org_sum
  )

  mr = mr_set_result(mr, agg=agg)

  saveRDS(agg, file.path(mr$out_dir,"agg.Rds"))

  if (mr$create.repdb) {
    base_to_repdb(mr=mr, agg=agg)

    # Store step results
    colinfo = repboxReg::mr_get_steps_result(mr, "colinfo",as_df = TRUE)
    parcels = list(colinfo = list(colinfo=colinfo))
    repdb_save_parcels(parcels, file.path(project_dir,"repdb"))

  }

  mr
}

mr_base_step_run_fun_outer = function(mr,step, ..., has_error=FALSE, err=NULL) {
  err_msg = ""
  if (!has_error) {
    err = try(mr_base_step_run_fun(mr=mr,step=step,...), silent=TRUE)
    restore.point("mr_base_step_run_fun_outer1")
    has_error = is(err, "try-error")
    if (!has_error) {
      mr = err
    } else {
      err_msg = as.character(err)
      cat("\n",err_msg,"\n")
    }
  } else {
    err_msg = as.character(err)
  }
  restore.point("mr_base_step_run_fun_outer2")
  # There was likely a parsing error
  if (has_error) {
    # outdir = file.path(mr$project_dir,"metareg","base","error_steps")
    # if (!dir.exists(outdir)) dir.create(outdir)
    # writeLines(err_msg, paste0(outdir, "error_", step, ".txt"))
    cat(paste0("\n  R analysis of regression in step = ", step, " cancelled due to an error.\n"))
    mr = mr_set_step_result(mr, step, reg_error=list(step=step, err_msg = err_msg))

  }
  return(mr)
}
# In R we will perform a regression using fixest (lm might take too long)
mr_base_step_run_fun =  function(mr,step, reg, dat, org_dat, infeasible_filter, ...) {
  restore.point("mr_base_step_run_fun")
  # stop()

  project_dir = mr$project_dir
  # Extract cmdpart, opts_df and se_info
  cmdpart = cmdparts_of_stata_reg(reg$cmdline)
  cmdpart$step = step
  cmdpart$artid = mr$artid

  opts_df = cmdpart_to_opts_df(cmdpart)
  se_info = se_stata_to_repdb(reg$cmd, opts_df)
  reg$cmdpart = list(cmdpart)
  reg$opts_df = list(opts_df)

  reg$runid = mr$step.df$runid[step]
  reg[c("se_category","se_type","se_args")] = se_info[c("se_category","se_type","se_args")]


  # Add information to stata results based on data set
  stata_agg = mr_get_result(mr, "stata_agg")


  org_regs = stata_agg$org_regs


  # Need handling for case that original regression
  # did not throw an error (so is part of step.df)
  # but nevertheless has no coef.table output.
  # That can e.g. be the case if timevar was not
  # correctly set.
  if (step %in% org_regs$step) {
    org_ct = org_regs$ct[org_regs$step==step][[1]]
  } else {
    org_ct = NULL
  }

  vi = reg$vi[[1]]


  # We want to generate also columns for all dummies
  # shown in resulting coefficient tables
  # Yet, some commands like xtlogit, show additional
  # coefficients that don't map to any x-variable
  # we need to remove them.
  ct_cterms = setdiff(org_ct$cterm,c("lnsig2u"))

  # All columns relevant for the regression
  cols = unique(c(reg$depvar,reg$panelvar, reg$timevar, ct_cterms, vi[["var"]], vi$cterm, vi$ia_cterm)) %>% setdiff(c("(Intercept)",""))



  #cols = unique(c(reg$depvar,reg$panelvar, reg$timevar, vi[["var"]], vi$cterm, vi$ia_cterm)) %>% setdiff(c("(Intercept)",""))

  wide_dat = create_cterm_cols(dat, cols)[,cols]

  reg_cols = unique(c(reg$depvar, org_ct$cterm, vi[["var"]], vi$cterm)) %>% setdiff(c("(Intercept)",""))
  reg_wide_dat = omit.na.or.similar(wide_dat, reg_cols)

  reg_types = bind_rows(
    vi %>% select(term = cterm, reg_type = var_reg_type),
    vi %>% select(term = ia_cterm, reg_type = ia_reg_type)
  ) %>% unique()

  colstats = make_colstats(cols, wide_dat, reg_wide_dat, reg_types)

  reg$ncoef = NROW(org_ct)
  reg$colstat_numeric = colstats["colstat_numeric"]
  reg$colstat_dummy = colstats["colstat_dummy"]
  reg$colstat_factor = colstats["colstat_factor"]

  reg$nobs_org = NROW(dat)

  #outfile = paste0(mr$step.dir, "/reg_", step, ".Rds")
  #saveRDS(reg, outfile)

  regcoef = stata_agg$stata_ct %>%
    ct_to_regcoef(variant="sb",artid=mr$artid)
  regvar = vi %>%
    mutate(artid=mr$artid, step=step)

  regxvar = make_regxvar(regvar, dat, regcoef)
  reg$regxvar = list(regxvar)

  reg$step = step
  mr = mr_set_step_result(mr, step, reg=reg)


  # colinfo
  # Sample data set
  if (NROW(dat)<=6) {
    sample_dat = dat
  } else {
    sample_dat = bind_rows(head(dat,3), tail(dat,3))
  }

  sample_dat_dir = file.path(mr$project_dir,"metareg","base","sample_dat")
  if (!dir.exists(sample_dat_dir)) dir.create(sample_dat_dir)
  sample_file = file.path(sample_dat_dir,paste0("sampledat_", step, ".Rds"))
  saveRDS(sample_dat, sample_file)

  colinfo = repbox_compute_step_col_info(step, project_dir, dat, org_dat, reg)
  #repboxDB::repdb_check_data(colinfo,"colinfo")
  mr = mr_set_step_result(mr, step, colinfo=colinfo)


  return(mr)
}


# We simply use the original Stata regression code
# and store the output as tsv
mr_base_stata_code_fun = function(mr, step, stata_code, ...) {
  restore.point("mr_base_stata_code_fun")


  # If the regression has a if condition that uses Stata
  # functions, we may not be able to translate it to R.
  # In this case our Stata code shall store the chosen
  # row numbers
  if_store_code = mr_base_if_store_code(mr, step)

  extra_code = ""
  outfile = paste0(mr$step.dir, "/reg_", step, "__sb.dta")
  scalar_outfile = paste0(mr$step.dir, "/regscalar_", step, "__sb.txt")
  macro_outfile = paste0(mr$step.dir, "/regmacro_", step, "__sb.txt")

  cmd = mr$step.df$cmd[step]
  # Command for which marginal effects are stored
  if (cmd %in% stata_cmds_with_margin()) {
    extra_code = paste0('
capture noisily margins, atmeans dydx(*) post
capture noisily parmest, saving("',mr$step.dir, "/reg_", step, "__sb_mem.dta",'")
capture noisily repbox_write_reg_scalars "',mr$step.dir, "/regscalar_", step, "__sb_mem.txt",'"
capture noisily repbox_write_reg_macros "',mr$step.dir, "/regmacro_", step, "__sb_mem.txt",'"
')
  } else if (cmd == "dprobit") {
    extra_code = paste0('capture noisily repbox_write_dprobit_coef_se "',mr$step.dir, "/dprobit_", step, ".csv\n")
  } else if (cmd %in% stata_cmds_with_exp_coef()) {
    extra_code = paste0('capture noisily estout . using "', mr$step.dir,'/reg_', step, '__sb_exp.tsv", cells("b se t p ci_l ci_u") replace eform\n')
  }

  code = paste0(
    "capture noisily ", stata_code,
    '\n
capture noisily parmest, label saving("',outfile,'", replace)
capture noisily repbox_write_reg_scalars "', scalar_outfile,'"
capture noisily repbox_write_reg_macros "', macro_outfile,'"
', extra_code, if_store_code
  )
  code
}



#
# get_from_stata_stat = function(stata_stat, stat, cmd=NA) {
#   restore.point("get_from_stata_stat")
#   if (stat=="r2") {
#     guesses = c("r2_p")
#   } else if (stat == "adj_r2") {
#     guesses = c("")
#   } else if (stat == "nobs") {
#     guesses = c("N")
#   }
#   cols = guesses[guesses %in% names(stata_stat)]
#   if (length(cols)==0) return(NA_real_)
#   stata_stat[[cols[1]]]
# }


mr_base_if_store_code = function(mr, step) {
  restore.point("mr_if_stor_code")
  cmdline = mr$step.df$cmdline[step]

  if (!has.substr(cmdline, " if ")) return(NULL)

  reg = stata.reg.parse(cmdline, run=mr$step.df[step,])
  if_str = reg$if_str


  # No Stata function call in if_str: we can manually
  # translate to R
  if (!has.substr(if_str,"(")) return(NULL)

  dir = file.path(mr$project_dir,"metareg","dap","stata", "ifrows")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file = paste0(dir,"/ifrows_", step, ".dta")

  code = paste0('

// Store row number from regression if condition
restore, preserve // because saving regstats changes data sets
gen r0W_ox_zA___2G__nUm__ = _n
capture noisily keep if ', if_str,'
keep r0W_ox_zA___2G__nUm__
capture noisily save "', file, '", replace
')
  code
}
