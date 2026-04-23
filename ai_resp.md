The main problem is not in the probit/logit math. It is in how the pipeline captures and labels results after failure.

Your `runid=5` parcels are a mixed artifact of three separate issues:

First, the original Stata command actually fails. In your log, `dprobit d1 x1 x i.i1 i.i2` throws `factor-variable and time-series operators not allowed`. Immediately after that, the injected `parmest` / `ereturn list` block still runs, and `e(cmdline)` is still the previous successful `logit d2 ...` model. So `regcoef_so` for runid 5 is not a dprobit result at all; it is stale logit state that got harvested after the failed command. That is the smoking gun in the log you pasted.

Second, on the metaregBase side, the main coefficient parcel is leaking in non-main variants. `mrb_agg_stata_regcoef()` loads every `reg_*.dta` variant, but `mrb_run_r_base_step()` does not restrict the main coefficient parcel to `variant == "sb"`. For probit/logit-style commands with auxiliary margins output, that lets marginal-effect captures like `sb_mem` or `sb_mfx` contaminate the main `regcoef` parcel. That explains why your main `regcoef` for runid 5 looks like a marginal effect (`0.0531`) instead of a coefficient-scale probit/logit estimate.

Third, `regcoef_rb` is currently on the underlying probit-coefficient scale, while your Stata-side `regcoef` for runid 5 is on a marginal-effect scale. Those are not comparable objects. In current Stata, `logit` reports coefficients by default, while `logistic` reports the same fitted model in odds-ratio form; the `or` option changes only the display, not the estimation. Current `probit` also supports factor-variable notation and reports the usual coefficient / SE / z / p / CI table. ([Stata][1])

So the conceptual fix is:

1. Never harvest `parmest`, `ereturn`, or margins output after a failed estimation command.
2. Treat `dprobit` as a legacy front-end to an underlying probit model.
3. Use the underlying probit coefficients as the canonical `sb` object for comparison with R.
4. Store marginal effects in a separate variant, not in the main `regcoef` parcel.

That design is also the one most consistent with the current Stata docs: `probit` and `logit` are the stable estimation commands with factor-variable support, and the coefficient table is the canonical output; transformed displays or postestimation quantities should be kept separate. ([Stata][1])

I would fix it in three places.

!MODIFICATION injection.reg in inject_reg_stata.R
scope = "function"
file = "inject_reg_stata.R"
function_name = "injection.reg"
description = "Prevent stale e() results from being harvested after a failed regression command by storing _rc immediately and gating the regression extraction block."
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
# Will be called from repboxStata
#
# Inject regression specific information
injection.reg = function(txt, lines=seq_along(txt),do, opts=rbs.opts()) {
  restore.point("injection.reg")

  repbox.dir = file.path(do$project_dir,"repbox/stata")
  res.dir = file.path(repbox.dir,"tsv")

  res.files = paste0(res.dir,"/",do$donum,"_",  lines,"_`repbox_local_cmd_count'",".dta")

  paste0('
local repbox_reg_rc = _rc
', end.injection(do$donum, lines, "RUNCMD",do),'
* REGRESSION INJECTION START
if (`repbox_reg_rc\' == 0) {
  parmest, label saving("', res.files,'", replace)

',post.injection(txt,lines,do=do, report.xtset=TRUE),'
  display "#~# INJECT REG_ERETURN ', do$donum,' ', lines,' `repbox_local_cmd_count\'"
  ereturn list
  display "#~# END INJECT REG_ERETURN ',do$donum,' ', lines,' `repbox_local_cmd_count\'"
}
else {
  display "#~# REGRESSION COMMAND FAILED ',do$donum,' ', lines,' `repbox_local_cmd_count\' rc=`repbox_reg_rc\'"
}

* REGRESSION INJECTION END
')
}
```

!END_MODIFICATION injection.reg in inject_reg_stata.R

That change is the most important one. It stops the stale-logit contamination you see in `regcoef_so`.

Then I would fix the metaregBase Stata runner itself so it also never stores stale postestimation output, and so `dprobit` is canonicalized to `probit` plus separate margins capture.

!MODIFICATION mrb_code_reg_stata in mrb_stata.R
scope = "function"
file = "mrb_stata.R"
function_name = "mrb_code_reg_stata"
description = "Skip all postestimation capture after failed commands, and canonicalize dprobit to probit for the main sb coefficient capture while storing marginal effects separately."
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
mrb_code_reg_stata = function(code_df, run_df=NULL, outdir=NULL,runid = code_df$runid, capture=TRUE, ...) {
  restore.point("mrb_code_reg_stata")
  library(repboxStata)
  stata_code = code_df$code

  if (!dir.exists(outdir))
    dir.create(outdir, recursive = TRUE)

  outfile = paste0(outdir, "/reg_", runid, "__sb.dta")
  scalar_outfile = paste0(outdir, "/regscalar_", runid, "__sb.txt")
  macro_outfile = paste0(outdir, "/regmacro_", runid, "__sb.txt")

  if (capture) {
    cap_str = "capture noisily "
  } else {
    cap_str = ""
  }

  cmd = code_df$cmd

  # Canonicalize legacy dprobit to probit for the main coefficient capture.
  # This keeps sb on the same scale as the R-side probit translation and
  # avoids the legacy dprobit parser limitation with factor variables.
  main_stata_code = stata_code
  if (identical(cmd, "dprobit")) {
    main_stata_code = sub(
      "^([[:space:]]*)dprobit\\b",
      "\\1probit",
      stata_code,
      ignore.case = TRUE,
      perl = TRUE
    )
  }

  extra_code = ""

  # Store marginal effects in a separate variant, never in the main sb parcel.
  if (cmd %in% c(stata_cmds_with_margin(), "dprobit")) {
    extra_code = paste0(
'
  ', cap_str, 'margins, atmeans dydx(*) post
  ', cap_str, 'parmest, saving("', outdir, '/reg_', runid, '__sb_mfx.dta", replace)
'
    )
  } else if (cmd %in% stata_cmds_with_exp_coef()) {
    extra_code = paste0(
'
  ', cap_str, 'estout . using "', outdir,'/reg_', runid, '__sb_exp.tsv", cells("b se t p ci_l ci_u") replace eform
'
    )
  }

  code = paste0(
    'capture ereturn clear
',
    cap_str, main_stata_code, '
local repbox_reg_rc = _rc

if (`repbox_reg_rc\' == 0) {
  ', cap_str, 'parmest, label saving("',outfile,'", replace)
  ', cap_str, 'repbox_write_reg_scalars "', scalar_outfile,'"
  ', cap_str, 'repbox_write_reg_macros "', macro_outfile,'"
', extra_code, '
}
else {
  display as error "metaregBase: skipping postestimation capture for runid ', runid, ' because rc=`repbox_reg_rc\'"
}
'
  )
  code
}
```

!END_MODIFICATION mrb_code_reg_stata in mrb_stata.R

That makes the Stata side sane again:

* failed commands do not create fake parcels,
* `sb` stays on coefficient scale,
* marginal effects live in `sb_mfx`.

Finally, I would stop `mrb_run_r_base_step()` from mixing those variants together.

!MODIFICATION mrb_run_r_base_step in mrb_r_base.R
scope = "function"
file = "mrb_r_base.R"
function_name = "mrb_run_r_base_step"
description = "Restrict the main Stata coefficient parcel to the sb variant only, so marginal-effect variants do not leak into regcoef."
----------------------------------------------------------------------------------------------------------------------------------------

```r
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
  stata_ct_all = if (!is.null(mrb$stata_ct_sb)) mrb$stata_ct_sb %>% filter(runid == pid) else NULL
  stata_ct = if (!is.null(stata_ct_all) && NROW(stata_ct_all) > 0) {
    if ("variant" %in% names(stata_ct_all)) {
      stata_ct_all %>% filter(variant %in% c("", "sb"))
    } else {
      stata_ct_all
    }
  } else {
    NULL
  }

  stata_scalars = if (!is.null(mrb$stata_scalars)) {
    out = mrb$stata_scalars %>% filter(runid == pid)
    if ("variant" %in% names(out)) out = out %>% filter(variant %in% c("", "sb"))
    out
  } else NULL

  stata_macros = if (!is.null(mrb$stata_macros)) {
    out = mrb$stata_macros %>% filter(runid == pid)
    if ("variant" %in% names(out)) out = out %>% filter(variant %in% c("", "sb"))
    out
  } else NULL

  # 3. Load Data & Expand Syntax
  dat = repboxDRF::drf_get_data(pid, drf = mrb$drf)
  org_dat = dat
  cmdpart = cmdpart_expand_vars(cmdpart, colnames(dat))

  # 4. Extract Options, SE, and build initial regvar
  opts_df = cmdpart_to_opts_df(cmdpart)
  se_info = se_stata_to_repdb(cmd, opts_df)
  regvar = cmdpart_to_regvar(cmdpart, dat, opts_df, se_info)

  depvar = regvar$cterm[regvar$role == "dep"]

  # 5. Data Mutations & Stats
  ct_cterms = unique(c(depvar, regvar$var, regvar$cterm, regvar$ia_cterm)) %>% setdiff(c("(Intercept)",""))

  wide_dat_full = create_cterm_cols(
    dat,
    ct_cterms,
    timevar=xtvar$timevar,
    panelvar=xtvar$panelvar,
    tdelta=xtvar$tdelta
  )
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

  # A. REGCOEF (main Stata base coefficients only)
  if (!is.null(stata_ct) && nrow(stata_ct) > 0) {
    step_parcels$regcoef = ct_to_regcoef(stata_ct, variant = "sb", artid = mrb$artid)
  } else {
    step_parcels$regcoef = tibble()
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
  step_parcels$regxvar = make_regxvar(step_parcels$regvar, wide_dat_full, step_parcels$regcoef)

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

!END_MODIFICATION mrb_run_r_base_step in mrb_r_base.R

What these three changes buy you:

* `regcoef_so` for failed commands stops being polluted by the previous successful model.
* `regcoef` for successful probit/logit/dprobit paths stops mixing coefficients with margins.
* `regcoef_rb` becomes comparable to `regcoef` again, because both are on coefficient scale.

I would also make one design decision explicit in your code comments and docs:

* `regcoef` should always mean coefficient-scale estimates.
* Marginal effects should always live in a separate parcel or at least a separate variant such as `sb_mfx` / `rb_mfx`.

That is the cleanest interpretation of the Stata outputs you asked about. `logit` and `probit` produce coefficient tables; `logistic` is just a different display of the same fitted logit model; transformed outputs should not overwrite the canonical coefficient parcel. ([Stata][1])

One more thing I would watch: you currently have two definitions of `mrb_run_r_base_step`, one in `mrb_agg_stata.R` and one in `mrb_r_base.R`. Even if only one wins at load time, that duplication is dangerous for debugging because it is easy to patch one and forget the other.

After these changes, your example should behave like this:

* original repbox run: line 5 is recorded as failed, with no fake `regcoef_so`;
* metaregBase run: `dprobit` is canonicalized to `probit` for `sb`;
* `regcoef` for runid 5 contains a proper coefficient table, not just one marginal-effect row;
* `regcoef_rb` and `regcoef` are now on the same scale, so coefficient comparison becomes meaningful.

[1]: https://www.stata.com/manuals/rlogit.pdf "https://www.stata.com/manuals/rlogit.pdf"
