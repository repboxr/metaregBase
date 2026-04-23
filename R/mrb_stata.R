# New attempt metareg base: compute Stata code
# for repboxDRF generated Stata file

example = function() {
  # Should point to this project dir
  project_dir = "~/repbox/projects/aejapp_11_2_10"

  if (FALSE)
    rstudioapi::filesPaneNavigate(project_dir)

  drf = drf_load(project_dir)
  mrb = mrb_init(project_dir, drf=drf)
  #mrb = mrb_make_cmdpart_parcel(mrb)
  mrb = mrb_full_stata_script(mrb, capture = FALSE)
  mrb = mrb_run_stata_script(mrb)
}


mrb_run_stata_script = function(mrb, do_file = mrb$stata_do_file) {
  restore.point("mrb_run_stata_script")
  if (is.null(do_file) | !file.exists(do_file)) {
    stop("No existing do file specified.")
  }
  library(repboxStata)
  run_stata_do(do_file, nostop = FALSE)
  mrb
}

mrb_check_stata_reg_out_complete = function(mrb) {
  path_df = mrb$drf$path_df
  pids = unique(path_df$pid)

  length(pids)


  reg_runids = unique(code_df$pid)

}


mrb_full_stata_script = function(mrb, capture=TRUE) {
  restore.point("mrb_full_stata_script")
  run_df = mrb$drf$run_df

  path_merge = c("load_natural")
  outdir = file.path(mrb$mrb_dir, "stata_reg_out")

  if (dir.exists(outdir)) {
    old_files = list.files(outdir, full.names = TRUE)
    if (length(old_files) > 0) file.remove(old_files)
  } else {
    dir.create(outdir, recursive = TRUE)
  }

  # We want to inject caches after some commands that cannot be effectively translated
  # to R.
  # Currently that is xi as it is hard to find the same ordering of generated
  # dummy variables as Stata
  cache_cmds = "xi"

  code_df = repboxDRF::drf_stata_code_df(drf=mrb$drf,cache_after_cmd = cache_cmds)
  code_df = code_df %>%
    drf_code_adapt(mrb_code_reg_stata, just_path_pos="end", run_df=run_df, outdir=outdir, capture=capture) %>%
    drf_code_stata_path_header()

  script_file = file.path(mrb$mrb_dir, "stata_code/mrb_stata.do")
  drf_code_write(code_df, script_file)
  mrb$stata_code_df = code_df
  mrb$stata_do_file = script_file
  mrb
}

drf_code_write = function(code_df, file) {
  restore.point("drf_code_write")
  dir = dirname(file)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  if (has_col(code_df,"scalar_stata_code")) {
    code_df$pre = paste0(na.val(code_df$scalar_stata_code,""),code_df$pre)
  }
  txt = paste0(code_df$pre, code_df$code, code_df$post, collapse="\n")
  write_utf8(txt, file)
  invisible(txt)
}

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



mrb_clear_stata_reg_out = function(project_dir, runids=NULL) {
  reg_out_dir = file.path(project_dir, "metareg/base/stata_reg_out")
  del_files = list.files(reg_out_dir, "^.*\\.(dta|txt|csv|tsv)$",full.names = TRUE)
  if (!is.null(runids)) {
    pattern = paste0("(",paste0("_", runids, "_", collapse="|"),")")
    use = stri_detect_regex(del_files, pattern)
    del_files = del_files[use]
  }
  file.remove(del_files)

}

