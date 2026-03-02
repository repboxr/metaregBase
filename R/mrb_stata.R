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
  code_df = repboxDRF::drf_stata_code_df(drf=mrb$drf)
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
  dir = dirname(file)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
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

  extra_code = ""
  outfile = paste0(outdir, "/reg_", runid, "__sb.dta")
  scalar_outfile = paste0(outdir, "/regscalar_", runid, "__sb.txt")
  macro_outfile = paste0(outdir, "/regmacro_", runid, "__sb.txt")

  if (capture) {
    cap_str = "capture noisily "
  } else {
    cap_str = ""
  }


  cmd = code_df$cmd
  # Command for which marginal effects are stored
  if (cmd %in% stata_cmds_with_margin()) {
    extra_code = paste0('
', cap_str, 'margins, atmeans dydx(*) post
', cap_str, 'parmest, saving("',outdir, "/reg_", runid, "__sb_mem.dta",'")
', cap_str, 'repbox_write_reg_scalars "',outdir, "/regscalar_", runid, "__sb_mem.txt",'"
', cap_str, 'repbox_write_reg_macros "',outdir, "/regmacro_", runid, "__sb_mem.txt",'"
')
  } else if (cmd == "dprobit") {
    extra_code = paste0(cap_str, '
repbox_write_dprobit_coef_se "',outdir, "/dprobit_", runid, ".csv\n")
  } else if (cmd %in% stata_cmds_with_exp_coef()) {
    extra_code = paste0(cap_str, ' estout . using "', outdir,'/reg_', runid, '__sb_exp.tsv", cells("b se t p ci_l ci_u") replace eform\n')
  }

  code = paste0(
    cap_str, stata_code,
    '\n
', cap_str, 'parmest, label saving("',outfile,'", replace)
', cap_str, 'repbox_write_reg_scalars "', scalar_outfile,'"
', cap_str, 'repbox_write_reg_macros "', macro_outfile,'"
', extra_code
  )
  code
}
