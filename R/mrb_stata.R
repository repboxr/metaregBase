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


mrb_run_stata_script = function(mrb, do_file = mrb$stata_do_file, nostop=TRUE, timeout=mrb$stata_timeout) {
  restore.point("mrb_run_stata_script")
  if (is.null(do_file) | !file.exists(do_file)) {
    stop("No existing do file specified.")
  }

  if (is.null(timeout)) timeout = 60*60*2

  # Clear previous timeout problems before the run
  try(mrb_remove_problems(mrb$project_dir, "mrb_stata_timeout"), silent = TRUE)

  library(repboxStata)
  res = run_stata_do(do_file, nostop = nostop, timeout = timeout)

  if (isTRUE(res$timeout)) {
    msg = paste0("Timeout (", timeout, "s) during mrb_run_stata_script for ", basename(do_file), ".")

    last_runid = NA_integer_
    outdir = file.path(mrb$mrb_dir, "stata_reg_out")
    if (dir.exists(outdir)) {
      files = list.files(outdir, pattern="reg_[0-9]+__sb\\.dta")
      if (length(files) > 0) {
        runids = as.integer(stringi::stri_match_first_regex(files, "reg_([0-9]+)__sb\\.dta")[,2])
        last_runid = max(runids, na.rm=TRUE)
      }
    }

    cache_dir = file.path(mrb$project_dir, "drf/cached_dta")
    last_cache = NA_integer_
    if (dir.exists(cache_dir)) {
      files = list.files(cache_dir, pattern="[0-9]+_cache\\.dta")
      if (length(files) > 0) {
        runids = as.integer(stringi::stri_match_first_regex(files, "([0-9]+)_cache\\.dta")[,2])
        last_cache = max(runids, na.rm=TRUE)
      }
    }

    extra = ""
    if (!is.na(last_runid)) {
      extra = paste0(" Last generated runid in stata_reg_out is ", last_runid, ".")
    }
    if (!is.na(last_cache)) {
      extra = paste0(extra, " Last generated cache in drf/cached_dta is ", last_cache, ".")
    }

    repboxUtils::repbox_problem(
      msg = paste0(msg, extra),
      type = "mrb_stata_timeout",
      project_dir = mrb$project_dir,
      fail_action = "msg"
    )
  }

  mrb
}

mrb_check_stata_reg_out_complete = function(mrb) {
  path_df = mrb$drf$path_df
  pids = unique(path_df$pid)

  length(pids)


  reg_runids = unique(code_df$pid)

}


mrb_stata_always_cache_commands = function() {
  "xi"
}


# custom caches are caches we add due to a positive speed / cache size trade-off. The trade-off parameters are defined in mrb and we use the heuristic in drf_suggest_cache_runids

mrb_find_custom_cache_runids = function(mrb, cache_cmds = mrb_stata_always_cache_commands()) {
  restore.point("mrb_find_custom_cache_runids")
  drf = mrb$drf

  # Consider the caches we add due to cache_cmds
  extra_caches = drf$run_df$runid[drf$run_df$cmd %in% cache_cmds]


  cache_runids = repboxDRF::drf_suggest_cache_runids(drf,max_caches = mrb$custom_max_caches,min_score = mrb$custom_cache_min_score,extra_caches = extra_caches)
  cache_runids
}

mrb_adopath_injection_code = function(project_dir) {
  restore.point("mrb_adopath_injection_code")
  drf_ado_dir = file.path(project_dir, "drf", "ado")

  if (dir.exists(drf_ado_dir)) {
    ado_files = list.files(drf_ado_dir, glob2rx("*.ado"), full.names = TRUE, recursive = TRUE)
  } else {
    ado_files = character(0)
  }

  extra_ado_dirs = repboxStata::get_ado_dirs()
  ado_dirs = unique(c(dirname(ado_files), extra_ado_dirs))

  if (length(ado_dirs) == 0) return("")

  plus.dir = extra_ado_dirs["plus"]
  personal.dir = extra_ado_dirs["personal"]

  code = ""
  if (!is.na(plus.dir)) {
    ado_dirs = setdiff(ado_dirs, plus.dir)
    code = paste0(code, 'sysdir set PLUS "', plus.dir,'"\n')
  }
  if (!is.na(personal.dir)) {
    ado_dirs = setdiff(ado_dirs, personal.dir)
    code = paste0(code, 'sysdir set PERSONAL "', personal.dir,'"\n')
  }

  if (length(ado_dirs) > 0) {
    ado_dirs = gsub("\\\\", "/", ado_dirs)
    code = paste0(code, paste0('adopath + "', rev(ado_dirs), '"', collapse = "\n"))
  }
  code
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
  cache_cmds = mrb_stata_always_cache_commands() # "xi"
  cache_runids = mrb_find_custom_cache_runids(mrb, cache_cmds)

  code_df = repboxDRF::drf_stata_code_df(drf=mrb$drf,cache_after_cmd = cache_cmds,cache_after_runids = cache_runids)

  code_df = code_df %>%
    drf_code_adapt(mrb_code_reg_stata, just_path_pos="end", run_df=run_df, outdir=outdir, capture=capture) %>%
    drf_code_stata_path_header()

  script_file = file.path(mrb$mrb_dir, "stata_code/mrb_stata.do")
  header_code = mrb_adopath_injection_code(mrb$project_dir)
  drf_code_write(code_df, script_file, header_code = header_code)

  mrb$stata_code_df = code_df
  mrb$stata_do_file = script_file
  mrb
}

drf_code_write = function(code_df, file, header_code = "") {
  restore.point("drf_code_write")
  dir = dirname(file)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  if (has_col(code_df,"scalar_stata_code")) {
    code_df$pre = paste0(na.val(code_df$scalar_stata_code,""),code_df$pre)
  }
  txt = paste0(code_df$pre, code_df$code, code_df$post, collapse="\n")
  if (nzchar(header_code)) {
    txt = paste0(header_code, "\n", txt)
  }
  tryCatch(write_utf8(txt, file), error = function(e) writeLines(enc2utf8(txt), file))
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
  capture estimates store repbox_orig_model
  ', cap_str, 'margins, atmeans dydx(*) post
  ', cap_str, 'parmest, saving("', outdir, '/reg_', runid, '__sb_mfx.dta", replace)
  capture quietly estimates restore repbox_orig_model
  capture quietly estimates drop repbox_orig_model
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

# Don't add ereturn clear:  not needed and not compatible with e(sample) stuff in if condition
#    'capture ereturn clear
#',
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

