# This file contains test utilities
# that are a variant of the test utilities in mrb_test_code_path
#
# Main idea: Our R translation of the data preparation steps (mainly by stata2r) may be imperfect and sometimes generate a different data set than the original Stata code.
# These helpers shall help to check whether the same data set is generated and
# if not at which runid the R translation generate different data sets than Stata
#
# This in turn shall help to better pinpoint the concrete bugs in stata2r that should be corrected.
#
# tdp stands for "test data path". we use it as abbreviation for our functions

#' @param max_dta_files max numbers of dta files that shall be written. If smaller than path split evenly but make sure that you store after the last data generation command.
#' @param include_runids if not NULL after those runids should definitly have a dta file
#' @param exclude_runids if not NULL don't save a dta file for those runids.
#' @param include_cmds if not NULL always save dta after Stata cmds in include_cmds
#' @param exclude_cmds if not NULL never save dta after Stata cmds in include_cmds
#' @param overwrite if FALSE don't overwrite existing dta files in dta dir, but if not for every data preparation step dta files exist try to fill up (but most max_dta_files additionally)

mrb_tdp_make_do = function(project_dir, pid, max_dta_files=10, include_runids=NULL, exclude_runids=NULL, include_cmds=NULL, exclude_cmds=NULL, overwrite=FALSE, drf=NULL, parcels=list()) {
  restore.point("mrb_tdp_make_do")

  if (is.null(drf)) drf = repboxDRF::drf_load(project_dir, parcels=parcels)

  outdir = paste0(project_dir, "/run/pid_", pid)
  if (!dir.exists(outdir)) dir.create(outdir, recursive=TRUE)
  outfile = file.path(outdir, paste0("tdp_", pid, ".do"))

  dta_dir = file.path(outdir, "dta")
  if (!dir.exists(dta_dir)) dir.create(dta_dir, recursive=TRUE)

  # Get the path for this pid
  path_df = drf$path_df %>% dplyr::filter(pid == !!pid, runid <= !!pid) %>% dplyr::arrange(runid)
  if (NROW(path_df) == 0) return(invisible(NULL))

  # Get the Stata code for this path
  # keep_non_mod_reg=FALSE normally drops the regression itself so we end at the last prep command
  sc_df = repboxDRF::drf_stata_code_df(drf, runids=pid, path_merge="none", keep_non_mod_reg=FALSE)
  if (NROW(sc_df) == 0) return(invisible(NULL))

  # Candidate runids are the ones in sc_df
  cands = sc_df$runid
  run_df_cands = drf$run_df %>% dplyr::filter(runid %in% cands)

  existing_files = list.files(dta_dir, pattern="\\.dta$")
  existing_runids = as.integer(tools::file_path_sans_ext(existing_files))

  to_save = integer(0)

  # Mandatory includes
  if (!is.null(include_runids)) to_save = union(to_save, intersect(cands, include_runids))
  if (!is.null(include_cmds)) {
    cmd_match = run_df_cands$runid[run_df_cands$cmd %in% include_cmds]
    to_save = union(to_save, cmd_match)
  }

  # Always include the last data generation command
  last_runid = max(cands)
  to_save = union(to_save, last_runid)

  # Exclusions
  if (!is.null(exclude_runids)) cands = setdiff(cands, exclude_runids)
  if (!is.null(exclude_cmds)) {
    cmd_match = run_df_cands$runid[run_df_cands$cmd %in% exclude_cmds]
    cands = setdiff(cands, cmd_match)
  }

  # Add existing runids to to_save if we don't overwrite, so they count towards max_dta_files limit
  if (!overwrite) {
    to_save = union(to_save, intersect(cands, existing_runids))
  } else {
    existing_runids = integer(0)
  }

  # Fill up remaining slots evenly
  num_to_select = max_dta_files - length(to_save)
  if (num_to_select > 0) {
    avail = setdiff(cands, to_save)
    if (length(avail) > 0) {
      if (length(avail) <= num_to_select) {
        to_save = union(to_save, avail)
      } else {
        idx = round(seq(1, length(avail), length.out=num_to_select))
        to_save = union(to_save, avail[idx])
      }
    }
  }

  # We only need to append Stata save code for runids we actively need to generate
  runids_to_generate = setdiff(to_save, existing_runids)

  if (length(runids_to_generate) > 0) {
    rows = match(runids_to_generate, sc_df$runid)
    # Forward slashes work cleanly in Stata across all OS
    save_cmds = paste0('\ncapture noisily save "', dta_dir, '/', runids_to_generate, '.dta", replace\n')
    sc_df$post[rows] = paste0(sc_df$post[rows], save_cmds)
  }

  # Write do file
  drf_code_write(sc_df, outfile)

  invisible(list(do_file=outfile, runids_to_test=to_save, runids_to_generate=runids_to_generate))
}


#' Make the R file similar to mrb_test_code_path but include the data comparisions, i.e. calls to mrb_tdp_compare_r_stata_data
#' We don't need to include the later R translation of the regression itself.
mrb_tdp_make_r = function(
  project_dir,
  pid,
  runids_to_test,
  drf = NULL,
  parcels = list(),
  load_dta = TRUE,
  stop_if_differ = FALSE,
  show_if_ok = FALSE
) {
  restore.point("mrb_tdp_make_r")

  if (is.null(drf)) drf = repboxDRF::drf_load(project_dir, parcels=parcels)

  outdir = paste0(project_dir, "/run/pid_", pid)
  if (!dir.exists(outdir)) dir.create(outdir, recursive=TRUE)
  outfile = file.path(outdir, paste0("tdp_", pid, ".R"))
  log_file = file.path(outdir, paste0("tdp_", pid, "_compare.log"))


  lines = c(
    paste0("# Test Data Path script for pid ", pid),
    paste0("project_dir = '", project_dir, "'"),
    paste0("pid = ", pid),
    "try(library(repboxRun), silent=TRUE)",
    "library(metaregBase)",
    "library(dplyr)",
    paste0('writeLines("","', log_file, '") # clear log'),
    "parcels = repboxDB::repdb_load_parcels(project_dir, c('stata_run_cmd', 'r_trans'))",
    "drf = repboxDRF::drf_load(project_dir, parcels=parcels)",
    "stata2r_env = stata2r::stata2r_env",
    "data = NULL",

    ""
  )

  sc_df = repboxDRF::drf_stata_code_df(drf, runids=pid, path_merge="none", keep_non_mod_reg=FALSE)

  cands = sc_df$runid
  run_df_cands = drf$run_df %>% dplyr::filter(runid %in% cands)

  for (i in seq_len(NROW(run_df_cands))) {
    r_id = run_df_cands$runid[i]
    stata_cmd = run_df_cands$cmdline[i]

    stata_cmd_lines = strsplit(stata_cmd, "\n", fixed=TRUE)[[1]]
    lines = c(lines, paste0("# [runid ", r_id, "] Stata: ", paste0(stata_cmd_lines, collapse = "\n#        ")))

    rcode = run_df_cands$rcode[i]

    if (i == 1 && isTRUE(run_df_cands$has_file_cache[i])) {
      drf_rel_path = paste0("cached_dta/", basename(run_df_cands$drf_cache_file[i]))
      rcode = paste0(
        'data = repboxDRF::drf_load_data(project_dir, "', drf_rel_path, '")\n',
        'data$stata2r_original_order_idx = seq_len(nrow(data))\n',
        'assign("has_original_order_idx", TRUE, envir = stata2r::stata2r_env)'
      )
    }

    if (is.null(rcode) || is.na(rcode) || !nzchar(rcode)) {
      rcode = "# No R translation found/needed"
    }

    lines = c(lines, rcode)

    if (r_id %in% runids_to_test) {
      lines = c(
        lines,
        paste0(
          "metaregBase:::mrb_tdp_compare_r_stata_data(",
          "data = data, ",
          "runid = ", r_id, ", ",
          "project_dir = project_dir, ",
          "pid = ", pid, ", ",
          "stata_cmd = ", deparse(stata_cmd), ", ",
          "r_code = ", deparse(rcode), ", ",
          "stop_if_differ = ", if (isTRUE(stop_if_differ)) "TRUE" else "FALSE", ", ",
          "show_if_ok = ", if (isTRUE(show_if_ok)) "TRUE" else "FALSE",
          ")"
        )
      )

      if (isTRUE(load_dta)) {
        lines = c(
          lines,
          paste0(
            "data = metaregBase:::mrb_tdp_load_dta(",
            "data = data, ",
            "project_dir = project_dir, ",
            "pid = ", pid, ", ",
            "runid = ", r_id,
            ")"
          )
        )
      }
    }

    lines = c(lines, "")
  }

  writeLines(paste0(lines, collapse="\n"), outfile)
  invisible(outfile)
}


mrb_tdp_compare_r_stata_data = function(
  data,
  runid,
  project_dir,
  pid,
  stata_cmd = NULL,
  r_code = NULL,
  stop_if_differ = FALSE,
  show_if_ok = FALSE
) {
  restore.point("mrb_tdp_compare_r_stata_data")

  outdir = file.path(project_dir, paste0("run/pid_", pid))
  log_file = file.path(outdir, paste0("tdp_", pid, "_compare.log"))
  dta_file = file.path(outdir, "dta", paste0(runid, ".dta"))

  cmd_msg = paste0(
    "\nrunid: ", runid, "\n",
    "Stata: ",
    if (is.null(stata_cmd) || is.na(stata_cmd)) "" else stata_cmd,
    "\n",
    "R: ",
    if (is.null(r_code) || is.na(r_code)) "" else r_code,
    "\n"
  )

  ok = FALSE
  diff_msg = ""

  make_result = function(ok, msg) {
    invisible(list(ok = ok, msg = msg))
  }

  if (!file.exists(dta_file)) {
    diff_msg = paste0(diff_msg, "Stata data file not found: ", dta_file, "\n")
  } else if (is.null(data) || !is.data.frame(data)) {
    diff_msg = paste0(diff_msg, "R data is NULL or not a data.frame.\n")
  } else {
    stata_data = try(haven::read_dta(dta_file), silent=TRUE)

    if (inherits(stata_data, "try-error")) {
      diff_msg = paste0(diff_msg, "Failed to load Stata data file.\n")
    } else {
      nrow_r = nrow(data)
      ncol_r = ncol(data)
      nrow_s = nrow(stata_data)
      ncol_s = ncol(stata_data)

      if (nrow_r != nrow_s) {
        diff_msg = paste0(
          diff_msg,
          sprintf(
            "Rows / cols differ: R has %d rows, %d cols; Stata has %d rows, %d cols.\n",
            nrow_r, ncol_r, nrow_s, ncol_s
          )
        )
      }

      r_cols = setdiff(colnames(data), c("stata2r_original_order_idx", ".ORG.ROW", ".ROW"))
      s_cols = colnames(stata_data)

      missing_in_r = setdiff(s_cols, r_cols)
      extra_in_r = setdiff(r_cols, s_cols)

      if (length(missing_in_r) > 0) {
        diff_msg = paste0(
          diff_msg,
          "Columns in Stata but missing in R: ",
          paste(missing_in_r, collapse=", "),
          "\n"
        )
      }

      if (length(extra_in_r) > 0) {
        diff_msg = paste0(
          diff_msg,
          "Columns in R but missing in Stata: ",
          paste(extra_in_r, collapse=", "),
          "\n"
        )
      }

      common_cols = intersect(r_cols, s_cols)

      if (nrow_r == nrow_s && length(common_cols) > 0) {
        diff_cols = character(0)

        for (col in common_cols) {
          val_r = data[[col]]
          val_s = stata_data[[col]]

          if (is.character(val_r) || is.character(val_s) || is.factor(val_r) || is.factor(val_s)) {
            char_r = as.character(val_r)
            char_s = as.character(val_s)

            same = identical(is.na(char_r), is.na(char_s)) &&
              isTRUE(all(char_r[!is.na(char_r)] == char_s[!is.na(char_s)]))
          } else {
            num_r = as.numeric(val_r)
            num_s = as.numeric(val_s)

            same = isTRUE(all.equal(
              num_r,
              num_s,
              tolerance = 1e-5,
              check.attributes = FALSE
            ))
          }

          if (!same) {
            diff_cols = c(diff_cols, col)
          }
        }

        if (length(diff_cols) > 0) {
          diff_msg = paste0(
            diff_msg,
            "Values differ in columns: ",
            paste(diff_cols, collapse=", "),
            "\n"
          )
        }
      } else if (nrow_r != nrow_s) {
        diff_msg = paste0(
          diff_msg,
          "Skipped column value comparison because row counts differ.\n"
        )
      }

      ok = !nzchar(diff_msg)
    }
  }

  if (ok) {
    msg = paste0(cmd_msg, "OK\n")
  } else {
    msg = paste0(cmd_msg, "DATA DIFFER:\n", diff_msg)
  }


  if (!ok | show_if_ok) {
    cat(msg, file=log_file, append=TRUE)
    cat(msg)
  }
  if (ok) {
    return(make_result(TRUE, msg))
  }
  if (stop_if_differ) {
    stop(msg, call.=FALSE)
  }

  make_result(FALSE, msg)
}


#' Main function
#' create do and r file
#' run the do file to create dta files and also run the R files
#' to create logs
#' make somewhat verbose so we see understandable output on console
mrb_test_data_path = function(
  project_dir,
  pid,
  max_dta_files = 100,
  include_runids = NULL,
  exclude_runids = NULL,
  include_cmds = NULL,
  exclude_cmds = NULL,
  overwrite = FALSE,
  run_do = TRUE,
  run_r = TRUE,
  load_dta = TRUE,
  stop_if_differ = FALSE,
  show_if_ok = FALSE,
  drf = NULL,
  parcels = list()
) {
  restore.point("mrb_test_data_path")

  try(library(repboxRun), silent=TRUE)
  library(repboxDRF)
  library(metaregBase)

  mrb_deploy_test_pid_f2p_tomls(project_dir, pid, overwrite=FALSE)
  mrb_print_test_parcels(project_dir, pid)

  if (is.null(drf)) {
    drf = repboxDRF::drf_load(project_dir, parcels=parcels)
  }

  cat(sprintf("\nGenerating Stata data path Do-file for pid %d...\n", pid))
  do_res = mrb_tdp_make_do(
    project_dir = project_dir,
    pid = pid,
    max_dta_files = max_dta_files,
    include_runids = include_runids,
    exclude_runids = exclude_runids,
    include_cmds = include_cmds,
    exclude_cmds = exclude_cmds,
    overwrite = overwrite,
    drf = drf,
    parcels = parcels
  )

  if (is.null(do_res)) {
    cat(sprintf("No Stata data preparation path found for pid %d.\n", pid))
    return(invisible(NULL))
  }

  do_file = do_res$do_file
  runids_to_test = do_res$runids_to_test
  runids_to_generate = do_res$runids_to_generate

  if (run_do) {
    if (length(runids_to_generate) > 0) {
      cat(sprintf("Running Stata do-file to generate %d newly needed dta files...\n", length(runids_to_generate)))
      repboxStata::run_stata_do(do_file, nostop=TRUE)
    } else {
      cat("All requested .dta files already exist. Skipping Stata execution.\n")
    }
  }

  cat(sprintf("\nGenerating R data path test script for pid %d...\n", pid))
  r_file = mrb_tdp_make_r(
    project_dir = project_dir,
    pid = pid,
    runids_to_test = runids_to_test,
    drf = drf,
    parcels = parcels,
    load_dta = load_dta,
    stop_if_differ = stop_if_differ,
    show_if_ok = show_if_ok
  )

  if (run_r) {
    cat("Running R test script and comparing datasets...\n")
    test_env = new.env(parent=globalenv())
    tryCatch({
      source(r_file, local=test_env, echo=FALSE)
    }, error = function(e) {
      cat("\nError while executing R script:\n")
      print(e)
    })
  }

  invisible(list(do_file = do_file, r_file = r_file))
}

mrb_tdp_load_dta = function(data, project_dir, pid, runid) {
  restore.point("mrb_tdp_load_dta")

  outdir = file.path(project_dir, paste0("run/pid_", pid))
  dta_file = file.path(outdir, "dta", paste0(runid, ".dta"))

  if (!file.exists(dta_file)) {
    return(data)
  }

  dta_data = try(haven::read_dta(dta_file), silent=TRUE)

  if (inherits(dta_data, "try-error")) {
    warning("Could not load Stata dta file: ", dta_file, call.=FALSE)
    return(data)
  }

  dta_data
}

mrb_deploy_test_pid_f2p_tomls = function(project_dir, pid, overwrite = FALSE) {
  outdir = paste0(project_dir, "/run/pid_", pid)

  tpl_dir = system.file("tpl", package = "metaregBase")

  tpl_files = list.files(tpl_dir, glob2rx("tpl_f2p_test*.toml"), full.names = TRUE)

  tpl_file = tpl_files[1]

  for (tpl_file in tpl_files) {
    outbase = basename(tpl_file) %>%
      stri_replace_first_fixed("tpl_","") %>%
      stri_replace_first_fixed(".toml",paste0("_",pid,".toml"))
    outfile = file.path(outdir, outbase)
    if (!overwrite & file.exists(outfile)) next

    txt = read_utf8(tpl_file)
    txt = stringi::stri_replace_all_fixed(txt, "{{pid}}", pid)
    txt = stringi::stri_replace_all_fixed(txt, "{{project_dir}}", project_dir)
    writeUtf8(txt,outfile)
  }


}

mrb_print_test_parcels = function(project_dir, pid) {
  outdir = paste0(project_dir, "/run/pid_", pid)

  mrb_print_parcels(project_dir,runid = pid,outfile = file.path(outdir, paste0("parcels_", pid,".txt")))
}

mrb_copy_cached_runid_to_test_project = function(project_dir, runid) {

  project_dir = "~/repbox/projects/ms_67_4_18"
  runid = 128


  cache_file = paste0(project_dir, "/drf/cached_dta/",runid, "_cache.dta")

  if (!file.exists(cache_file)) {
    stop("No cache file exists")
  }


  artid = basename(project_dir)

  dest_dir = "~/repbox/projects_test/test/org/code"
  dest_data_file = file.path(dest_dir, paste0(artid, "_", runid, ".dta"))
  file.copy(cache_file, dest_data_file)


  dest_do_file = file.path(dest_dir, paste0(artid, "_", runid, ".do"))
  parcels = repdb_load_parcels(project_dir, "stata_run_cmd")
  run_df = parcels$stata_run_cmd

  code = c(
    paste0('use "', basename(dest_data_file),'", clear'),
    run_df$cmdline[run_df$runid==runid]
  )
  writeLines(code, dest_do_file)

}

