I completely understand your concern. Function-scope modifications are indeed much safer to prevent accidental overwrites of unrelated parts of the file. 

Here are the exact same changes, meticulously broken down into `function` scope modifications for targeted, precise injections.

!MODIFICATION drf_add_dep_df repboxDRF/R/drf_deps.R
scope = "function"
file = "/home/rstudio/repbox/repboxDRF/R/drf_deps.R"
function_name = "drf_add_dep_df"
description = "Update drf_add_dep_df to capture the exact macro_name required for downstream dependency caching."
---
```r
#' Compute r(), e() or xi dependencies between run_df commands
drf_add_dep_df = function(drf) {
  restore.point("drf_make_deps_df")
  run_df = drf$run_df
  cmdlines = run_df$cmdline
  run_df$code = run_df$cmdline

  # e() dependencies (via regression commands)
  make_df_e = run_df %>%
    filter(cmd_type %in% c("reg","quasi_reg")) %>%
    select(source_runid=runid)

  e_matches = stringi::stri_extract_all_regex(run_df$cmdline, "\\be\\([a-zA-Z0-9_]+\\)")
  edep_list = lapply(seq_along(e_matches), function(i) {
    if (is.na(e_matches[[i]][1])) return(NULL)
    data.frame(runid = run_df$runid[i], dep_type = "e", macro_name = e_matches[[i]], stringsAsFactors = FALSE)
  })
  edep_df = bind_rows(edep_list)
  if (NROW(edep_df) > 0) {
    edep_df = edep_df %>% distinct() %>% left_join(make_df_e, by = join_by(closest(runid > source_runid)), relationship = "many-to-one")
  } else {
    edep_df = data.frame(runid=integer(), source_runid=integer(), dep_type=character(), macro_name=character())
  }

  # r() dependencies (via commands like summarize)
  make_df_r = run_df %>%
    filter(cmd %in% stata_make_r_cmds()) %>%
    select(source_runid=runid)

  r_matches = stringi::stri_extract_all_regex(run_df$cmdline, "\\br\\([a-zA-Z0-9_]+\\)")
  rdep_list = lapply(seq_along(r_matches), function(i) {
    if (is.na(r_matches[[i]][1])) return(NULL)
    data.frame(runid = run_df$runid[i], dep_type = "r", macro_name = r_matches[[i]], stringsAsFactors = FALSE)
  })
  rdep_df = bind_rows(rdep_list)
  if (NROW(rdep_df) > 0) {
    rdep_df = rdep_df %>% distinct() %>% left_join(make_df_r, by = join_by(closest(runid > source_runid)), relationship = "many-to-one")
  } else {
    rdep_df = data.frame(runid=integer(), source_runid=integer(), dep_type=character(), macro_name=character())
  }

  # xi dependencies
  makes_xi = run_df$cmd == "xi"
  rows = which(run_df$cmd_type %in%  c("reg","quasi_reg"))
  mxi = stringi::stri_detect_regex(run_df$cmdline[rows], "\\bxi\\:")
  makes_xi[rows[mxi]] = TRUE

  make_df_xi = data.frame(source_runid = run_df$runid[makes_xi])

  xidep_df = run_df %>%
    filter(stringi::stri_detect_regex(cmdline, "\\b_I[a-zA-Z0-9_]+")) %>%
    select(runid) %>%
    mutate(dep_type = "xi", macro_name = "xi") %>%
    left_join(make_df_xi, by = join_by(closest(runid > source_runid)), relationship = "many-to-one")

  drf$dep_df = bind_rows(edep_df, rdep_df, xidep_df) %>% filter(!is.na(source_runid))
  
  outfile = file.path(drf$project_dir,"drf/dep_df.Rds")
  save_rds_create_dir(drf$dep_df, outfile)

  drf
}
```
!END_MODIFICATION drf_add_dep_df repboxDRF/R/drf_deps.R

!MODIFICATION drf_add_code_store_e_r repboxDRF/R/drf_stata_code.R
scope = "function"
file = "/home/rstudio/repbox/repboxDRF/R/drf_stata_code.R"
insert_before_fun = "drf_stata_code_df"
description = "Inject a new helper function that writes Stata ADO instructions to save dependencies locally."
---
```r
drf_add_code_store_e_r = function(run_df, dep_df, project_dir, overwrite_e_r = FALSE) {
  restore.point("drf_add_code_store_e_r")
  if (is.null(dep_df) || NROW(dep_df) == 0) return(run_df)

  e_r_deps = dep_df %>% dplyr::filter(dep_type %in% c("e", "r"), !is.na(source_runid)) %>%
    dplyr::select(source_runid, dep_type, macro_name) %>% dplyr::distinct()

  if (NROW(e_r_deps) == 0) return(run_df)

  outdir = file.path(project_dir, "drf", "stata_e_r")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  for (i in seq_len(NROW(e_r_deps))) {
    s_runid = e_r_deps$source_runid[i]
    m_name = e_r_deps$macro_name[i]
    
    prefix = substr(m_name, 1, 1) # 'e' or 'r'
    inner = gsub("^[er]\\(|\\)$", "", m_name)

    if (m_name == "e(sample)") {
      outfile = file.path(outdir, paste0(prefix, "_", s_runid, "_", inner, ".dta"))
      if (!overwrite_e_r && file.exists(outfile)) next
      save_code = paste0(
        "\n* Save e(sample) for downstream R dependency\n",
        "capture {\n",
        "  preserve\n",
        "  gen __esample = e(sample)\n",
        "  keep __esample\n",
        "  save \"", outfile, "\", replace\n",
        "  restore\n",
        "}\n"
      )
    } else {
      outfile = file.path(outdir, paste0(prefix, "_", s_runid, "_", inner, ".txt"))
      if (!overwrite_e_r && file.exists(outfile)) next
      save_code = paste0(
        "\n* Save macro for downstream R dependency\n",
        "capture file close _er_file\n",
        "capture file open _er_file using \"", outfile, "\", write replace\n",
        "capture file write _er_file (string(", m_name, "))\n",
        "capture file close _er_file\n"
      )
    }

    row_idx = match(s_runid, run_df$runid)
    if (!is.na(row_idx)) {
      run_df$code[row_idx] = paste0(run_df$code[row_idx], save_code)
    }
  }
  run_df
}
```
!END_MODIFICATION drf_add_code_store_e_r repboxDRF/R/drf_stata_code.R

!MODIFICATION drf_stata_code_df repboxDRF/R/drf_stata_code.R
scope = "function"
file = "/home/rstudio/repbox/repboxDRF/R/drf_stata_code.R"
function_name = "drf_stata_code_df"
description = "Update drf_stata_code_df to trigger the e/r Stata dependency writes, honoring the new arguments."
---
```r
drf_stata_code_df = function(drf,runids=NULL, path_merge = c("none", "load", "natural", "load_natural")[4], cache_after_runids = drf$cache_after_runids, cache_after_cmd=drf$cache_after_cmd, write_e_r = TRUE, overwrite_e_r = FALSE) {
  restore.point("drf_stata_code_skel")
  project_dir = drf$project_dir
  pids = runids
  path_df = drf$path_df
  if (!is.null(pids)) {
    path_df = path_df %>%
      filter(pid %in% pids)
  }
  pids = unique(path_df$pid)
  if (length(pids)<=1) path_merge = "none"

  restore_code = function(data_path) {
    paste0("* Restore previously loaded data set", basename(data_path), "\nframe copy cache_frame default, replace")
  }
  preserve_code = function() {
    "\nframe copy default cache_frame, replace"
  }
  update_rdf_cache_code = function(rdf) {
    rdf$pre = rep("", NROW(rdf))
    if (isTRUE(rdf$has_file_cache[1]) & NROW(rdf) > 1) {
         rdf$code[1] = paste0('use "', rdf$drf_cache_file[1], '", clear')
    } else if (isTRUE(rdf$has_file_cache[1])) {
      # load file cache and keep code that runs regression
      rdf$pre[1] = paste0('use "', rdf$drf_cache_file[1], '", clear\n\n')
    }
    rdf
  }

  run_df = drf$run_df
  run_df = run_df %>% semi_join(path_df, by="runid")

  if (!has_col(run_df, "aux_cmd_type")) {
    run_df$aux_cmd_type = rep("", NROW(run_df))
  }

  run_df$code = ifelse(is.na(run_df$ok) | run_df$ok, run_df$cmdline, paste0("capture noisily ", run_df$cmdline))

  run_df = drf_replace_run_df_code_data_path(run_df = run_df, drf=drf)

  # Only relevant for 1st element in paths
  run_df$data_path = ifelse(is.na(run_df$drf_cache_file) | run_df$drf_cache_file=="", run_df$org_data_path, run_df$drf_cache_file)

  run_df = drf_code_stata_add_save_cache(project_dir = project_dir, run_df=run_df,cache_after_runids = cache_after_runids, cache_after_cmd = cache_after_cmd)

  if (write_e_r) {
    run_df = drf_add_code_store_e_r(run_df, drf$dep_df, project_dir, overwrite_e_r)
  }

  path_li = split(path_df, path_df$pid)
  code_li = NULL
  pid = pids[1]
  if (path_merge == "none") {
    code_li = lapply(pids, function(pid) {
      pdf = path_li[[as.character(pid)]]
      pdf = drf_remove_non_mod_reg_from_path_df(pdf, drf)

      rdf = run_df[run_df$runid %in% pdf$runid, ]
      rdf = update_rdf_cache_code(rdf)
      rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre=pre, post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type=na.val(aux_cmd_type,""))
    })
    sc_df = bind_rows(code_li)
    # we now add scalar definitions from scalar map
    sc_df = sc_df %>%
      left_join(drf$scalar_code, by="runid") %>%
      mutate(
        scalar_stata_code = na.val(scalar_stata_code,""),
        scalar_r_code = na.val(scalar_r_code,"")
      )

    return(sc_df)
  }
  ps_df = path_df %>%
    group_by(pid) %>%
    summarize(
      first_runid = min(runid),
      last_runid = max(runid),
    ) %>%
    left_join(run_df %>% select(first_runid=runid, data_path), by="first_runid")

  data_df = ps_df %>%
    group_by(data_path) %>%
    summarize(
      data_runid = min(first_runid),
      data_num_paths = n()
    ) %>%
    arrange(data_runid)

  ps_df = ps_df %>%
    left_join(data_df, by="data_path")

  merge_load = path_merge %in% c("load", "load_natural")
  merge_natural = path_merge %in% c("natural", "load_natural")

  if (merge_load) {
    ps_df = ps_df %>%
      ungroup() %>%
      arrange(data_runid, first_runid, last_runid) %>%
      mutate(
        restore_data = is.true(lag(data_runid)==data_runid),
        preserve_data = !restore_data & data_num_paths > 1
      )

  } else {
    ps_df = ps_df %>%
      arrange(first_runid, last_runid) %>%
      mutate(restore_data = FALSE, preserve_data=FALSE)
  }

  pids = ps_df$pid
  if (!merge_natural) {
    code_li = lapply(pids, function(pid) {
      pdf = path_li[[as.character(pid)]]
      pdf = drf_remove_non_mod_reg_from_path_df(pdf, drf)
      rdf = run_df[run_df$runid %in% pdf$runid, ]
      rdf = update_rdf_cache_code(rdf)

      rdf = rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre=pre, post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type="", clear=FALSE)

      ps = ps_df[ps_df$pid==pid,]
      if (ps$preserve_data) {
        rdf$code[1] = paste0(rdf$code[1],preserve_code())
        rdf$aux_cmd_type[1] = paste0("load_preserve")
      } else if (ps$restore_data) {
        rdf$code[1] = restore_code(ps$data_path)
        rdf$aux_cmd_type[1] = paste0("restore")
      }
      rdf
    })
    sc_df = bind_rows(code_li)
    # we now add scalar definitions from scalar map
    sc_df = sc_df %>%
      left_join(drf$scalar_code, by="runid") %>%
      mutate(
        scalar_stata_code = na.val(scalar_stata_code,""),
        scalar_r_code = na.val(scalar_r_code,"")
      )

    return(sc_df)
  }

  code_li = vector("list", length(pids))
  opdf = NULL
  counter = 0

  while (counter < length(pids)) {
    counter = counter+1
    pid = pids[counter]
    pdf = path_li[[as.character(pid)]]
    if (is.null(opdf) | NROW(opdf)>=NROW(pdf)) {
      restart = TRUE
    } else {
      restart = !all(opdf$runid == pdf$runid[1:NROW(opdf)])
    }

    if (restart) {
      rdf = run_df[run_df$runid %in% pdf$runid, ]
      rdf = update_rdf_cache_code(rdf)

      rdf = rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre=pre, post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type="", clear=FALSE)

      ps = ps_df[ps_df$pid==pid,]

      if (ps$preserve_data) {
        rdf$code[1] = paste0(rdf$code[1],preserve_code())
        rdf$aux_cmd_type[1] = paste0("load_preserve")
      } else if (ps$restore_data) {
        rdf$code[1] = restore_code(ps$data_path)
        rdf$aux_cmd_type[1] = paste0("restore")
      }
      opdf = pdf

    } else if (!restart) {
      npdf = pdf %>% filter(runid > max(opdf$runid))
      opdf = pdf
      pdf = npdf

      rdf = run_df[run_df$runid %in% pdf$runid, ]
      rdf = update_rdf_cache_code(rdf)

      rdf = rdf %>%
        transmute(pid=pid,runid=runid, code=code, pre=pre, post="", cmd_type=cmd_type, cmd=cmd, is_target = runid==pid, aux_cmd_type="", clear=FALSE)
    }
    code_li[[counter]] = rdf
  }
  sc_df = bind_rows(code_li)

  # we now add scalar definitions from scalar map
  if (!is.null(drf$scalar_code)) {
    sc_df = sc_df %>%
      left_join(drf$scalar_code, by="runid") %>%
      mutate(
        scalar_stata_code = na.val(scalar_stata_code,""),
        scalar_r_code = na.val(scalar_r_code,"")
      )
  }
  sc_df
}
```
!END_MODIFICATION drf_stata_code_df repboxDRF/R/drf_stata_code.R

!MODIFICATION drf_run_df_create_rcode repboxDRF/R/drf_r_code.R
scope = "function"
file = "/home/rstudio/repbox/repboxDRF/R/drf_r_code.R"
function_name = "drf_run_df_create_rcode"
description = "Inject logic into drf_run_df_create_rcode to load e/r dependencies saved by Stata locally before running translated code."
---
```r
drf_run_df_create_rcode = function(run_df=drf$run_df, runids=drf_runids(drf), scalar_code = drf$scalar_code, drf=NULL) {
  restore.point("drf_run_df_create_rcode")

  if (!has_col(run_df, "rcode")) {
    run_df$rcode = rep("", NROW(run_df))
  }
  if (!is.null(runids)) {
    rows = match(runids, run_df$runid)
  } else {
    rows = seq_len(NROW(run_df))
  }
  rows = sort(unique(rows[!is.na(rows)]))

  update_rows = rows

  if (length(update_rows)==0) return(run_df)

  stata_code = run_df$cmdline[update_rows]

  stata_code = gsub("\n", " ", stata_code, fixed = TRUE)

  r_df = stata2r::do_to_r(stata_code, return_df = TRUE)

  translated_code = r_df$r_code
  run_df$rcode[update_rows] = ifelse(is.na(translated_code), "", translated_code)


  # Overwrite 'load' commands with repbox's own data loading logic
  inds = update_rows[run_df$cmd_type[update_rows] %in% c("load")]

  # Also overwrite the VERY FIRST execution row if we truncated the path at a file cache
  if (!is.null(runids) && length(runids) > 0) {
    first_runid = min(runids)
    first_row = match(first_runid, run_df$runid)
    if (!is.na(first_row) && isTRUE(run_df$has_file_cache[first_row])) {
      inds = unique(c(inds, first_row))
    }
  }

  if (length(inds)>0) {
    for (idx in inds) {
      if (isTRUE(run_df$has_file_cache[idx]) && idx == match(min(runids), run_df$runid)) {
        drf_rel_path = paste0("cached_dta/", basename(run_df$drf_cache_file[idx]))
      } else {
        drf_rel_path = ifelse(run_df$is_intermediate[idx],
                              paste0("im_data/", sub("^.*?im_data/", "", run_df$org_data_path[idx])),
                              paste0("org_data/", run_df$found_path[idx]))
      }

      code = paste0(
        'data = drf_load_data(project_dir, "', drf_rel_path ,'")\n',
        'data$stata2r_original_order_idx = seq_len(nrow(data))\n',
        'assign("has_original_order_idx", TRUE, envir = stata2r::stata2r_env)'
      )
      run_df$rcode[idx] = code
    }
  }
  
  run_df$rcode = na.val(run_df$rcode, "")

  # Load locally saved Stata dependency values into the R environment
  if (!is.null(drf) && !is.null(drf$dep_df) && NROW(drf$dep_df) > 0) {
    for (idx in update_rows) {
      r_id = run_df$runid[idx]
      my_deps = drf$dep_df %>% dplyr::filter(runid == r_id, dep_type %in% c("e", "r"), !is.na(source_runid))

      if (NROW(my_deps) > 0) {
        load_code = c()
        for (j in seq_len(NROW(my_deps))) {
          s_runid = my_deps$source_runid[j]
          m_name = my_deps$macro_name[j]
          prefix = substr(m_name, 1, 1)
          inner = gsub("^[er]\\(|\\)$", "", m_name)

          if (m_name == "e(sample)") {
            outfile = file.path("drf", "stata_e_r", paste0(prefix, "_", s_runid, "_", inner, ".dta"))
            var_name = "e_sample"
            load_code = c(load_code, paste0(
              "if (file.exists(file.path(project_dir, '", outfile, "'))) {\n",
              "  stata2r_env$", var_name, " = haven::read_dta(file.path(project_dir, '", outfile, "'))$__esample\n",
              "} else {\n",
              "  repboxUtils::repbox_problem('Missing dependency file: ", outfile, "', type='missing_dep', project_dir=project_dir, fail_action='warn')\n",
              "}"
            ))
          } else {
            outfile = file.path("drf", "stata_e_r", paste0(prefix, "_", s_runid, "_", inner, ".txt"))
            var_name = paste0(prefix, "_", inner)
            load_code = c(load_code, paste0(
              "if (file.exists(file.path(project_dir, '", outfile, "'))) {\n",
              "  stata2r_env$", var_name, " = as.numeric(readLines(file.path(project_dir, '", outfile, "'), warn=FALSE)[1])\n",
              "} else {\n",
              "  repboxUtils::repbox_problem('Missing dependency file: ", outfile, "', type='missing_dep', project_dir=project_dir, fail_action='warn')\n",
              "}"
            ))
          }
        }
        run_df$rcode[idx] = paste0(paste(load_code, collapse="\n"), "\n", run_df$rcode[idx])
      }
    }
  }

  if (NROW(scalar_code)>0) {
    run_df = run_df %>%
      left_join(scalar_code %>% select(runid, scalar_r_code), by="runid") %>%
      mutate(scalar_r_code = na.val(scalar_r_code, "")) %>%
      mutate(rcode = ifelse(rcode=="", rcode, paste0(scalar_r_code, rcode))) %>%
      select(-scalar_r_code)
  }

  run_df
}
```
!END_MODIFICATION drf_run_df_create_rcode repboxDRF/R/drf_r_code.R

!MODIFICATION do_parse stata2r/R/do_parse.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/do_parse.R"
function_name = "do_parse"
description = "Clean up tracking of unused 'needed' macros logic, leaving only need_xi."
---
```r
do_parse = function(do_code) {
  if (is.list(do_code)) {
    do_code = unlist(do_code)
  }
  if (!is.character(do_code)) {
    do_code = as.character(do_code)
  }

  num_lines = length(do_code)
  if (num_lines == 0) {
    return(data.frame(
      line = integer(0),
      do_code = character(0),
      stata_cmd_original = character(0),
      stata_cmd = character(0),
      rest_of_cmd = character(0),
      is_by_prefix = logical(0),
      is_bysort_prefix = logical(0),
      by_group_vars = character(0),
      by_sort_vars = character(0),
      is_quietly_prefix = logical(0),
      is_capture_prefix = logical(0),
      is_xi_prefix = logical(0),
      do_translate = logical(0),
      is_mod = logical(0),
      need_xi = logical(0),
      stata_translation_error = character(0),
      will_have_original_order_idx = logical(0),
      will_ignore_row_order_for_comparison = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  parsed_info = parse_stata_command_lines(do_code)

  cmd_df = data.frame(
    line = seq_len(num_lines),
    do_code = do_code,
    stata_cmd_original = parsed_info$stata_cmd_original,
    stata_cmd = parsed_info$stata_cmd,
    rest_of_cmd = parsed_info$rest_of_cmd,
    is_by_prefix = parsed_info$is_by_prefix,
    is_bysort_prefix = parsed_info$is_bysort_prefix,
    by_group_vars = parsed_info$by_group_vars,
    by_sort_vars = parsed_info$by_sort_vars,
    is_quietly_prefix = parsed_info$is_quietly_prefix,
    is_capture_prefix = parsed_info$is_capture_prefix,
    is_xi_prefix = parsed_info$is_xi_prefix,
    stata_translation_error = NA_character_,
    will_ignore_row_order_for_comparison = FALSE,
    stringsAsFactors = FALSE
  )

  cmd_df$will_have_original_order_idx = rep(FALSE, NROW(cmd_df))
  cmd_df$do_translate = rep(FALSE, NROW(cmd_df))
  cmd_df$is_mod = rep(FALSE, NROW(cmd_df))
  cmd_df$need_xi = rep(FALSE, NROW(cmd_df))

  return(cmd_df)
}
```
!END_MODIFICATION do_parse stata2r/R/do_parse.R

!MODIFICATION s2r_check_mod_df stata2r/R/s2r_check_mod.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/s2r_check_mod.R"
function_name = "s2r_check_mod_df"
description = "Rewrite s2r_check_mod_df to remove fragile backwards scanning for dependencies. Commands are marked only if they intrinsically modify data."
---
```r
s2r_check_mod_df = function(cmd_df) {
  restore.point("s2r_check_mod_df")

  n_rows = NROW(cmd_df)
  if (n_rows == 0) {
    if (!("is_mod" %in% names(cmd_df))) cmd_df$is_mod = logical(0)
    if (!("do_translate" %in% names(cmd_df))) cmd_df$do_translate = logical(0)
    if (!("need_xi" %in% names(cmd_df))) cmd_df$need_xi = logical(0)
    return(cmd_df)
  }

  if (!("is_xi_prefix" %in% names(cmd_df))) cmd_df$is_xi_prefix = rep(FALSE, n_rows)

  cmd_df$is_mod = rep(FALSE, n_rows)
  cmd_df$do_translate = rep(FALSE, n_rows)
  cmd_df$need_xi = rep(FALSE, n_rows)

  # Commands that are inherently data-modifying are always kept.
  cmd_df$is_mod = !is.na(cmd_df$stata_cmd) & (cmd_df$stata_cmd %in% stata_data_manip_cmds)

  # tabulate is usually display-only, but tabulate ..., gen(...) creates dummy variables.
  idx_tabulate_gen = which(!is.na(cmd_df$stata_cmd) & cmd_df$stata_cmd %in% c("tabulate", "tab"))
  if (length(idx_tabulate_gen) > 0) {
    has_gen = vapply(
      cmd_df$rest_of_cmd[idx_tabulate_gen],
      s2r_tabulate_has_gen_option,
      logical(1)
    )
    cmd_df$is_mod[idx_tabulate_gen[has_gen]] = TRUE
  }

  cmd_is_est = !is.na(cmd_df$stata_cmd) & (cmd_df$stata_cmd %in% stata_estimation_cmds)
  rest_of_cmd_vec = cmd_df$rest_of_cmd
  rest_of_cmd_vec[is.na(rest_of_cmd_vec)] = ""

  # Xi side effects from xi-prefixed estimation commands
  idx_xi_est = which(cmd_df$is_xi_prefix & cmd_is_est)
  if (length(idx_xi_est) > 0) {
    do_code_vec = cmd_df$do_code
    do_code_vec[is.na(do_code_vec)] = ""

    for (i in idx_xi_est) {
      parsed_est = s2r_p_estimation_cmd(rest_of_cmd_vec[i], estimator = cmd_df$stata_cmd[i])
      xi_prefixes = s2r_xi_specs_to_prefixes(parsed_est$xi_specs)

      need_xi_i = FALSE
      if (length(xi_prefixes) > 0 && i < n_rows) {
        later_lines = do_code_vec[(i + 1):n_rows]

        # Fast vectorized check across all later lines at once
        if (any(stringi::stri_detect_fixed(later_lines, "_I*"))) {
          need_xi_i = TRUE
        } else {
          for (pref in xi_prefixes) {
            if (any(stringi::stri_detect_fixed(later_lines, pref))) {
              need_xi_i = TRUE
              break
            }
          }
        }
      }

      cmd_df$need_xi[i] = need_xi_i
      if (need_xi_i) {
        cmd_df$is_mod[i] = TRUE
      }
    }
  }

  # Final explicit overrides (vectorized)
  is_non_manip = !is.na(cmd_df$stata_cmd) & (cmd_df$stata_cmd %in% stata_non_data_manip_cmds)

  override_idx = is_non_manip & !cmd_df$need_xi
  if (any(override_idx)) {
    cmd_df$is_mod[override_idx] = FALSE
  }

  cmd_df$is_mod[is.na(cmd_df$stata_cmd)] = FALSE

  is_standalone_clear = !is.na(cmd_df$stata_cmd) & cmd_df$stata_cmd == "clear" & (rest_of_cmd_vec == "")
  if (any(is_standalone_clear)) {
    cmd_df$is_mod[is_standalone_clear] = TRUE
  }

  cmd_df$do_translate = cmd_df$is_mod
  return(cmd_df)
}
```
!END_MODIFICATION s2r_check_mod_df stata2r/R/s2r_check_mod.R

!MODIFICATION t_estimation_cmd stata2r/R/t_estimation_cmd.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_estimation_cmd.R"
function_name = "t_estimation_cmd"
description = "Make estimation commands no-ops unless they contain xi: prefixes that generate variables."
---
```r
t_estimation_cmd = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = cmd_obj$stata_cmd) {
  restore.point("t_estimation_cmd")

  need_xi = isTRUE(cmd_obj$need_xi)

  if (!need_xi) {
    return(paste0("# ", estimator, " at line ", line_num, " is no-op (no later-used side effects)."))
  }

  parsed = s2r_p_estimation_cmd(rest_of_cmd, estimator = estimator)
  if (is.na(parsed$dep_var) && length(parsed$xi_specs) == 0) {
    return(paste0("# Failed to parse estimation command at line ", line_num, ": ", cmd_obj$do_code))
  }

  code_lines = character(0)

  if (length(parsed$xi_specs) == 0) {
    return(paste0("# Failed to parse xi side effects for estimation command at line ", line_num, ": ", cmd_obj$do_code))
  }

  for (spec in parsed$xi_specs) {
    if (is.null(spec$var2) || is.na(spec$var2) || spec$var2 == "") {
      code_lines = c(
        code_lines,
        paste0(
          "data = scmd_xi(data = data, var1 = ",
          quote_for_r_literal(spec$var1),
          ")"
        )
      )
    } else {
      code_lines = c(
        code_lines,
        paste0(
          "data = scmd_xi(data = data, var1 = ",
          quote_for_r_literal(spec$var1),
          ", var2 = ",
          quote_for_r_literal(spec$var2),
          ")"
        )
      )
    }
  }

  paste(code_lines, collapse = "\n")
}
```
!END_MODIFICATION t_estimation_cmd stata2r/R/t_estimation_cmd.R

!MODIFICATION scmd_estimation_effects stata2r/R/t_estimation_cmd.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_estimation_cmd.R"
function_name = "scmd_estimation_effects"
description = "Mark internal simulation regression logic as deprecated because dependencies are now directly read from file caches."
---
```r
scmd_estimation_effects = function(data, dep_var, model_vars, needed_e, r_if_cond = NA_character_, estimator = "regress", formula_terms = character(0)) {
  stop("scmd_estimation_effects is deprecated. R translations should load e() variables from repboxDRF caches.")
}
```
!END_MODIFICATION scmd_estimation_effects stata2r/R/t_estimation_cmd.R

!MODIFICATION translate_stata_expression_with_r_values stata2r/R/translate_stata_expression_with_r_values.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/translate_stata_expression_with_r_values.R"
function_name = "translate_stata_expression_with_r_values"
description = "Remove fragile backwards scanning and map e() and r() directly to the stated conventions established by repboxDRF."
---
```r
translate_stata_expression_with_r_values = function(stata_expr, line_num, cmd_df, context) {
  restore.point("translate_stata_expression_with_r_values")

  r_value_mappings = list()

  # We extract all e(...) and r(...) used in stata_expr
  e_matches = stringi::stri_extract_all_regex(stata_expr, "\\be\\([a-zA-Z0-9_]+\\)")[[1]]
  e_matches = e_matches[!is.na(e_matches)]
  for (m in e_matches) {
    inner = gsub("^[er]\\(|\\)$", "", m)
    r_value_mappings[[m]] = paste0("stata2r_env$e_", inner)
  }

  r_matches = stringi::stri_extract_all_regex(stata_expr, "\\br\\([a-zA-Z0-9_]+\\)")[[1]]
  r_matches = r_matches[!is.na(r_matches)]
  for (m in r_matches) {
    inner = gsub("^[er]\\(|\\)$", "", m)
    r_value_mappings[[m]] = paste0("stata2r_env$r_", inner)
  }

  translated_expr = translate_stata_expression_to_r(
    stata_expr,
    context = context,
    r_value_mappings = r_value_mappings
  )

  return(translated_expr)
}
```
!END_MODIFICATION translate_stata_expression_with_r_values stata2r/R/translate_stata_expression_with_r_values.R

!MODIFICATION t_summarize stata2r/R/t_summarize.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_summarize.R"
function_name = "t_summarize"
description = "Make summarize entirely a no-op."
---
```r
t_summarize = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_summarize")
  return(paste0("# summarize at line ", line_num, " is no-op (handled via DRF caching)."))
}
```
!END_MODIFICATION t_summarize stata2r/R/t_summarize.R

!MODIFICATION scmd_summarize stata2r/R/t_summarize.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_summarize.R"
function_name = "scmd_summarize"
description = "Make internal summarize computation a deprecation stub."
---
```r
scmd_summarize = function(data, needed_r, var_for_r = NA_character_, r_if_cond = NA_character_) {
  stop("scmd_summarize is deprecated. R translations should load r() variables from repboxDRF caches.")
}
```
!END_MODIFICATION scmd_summarize stata2r/R/t_summarize.R

!MODIFICATION t_tabulate stata2r/R/t_tabulate.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_tabulate.R"
function_name = "t_tabulate"
description = "Simplify tabulate to only run when using the `gen(...)` option and remove r() dependency evaluation."
---
```r
t_tabulate = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_tabulate")

  parsed = s2r_p_tabulate(rest_of_cmd)
  has_gen = !is.na(parsed$gen_stub) && parsed$gen_stub != ""

  if (!has_gen) {
    return(paste0("# tabulate at line ", line_num, " is no-op (no generated dummies)."))
  }

  if (length(parsed$var_tokens) == 0) {
    return(paste0("# Failed to parse tabulate command: ", rest_of_cmd))
  }

  if (has_gen && length(parsed$var_tokens) != 1) {
    return(paste0("# tabulate generate() is only implemented for one-way tabulate: ", rest_of_cmd))
  }

  varname = parsed$var_tokens[1]

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str) && parsed$if_str != "") {
    r_if_cond = translate_stata_expression_with_r_values(
      parsed$if_str,
      line_num,
      cmd_df,
      list(is_by_group = FALSE)
    )
  }

  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  args = c(
    "data = data",
    paste0("varname = ", quote_for_r_literal(varname)),
    paste0("gen_stub = ", quote_for_r_literal(parsed$gen_stub))
  )

  if (!is.na(r_if_cond)) {
    args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  }
  if (!is.na(r_in_range)) {
    args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  }

  args = c(args, paste0("include_missing = ", parsed$include_missing))

  r_code = paste0("data = scmd_tabulate(", paste(args, collapse = ", "), ")")
  return(r_code)
}
```
!END_MODIFICATION t_tabulate stata2r/R/t_tabulate.R

!MODIFICATION scmd_tabulate stata2r/R/t_tabulate.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_tabulate.R"
function_name = "scmd_tabulate"
description = "Remove internal r() dependency calculation since they are handled strictly via cached states."
---
```r
scmd_tabulate = function(data, varname, gen_stub = NA_character_,
                         r_if_cond = NA_character_, r_in_range = NA_character_,
                         include_missing = FALSE) {
  restore.point("scmd_tabulate")

  var_actual = expand_varlist(varname, names(data))[1]
  if (is.na(var_actual) || !(var_actual %in% names(data))) {
    stop(paste0("scmd_tabulate: variable '", varname, "' not found"))
  }

  n = NROW(data)
  mask = rep(TRUE, n)

  if (!is.na(r_if_cond) && r_if_cond != "") {
    r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))
    mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  }

  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, n)
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  x = data[[var_actual]]
  x_for_tab = x[mask]

  is_missing_value = function(v) {
    if (is.character(v)) {
      return(is.na(v) | stringi::stri_trim_both(v) == "")
    }
    is.na(v)
  }

  missing_x_for_tab = is_missing_value(x_for_tab)

  if (!include_missing) {
    levels_source = x_for_tab[!missing_x_for_tab]
  } else {
    levels_source = x_for_tab
  }

  if (is.character(levels_source) || is.factor(levels_source)) {
    levels_chr = as.character(levels_source)
    levels_chr[is.na(levels_chr)] = ""
    levs = unique(levels_chr)
    levs = stringi::stri_sort(levs, locale = "C")
  } else {
    levs = sort(unique(as.numeric(levels_source)), na.last = TRUE)
  }

  has_gen = !is.na(gen_stub) && gen_stub != ""
  if (has_gen) {
    if (length(levs) > 0) {
      new_names = paste0(gen_stub, seq_along(levs))
      existing_new = intersect(new_names, names(data))
      if (length(existing_new) > 0) {
        stop(paste0(
          "scmd_tabulate: generated variable(s) already exist: ",
          paste(existing_new, collapse = ", ")
        ))
      }

      x_cmp = x
      if (is.character(x_cmp) || is.factor(x_cmp)) {
        x_cmp = as.character(x_cmp)
        x_cmp[is.na(x_cmp)] = ""
      } else {
        x_cmp = as.numeric(x_cmp)
      }

      for (i in seq_along(levs)) {
        level_i = levs[i]
        new_var = new_names[i]

        if (is.character(x_cmp)) {
          match_i = x_cmp == as.character(level_i)
        } else if (is.na(level_i)) {
          match_i = is.na(x_cmp)
        } else {
          match_i = x_cmp == level_i
        }

        match_i[is.na(match_i)] = FALSE

        out = rep(NA_integer_, n)
        out[mask] = 0L
        out[mask & match_i] = 1L

        data[[new_var]] = out
        attr(data[[new_var]], "label") = paste0(var_actual, "==", as.character(level_i))
      }
    }
  }

  return(data)
}
```
!END_MODIFICATION scmd_tabulate stata2r/R/t_tabulate.R

!MODIFICATION s2r_store_e_results stata2r/R/s2r_store_results.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/s2r_store_results.R"
function_name = "s2r_store_e_results"
description = "Remove legacy environment tracking wrapper code."
---
```r
s2r_store_e_results = function(res) {
  # Deprecated - handled through explicit read via drf
  invisible(res)
}
```
!END_MODIFICATION s2r_store_e_results stata2r/R/s2r_store_results.R

!MODIFICATION s2r_store_r_results stata2r/R/s2r_store_results.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/s2r_store_results.R"
function_name = "s2r_store_r_results"
description = "Remove legacy environment tracking wrapper code."
---
```r
s2r_store_r_results = function(res) {
  # Deprecated - handled through explicit read via drf
  invisible(res)
}
```
!END_MODIFICATION s2r_store_r_results stata2r/R/s2r_store_results.R
