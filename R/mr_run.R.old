example = function() {
  project = "testsupp"
  project_dir = file.path("~/repbox/projects_reg",project)

  dap = get.project.dap(project_dir,add.run.df = TRUE)

  plot.dap(dap)
  step.df = dap$step.df
}

mr_get_used_steps = function(mr) {
  mr_get_asteps(mr, just_used=TRUE)
}

mr_get_asteps = function(mr, just_used=TRUE) {
  if (!is.null(mr[["used_steps"]]) & just_used) {
    return(mr$used_steps)
  }
  mr$step.df$step[mr$step.df$step_type=="a" & is.true(mr$step.df$reg_ok)]
}

mr_init_log = function(mr) {
  log.dir = file.path(mr$out_dir,"logs")
  if (!dir.exists(log.dir)) dir.create(log.dir)
  log.file = paste0(log.dir,"/log_",gsub(":","", Sys.time(), fixed=TRUE),".txt")
  options(metareg_log_file = log.file)

  mr_cat("New log starts", just.log=TRUE)
}

mr_close_log = function(...) {
  mr_cat("Log finishes", just.log=TRUE)
  options(metareg_log_file = NULL)
}

mr_cat = function(..., just.log=FALSE) {
  msg = paste(..., collapse="\n")
  if (!just.log) cat(msg)
  log.file = getOption("metareg_log_file")
  if (is.null(log.file)) return()
  restore.point("mr_cat2")

  cat(paste0(Sys.time(),": ", msg),file = log.file, append=TRUE)
}

mr_run = function(mr, asteps = mr_get_asteps(mr), run_stata=TRUE, clear_old_step_results = TRUE, do_agg = !is.null(mr$study_agg_fun), stop.on.error = mr$opts$stop.on.error) {
  restore.point("mr_run")
  repbox_reset_problem_log("run")
  repbox_set_problem_options(project_dir=mr$project_dir, fail_action=mr$opts$repbox_problem_fail_action, metaid = mr$metaid)
  repbox_set_current_project_dir(mr$project_dir)


  mr$check_df = tibble(step = asteps, did_run=NA, problem="", deviation=NA,tolerable_deviation=NA, comment="")

  if (mr_has_problem(mr)) {
    cat("\nDon't run metastudy ", mr$metaid, " due to problem ", mr$problem_msg,"\n")
    return(mr)
  }
  on.exit(mr_close_log())

  mr$run_start_time = Sys.time()
  mr_init_log(mr)

  mr$stop.on.error = stop.on.error

  if (is.null(mr$step.df) | length(asteps)==0) {
    mr_cat("\nNo regressions to be analysed found.")
    mr$did_run = FALSE
    return(mr)
  }

  mr$step.df$runtime = NA

  mr$stata_runtime = mr$r_runtime = mr$agg_runtime = mr$stata_agg_runtime = NA
  if (clear_old_step_results & !is.null(mr$step.dir)) {
    del.files = list.files(mr$step.dir, full.names = TRUE)
    file.remove(del.files)
  }

  # 0. Run stata code
  if (run_stata & !is.null(mr$stata_code_fun)) {
    mr = mr_make_all_stata_code(mr, mr$stata_code_fun, asteps = asteps)
    start = Sys.time()
    mr = mr_run_all_stata_code(mr)
    mr$stata_runtime = as.numeric(Sys.time())-as.numeric(start)
  }

  # If we have a separate aggregation function for Stata results
  # call it. The results will then be available in mr
  if (!is.null(mr$stata_agg_fun)) {
    start = as.numeric(Sys.time())
    mr = mr$stata_agg_fun(mr, stata_check_df = mr[["stata_check_df"]])
    check_mr_class(mr,"stata_agg_fun")

    mr$stata_agg_runtime = as.numeric(Sys.time())-as.numeric(start)
  }

  if (is.null(mr$step_run_fun)) {
    mr_store_runtime(mr)
    return(invisible(mr))
  }

  # 1. Determine necessary steps
  path.df = mr$path.df %>%
    filter(astep %in% asteps)

  # 2. Determine all source steps
  source_steps = unique(path.df$source_step)

  start = Sys.time()

  source_step = first(source_steps)
  # 3. Run analysis for all source steps
  for (source_step in source_steps) {
    mr_cat("\n*********************************************")
    mr_cat("\nRun analyses for source step ", source_step)
    mr_cat("\n*********************************************\n")

    mr = mr_run_for_source_step(mr, source_step, path.df)
  }
  mr$r_runtime = as.numeric(Sys.time())-as.numeric(start)

  if (do_agg) {
    start = as.numeric(Sys.time())
    mr = mr$study_agg_fun(mr)
    check_mr_class(mr,"study_agg_fun")
    mr$agg_runtime = as.numeric(Sys.time())-as.numeric(start)
  }

  mr = mr_store_runtime(mr, save=TRUE)
  mr$did_run = TRUE

  return(invisible(mr))
}

mr_store_runtime = function(mr, save = TRUE) {
  restore.point("mr_store_runtime")

  rt = list(
    total = data.frame(project=basename(mr$project_dir), metaid=mr$metaid, r_runtime = mr$r_runtime, stata_runtime=mr$stata_runtime, agg_runtime=mr$agg_runtime),
    steps_runtime = transmute(mr$step.df, step=step, action=ifelse(cache,"load",step_type), r_runtime=runtime) %>% filter(!is.na(r_runtime))
  )
  if (save) {
    saveRDS(rt,paste0(mr$out_dir,"/runtimes.Rds"))
  }
  mr$runtimes = rt
  mr
}

mr_run_for_source_step = function(mr, source_step, path.df) {
  restore.point("mr_run_for_source_step")

  path.df = path.df[path.df$source_step == source_step,]

  # Add maximum remaining path length (to.go)
  # and no of paths to each step in step.df

  path.df = path.df %>%
    group_by(astep) %>%
    mutate(
      to.go = rev(seq_len(n()))
    ) %>%
    ungroup()

  run.step.info = path.df %>%
    group_by(step) %>%
    summarize(
      num.paths = n_distinct(astep),
      to.go = max(to.go),
      len.guess = 3*to.go + num.paths
    )

  step.df = mr$step.df %>%
    left_join(run.step.info, by="step") %>%
    mutate(
      is.run = FALSE,
      data = vector("list",n())
    )

  open.steps = run.step.info$step

  stack.size = 0
  stack.steps = rep(NA, NROW(open.steps))
  cur.step = source_step
  dat = NULL
  is.on.stack = FALSE

  source_was_analyzed = FALSE

  while(TRUE) {
    #restore.point("while.start")

    # An analysis step my also be a source step
    # if we directly cache that data
    if (cur.step == source_step) {
      if (NROW(step.df$data[[cur.step]])>0) {
        dat = step.df$data[[cur.step]]
      } else {
        mr_cat("\nLoad data in step ", cur.step)
        step.start = as.numeric(Sys.time())
        dat = mr_load_data(mr, cur.step)
        mr$step.df$runtime[cur.step] = as.numeric(Sys.time())-step.start
      }
    }

    # An analysis step
    if (step.df$step_type[cur.step]=="a" & !(source_was_analyzed & cur.step==source_step)) {
      #stop("Analysis steps not yet implemented")
      mr_cat(paste0("\nAnalysis step ", cur.step, " ", step.df$dofile[cur.step], " L", step.df$orgline[cur.step], ": ", shorten.str(step.df$steplab[cur.step],150),"\n"))

      step.start = as.numeric(Sys.time())
      check_row = which(mr$check_df$step == cur.step)
      mr$check_df$did_run[check_row] = TRUE

      set_repbox_problem_step(cur.step)
      repbox_problem_set_step(cur.step)
      if (mr$opts$stop.on.error) {
        mr = mr_analysis_step(mr, cur.step,dat)
      } else {
        res = try(mr_analysis_step(mr, cur.step,dat),silent = TRUE)
        if (is(res, "try-error")) {
          #note_problem("analysis_step_error",paste0(as.character(res), collapse="\n"), stop=FALSE)
          mr$check_df$did_run[check_row] = FALSE
          msg = paste0(as.character(res), collapse="\n")
          msg = str.right.of(msg, ":") %>% trimws()
          mr$check_df$problem[check_row] = msg
        } else {
          mr = res
        }
      }
      clear_repbox_problem_step()
      repbox_problem_set_step(NA_integer_)


      mr$step.df$runtime[cur.step] = as.numeric(Sys.time())-step.start

      # Remove step from open steps
      open.steps = setdiff(open.steps, cur.step)
      if (cur.step == source_step) {
        source_was_analyzed = TRUE
      } else if (stack.size == 0) {
        break
      } else {
        cur.step = stack.steps[stack.size]
        dat = step.df$data[[cur.step]]
        is.on.stack = TRUE
        mr_cat("\nGo back to stack step ", cur.step)
        next
      }
    }



    # Get data after the step is computed
    if (NROW(step.df$data[[cur.step]])>0) {
      dat = step.df$data[[cur.step]]
    } else if (cur.step != source_step) {
      mr_cat("\nModification step ", cur.step, "\n")
      step.start = as.numeric(Sys.time())
      dat = mr_mod_step(mr, cur.step, dat)
      mr$step.df$runtime[cur.step] = as.numeric(Sys.time())-step.start
    }

    if (is.null(dat)) {
      restore.point("metareg_run_empty_dat")
      stop("dat is empty, but it shouldn't be!")
    }

    # Get relevant children that have not yet been computed
    children = step.df$children[[cur.step]] %>% intersect(open.steps)

    if (length(children) > 1) {
      # Put cur.step on stack and store data if more than on child left
      if (!is.on.stack) {
        stack.size = stack.size +1
        stack.steps[stack.size] = cur.step
        step.df$data[[cur.step]] = dat
      }

      # Continue with child that has probably
      # shortest evaluation
      len.guess = step.df$len.guess[children]
      cur.step = children[which.min(len.guess)]
      is.on.stack = FALSE
    } else if (length(children)==1) {
      # Clear data and remove from stack
      # if only one child left
      if (is.on.stack) {
        mr_cat("\nRemove step ", cur.step, "from stack.\n")
        stack.size = stack.size-1
        step.df$data[[cur.step]] = list()
      }

      # Clear from open steps
      open.steps = setdiff(open.steps, cur.step)

      # We only have a single child (left)
      cur.step = children[1]
      is.on.stack = FALSE
    } else {
      stop("Got a non-analysis step without children. That should not be possible. There seems to be some bug.")
    }
  }

  mr


}

mr_mod_step = function(mr, step, dat, env = new.env(parent = METAREG_STATA_ENV)) {
  restore.point("mr_mod_step")
  #if (step==5) stop()

  opts = mr$opts

  if (opts$load.extra.cache) {
    cache.dat = mr_load_extra_cache(mr, step)
    if (!is.null(cache.dat)) {
      msg = paste0("\nWe used the extra cache for step ", step, " (see ", mr$project_dir,"/metareg/extra_cache/). The cache was created in an earlier run of this step that yielded an error.\n")
      mr_cat(msg)
      return(cache.dat)
    }
  }


  env$. = NA
  env$dat = dat
  rcode = mr_get_rcode(mr, step)
  res = try(parse_eval(rcode, env))

  if (is(res, "try-error")) {
    restore.point("mr_mod_step_error")
    msg = paste0("Error in running R data modification step ", step,". Possible reasons: translation code has a bug / is not complete or the Stata code uses variable abbreviations, which we don't translate to R.")

    if (opts$extra.infeasible) {
      inf.dir = file.path(mr$project_dir,"/metareg/extra_infeasible")
      if (!dir.exists(inf.dir)) dir.create(inf.dir)
      inf.file = paste0(inf.dir,"/",step,".inf")
      writeLines("",inf.file)
    }

    if (opts$extra.cache) {
      cache.file = mr_make_extra_cache(mr,step = step)
      if (!is.null(cache.file)) {
        dat = mr_load_extra_cache(mr, step, cache.file)
        msg = paste0(msg,"\nWe created an extra cache file by running the Stata code and used it.\n")
        mr_cat(msg)
        return(dat)
      }
      msg = paste0(msg,"\nWe tried to create an extra cache file by running the Stata code (see", paste0(mr$project_dir,"/metareg/extra_cache/step_", step,".do"),"), but it failed. An error in the Stata code might be due to an error in the supplement or by missing some Stata command when creating the modification steps of the data analysis plan (DAP). This means the metareg package must be corrected.\n")
      if (opts$extra.infeasible) {
        msg = paste0(msg, "\nWe marked the step as infeasible (see ", inf.dir,") If you re-run the metareg study again, it will cache this step based on the Stata output.")
      }
      stop(msg)
    }
    if (!opts$extra.infeasible) {
      msg = paste0(msg, " Try to set option extra.cache=TRUE to perform modification steps with Stata instead.")
    }
    stop(msg)
  }

  # For debugging purposes
  classes = sapply(env$dat, class)
  if (any(classes == "list")) {
    restore.point("list.class")
    stop()
  }

  return(env$dat)
}

mr_analysis_step = function(mr, astep, dat) {
  restore.point("mr_run_analysis_step")

  res = try(mr_analysis_step_make_args(mr, astep, dat), silent=TRUE)

  if (is(res, "try-error")) {
    mr = try(mr$step_run_fun(mr=mr, step=astep,has_error=TRUE, err=res, dat=NULL, reg=NULL, org_dat=dat, infeasible_filter=NULL), silent = TRUE)
  } else {
    mr = mr$step_run_fun(mr=mr, step=astep, dat=res$dat, reg=res$reg,   org_dat=res$org_dat, infeasible_filter=res$infeasible_filter)
  }

  check_mr_class(mr,"step_run_fun")

  mr
}

mr_analysis_step_make_args = function(mr, astep, dat) {
  restore.point("mr_analysis_step_make_args")
  reg = mr_get_reg_info(mr, astep,dat)
  infeasible_filter=FALSE
  org_dat = dat
  dat = mr_adapt_data_for_reg(mr$project_dir, astep, reg, dat)


  if (mr_is_infeasible(dat)) {
    res  = dat
    save.dir = file.path(mr$out_dir,"step_results")
    save.file = paste0(save.dir,"/infeasible_", astep,".Rds")
    saveRDS(res, save.file)

    #return(res)
    infeasible_filter=TRUE
    dat = mr_adapt_data_for_reg(mr$project_dir, astep, reg, org_dat,use.filter=FALSE)
  }

  if (mr$opts$pass.repdb.info) {
    # Create cterm cols: This also takes care about
    # abbreviations
    regvar = mr_get_base_table(mr, "regvar", astep)
    cols = unique(regvar$cterm)
    dat = create_cterm_cols(dat, cols)

    #
    if (mr$opts$create.regxvar.cols) {
      regxvar = mr_get_base_table(mr, "regxvar", astep)
      dat = make_regxvar_cols(dat, regxvar)
    }
  }


  if (mr$opts$pass.repdb.info & !isTRUE(mr$opts$pass.internal.info)) {
    reg = mr_get_reg(mr, step=astep)
  }
  res = list(reg=reg, dat=dat, org_dat=org_dat, infeasible_filter=infeasible_filter)
}

mr_adapt_data_for_reg = function(project_dir,step, reg, dat, logical.to.dummy = TRUE, use.filter = TRUE) {
  restore.point("mr_adapt_data_for_reg")
  # Filter
  if (!is.na(reg$if_str) & use.filter) {
    res = try(eval_reg_if_condition(project_dir,step, reg, dat), silent=TRUE)
    if (is(res, "try-error")) {
      restore.point("sfhshfjkdhjkfhdshfhsd")
      return(mr_infeasible(paste0("\nCannot evaluate in R the if condition ", reg$if_str, "\nin\n", reg$cmdline,". Also no ifrows were stored","\nOriginal error:\n", as.character(res)), if_str=reg$if_str))
    }
    dat = res
  }

  # Transform FALSE-TRUE to 0-1 dummy
  # This increases compatibility with Stata
  # when comparing results
  if (logical.to.dummy) {
    cols = which(sapply(dat, is.logical))
    for (col in cols) {
      dat[[col]] = as_integer(dat[[col]])
    }

  }


  # Possibly generate new variables
  vi = reg$vi[[1]]

  # Variables with time series operators. See
  # https://www.stata.com/support/faqs/statistics/time-series-operators/
  # https://www.stata.com/manuals/u11.pdf#u11.4.4

  prefix.type = toupper(substring(vi$prefix,1,1))
  rows = which(prefix.type %in% c("D","L","F","S"))

  i = 1
  for (i in seq_along(rows)) {
    var = first(vi$var[rows[i]])
    dat = mr_create_stata_timeseries_var(dat,var=var,prefix= vi$prefix[rows[i]],reg=reg)
    if (mr_is_infeasible(dat)) return(dat)
  }

  dat
}

collapse.test = function() {
  library(dplyr)
  library(collapse)
  data = data.frame(group = sample(1:3, size=10, replace=TRUE), x=runif(10))

  groups = "group"
  data %>%
    GRP(by=groups) %>%
    fmutate(
      x=mean(x)
    )


  data <- data.frame(time = c(1:2, 4:5),value = rnorm(4))
  data$lag_value = collapse::flag(data$value, t=data$time)
  data

  # A bug: 2nd row should be NA
  data <- data.frame(time = c(1, 3:5),value = rnorm(4))
  data$lag_value = collapse::flag(data$value, t=data$time)
  data

}

# Variables with time series operators. See
# See https://www.stata.com/manuals/u11.pdf#u11.4.4
# We will deal with operators like:
# L2.x1
# but we currently ignore things like
# L(0/4).x1
# L2.S1.x1
mr_create_stata_timeseries_var = function(dat,var, prefix,reg, sep=".") {
  restore.point("mr_create_stata_timeseries_var")

  prefix.type = toupper(substring(prefix,1,1))
  prefix.num = substring(prefix,2)

  tdelta = as.numeric(reg$tdelta)
  if (is.na(tdelta)) tdelta = 1

  #if (! reg$tdelta %in% c("1","",NA)) {
  #  stop("\nCannot yet deal with time series prefixes if tdelta != 1")
  #}

  if (any(has.substr(prefix.num,"("))) {
    repbox_problem(type="ts_prefix_L(0/2)", "\nCannot yet generate variables in the data set for time series prefixes like L(0/2).")
    return(dat)
  }
  if (any(stri_detect_regex(prefix.num,"[a-zA-Z]"))) {
    repbox_problem(type="ts_prefix_L1.S1.x", "\nCannot yet generate variables in the data set for time series prefixes like L2.S1.x")
    return(dat)
  }



  args = list(x=dat[[var]], n=1)
  if (!is_empty(reg$timevar)) {
    args$t = dat[[reg$timevar]]
  }
  if (!is_empty(reg$panelvar)) {
    args$g = dat[[reg$panelvar]]
  }


  prefix.num = ifelse(prefix.num=="", 1, as_integer(trimws(prefix.num)))

  if (prefix.type == "L") {
    fun = collapse::flag
    args$n = prefix.num
  } else if (prefix.type == "F") {
    fun = collapse::flag
    args$n = -prefix.num
  } else if (prefix.type == "D") {
    fun = collapse::fdiff
    args$diff = prefix.num
  } else if (prefix.type == "S") {
    fun = collapse::fdiff
    args$n = prefix.num
  }
  if (tdelta > 1) {
    args$t = dat[[reg$timevar]]
    args$n = args$n * tdelta
  }

  new.var = paste0(prefix,sep, var)
  dat[[new.var]] = do.call(fun, args)
  dat
}


mr_get_rcode = function(mr, step) {
  restore.point("mr_get_rcode")
  s = mr$step.df[step,]
  if (s$step_type == "mod") {
    rcode = stata.to.r(sep.lines(s$stata_code))
  } else {
    stop("mr_get_rcode is currently only implemented for mod steps.")
  }
  rcode
}


mr_load_data = function(mr, step) {
  file = mr_get_cache_file(mr$project_dir, step)
  dat = haven::read_dta(file)
  dat
}

mr_get_reg_info = function(mr, step, dat, stata_code = mr$step.df$stata_code[step]) {
  restore.point("mr_get_reg_info")
  step.df = mr$step.df[step,]

  reg = stata.reg.parse(stata_code, run=mr$step.df[step,])
  reg$se.info = list(stata.reg.se.info(reg))
  reg$vi = list(vi.from.stata.reg(reg, dat))
  reg
}


mr_write_path_code = function(mr, astep=first(mr$path.df$astep), ...) {
  mr_write_path_r_code(mr, astep, ...)
  mr_write_path_stata_code(mr, astep, ...)
}

mr_write_path_r_code = function(mr, astep=first(mr$path.df$astep), code.file=paste0(mr$project_dir,"/metareg/step_",astep,".r"), stata.as.comment = FALSE, add.line.info = !is.null(run.df), run.df=mr$run.df,...) {
  restore.point("mr_write_path_r_code")

  txt = ""
  path = dap$path.df[dap$path.df$astep == astep,]
  # 1. Get data file
  step = path$step[1]
  file = mr_get_cache_file(mr$project_dir, step)
  txt = paste0(txt,'
# We will evaluate the following stata code using the special stata environment
stata.env = make.stata.funs.env()
restorepoint::copy.into.env(env, globalenv())

dat = haven::read_dta("',file,'")')


  # 2. code for all data modification steps
  mod.steps = path$step[-c(1, NROW(path))]
  step = first(mod.steps)


  if (add.line.info) {
    mod.txt = sapply(mod.steps, function(step) {
      run_rows = mr$step.df$run_rows[[step]]
      rrow = first(run_rows)
      scode = sapply(run_rows, function(rrow) {
        run = run.df[rrow,]
        stata_code = run$cmdline
        rcode = stata.to.r(stata_code)
        info = paste0("# ",run$donum, " ", mr$dotab$dofile[run$donum], " Line ", run$orgline)
        paste0("\n\n",info,"\n# ",stata_code, "\n", rcode)
      }) %>% merge.lines()
    }) %>% merge.lines()
    txt = paste0(txt,mod.txt)
  } else {
    for (step in mod.steps) {
      if (stata.as.comment) {
        stata_code = mr$step.df$stata_code[step] %>% sep.lines()
        rcode = mr_get_rcode(mr, step)
        txt = paste0(txt,"\n\n", paste0("#",stata_code,"\n",rcode, collapse="\n\n"))
      } else {
        rcode = mr_get_rcode(mr, step)
        txt = paste0(txt,"\n\n",paste0(trimws(rcode), collapse="\n\n"))
      }
    }
  }


  reg.file = paste0(tools::file_path_sans_ext(code.file),"_reg.Rds")

  stata_code = mr$step.df$stata_code[astep]
  txt = paste0(txt,

"\n
reg = readRDS('",reg.file,"')
reg$se.info = list(stata.reg.se.info(reg))
reg$vi = list(vi.from.stata.reg(reg, dat))

reg_call = make_reg_call(reg)

# Show call
do.call(call, c(list(name = reg_call$command), reg_call$args))
# Stata cmdline
reg$cmdline


reg_res = eval_reg(reg_call, data=dat)
reg_res
")

  parsed.reg = stata.reg.parse(stata_code, run=mr$step.df[astep,])
  saveRDS(parsed.reg, reg.file)

  writeLines(txt, code.file)
  invisible(txt)
}

mr_get_regcheck_after_run = function(mr, tolerable_deviation, artid=mr$artid, variant=mr$variant, add_stata_check=TRUE) {
  if (missing(tolerable_deviation)) {
    stop("To make your metastudy code more transparent, please expliclity specify the parameter tolerable deviation.

The tolerable deviation is a subjective assement of you, the metareg designer, to inform the user what you roughly deem a tolerable deviation for a particular step. This will depend on the analysis which also specifies the definition of the deviation.

  For example if we re-run the same stata code as in the original study, we may say the tolerable deviation is 0. If we translate code from Stata to R the tolerable deviation is usually larger.

  Sometimes we use the deviation column just for informative purposes and it can get arbitrarily larger without indicating a problem. Then just set tolerable_deviation=NA.

  In some metastudies the tolerable deviation may differ between regressions and / or variants. In this case please adapat the resulting data frame manually. If you set it to tolerable_deviation=NULL, we just keep the values currently in mr$check_df (typically NA).
")
  }
  restore.point("mr_get_regcheck_after_run")


  regcheck = mr$check_df


  regcheck = add_cols_if_not_exist(regcheck, artid=artid, variant=variant)
  if (!is.null(tolerable_deviation)) {
    regcheck$tolerable_deviation = rep(tolerable_deviation, length.out = NROW(regcheck))
  }
  if (add_stata_check) {
    stata_check_df = mr[["stata_check_df"]]
    if (is.null(stata_check_df)) return(regcheck)
    regcheck = left_join(regcheck, stata_check_df, by="step") %>%
      mutate(stata_ok = na.val(TRUE))

    regcheck = regcheck %>%
      mutate(
        r_did_run = did_run,
        problem = ifelse(stata_ok, problem, paste0(problem,"\nStata error r(", stata_err_code,")")),
        did_run = did_run & !is.true(stata_ok==FALSE)
      )

  }

  regcheck
}

add_cols_if_not_exist = function(df, ...) {
  args = list(...)
  if(is.null(df)) return(df)
  for (col in names(args)) {
    if (is.null(args[[col]])) next
    if (has.col(df, col)) next
    df[[col]] = rep(args[[col]], length = NROW(df))
  }
  df
}

check_mr_class = function(mr, fun=NULL) {
  if (!is(mr, "metareg_study")) {
    if (!is.null(fun)) {
      stop(paste0("Your custom function for ", fun, " did not return a proper mr object. Please make sure that it always returns the mr object, whih is passed as first argument. The mr object may be possibly modified inside the function, e.g. by calling mr_set_result."))
    } else {
      stop("One of your custom functions did not return a proper mr object. Please make sure that your custom functions: step_run_fun, study_agg_fun and stata_agg_fun always return the mr object, whih is passed as first argument. The mr object may be possibly modified inside the function, e.g. by calling mr_set_result.")

    }

  }
}

mr_load_reg_ifrows = function(project_dir, step) {
  restore.point("load_ifrows")
  dir = file.path(project_dir,"metareg","dap","stata", "ifrows")
  file = paste0(dir,"/ifrows_", step, ".dta")
  if (!file.exists(file)) return(list(ok=FALSE))
  row_dat = haven::read_dta(file)
  list(ok=TRUE, ifrows = row_dat[[1]])
}


eval_reg_if_condition = function(project_dir, step, reg, dat) {
  restore.point("eval_reg_if_condition")
  if_str = gsub("<-","< -", reg$if_str, fixed=TRUE)
  # In Stata ~= is a synonym for !=
  if_str = gsub("~=","!=",if_str,fixed=TRUE)
  # Replace var == . with is.na(var)
  if_str = replace.stata.is.na(if_str)

  if_str = paste0("as.logical(", if_str,")")
  code = paste0("filter(dat, ", if_str,")")
  try_dat = try(parse_eval(code),silent = TRUE)

  # If condition can be evaluated
  if (!is(try_dat,"try-error")) return(try_dat)

  res = mr_load_reg_ifrows(project_dir, step)
  # ifrows were stored
  if (res$ok) {
    dat = dat[res$ifrows,]
    return(dat)
  }

  cols = colnames(dat)
  call = try(parse.as.call(if_str), silent=TRUE)
  if (is(call, "try-error")) stop("if condition cannot be evaluated")

  vars = find.call.variables(call)
  unknown = vars[!vars %in% cols]
  if (length(unknown)==0) {
    stop("if condition cannot be evaluated")
  }

  i = 1
  for (i in seq_along(unknown)) {
    col = cols[startsWith(cols, unknown[i])]
    if (is.na(col)) stop("if condition cannot be evaluated")

    if_str = gsub(unknown[i],col, if_str, fixed=TRUE)
  }
  code = paste0("filter(dat, ", if_str,")")
  dat = parse_eval(code)
  dat
}
