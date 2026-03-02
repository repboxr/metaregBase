example = function() {
  project = "testsupp"
  project_dir = file.path("~/repbox/projects_reg",project)
  mr_base_aggregate_again(project_dir)

  res = mr_for_all_projects("~/repbox/projects_reg",mr_base_aggregate_again, stop.on.error = FALSE)

  #res = mr_for_all_projects("~/repbox/projects_reg",mr_base_aggregate_again, stop.on.error = TRUE, just.num = 2)

  has_err = mr_for_all_projects("~/repbox/projects_reg",function(project_dir) {
    tibble(project = basename(project_dir), agg_error = file.exists(file.path(project_dir, "metareg/base/has_error_aggregate_again.txt")))
  }) %>% bind_rows()


}

#' Newly aggregate results
#'
#' Aggregation is also part of mr_run. mr_aggregate_agin can be called if just the
#' aggregation functions have changed and it is not intended to run all
#' analysis again
mr_aggregate_again = function(mr) {
  restore.point("mr_aggregate_again")


  err.file = file.path(mr$out_dir, "has_error_aggregate_again.txt")
  writeLines(as.character(Sys.time()), err.file)

  if (!is.null(mr$stata_agg_fun)) {
    start = as.numeric(Sys.time())
    mr$stata_agg = mr$stata_agg_fun(mr)
    mr$stata_agg_runtime = as.numeric(Sys.time())-as.numeric(start)
  }


  start = as.numeric(Sys.time())
  mr$agg = mr$study_agg_fun(mr)
  mr$agg_runtime = as.numeric(Sys.time())-as.numeric(start)
  file.remove(err.file)

  mr
}



#' Helper functtion to aggregate the Rds files saved by a study's step_run_fun
#'
#' This function is useful for a study's agg.fun.
#' We assume that each result Rds file contains a data frame.
#'
#' @param mr The mr object passed to agg.fun.
#' @param glob A glob that matches all Rds files that shall be aggregated. If step_run_fun generates different types of Rds files, we can specify the types we want to aggregate in this call.
#' @param file_col The result will have a column that contains the file name of the Rds file that is aggregated. You can specify the name of that column with the file_col argument.
#' @returns A tibble with the aggregated values.

mr_agg_df_rds = function(mr, glob="*.Rds", file_col = "result_file") {
  restore.point("mr_agg_df_rds")
  files = list.files(mr$step.dir, glob2rx(glob), full.names=TRUE)
  li = lapply(files, readRDS)
  if (length(li)==0) return(NULL)
  if (is.null(file_col)) {
    return(bind_rows(li))
  }
  res = bind_rows(li, .id=file_col)
  res[[file_col]] = basename(files[ as_integer(res[[file_col]]) ])
  res
}

# Aggregates regression statistics stored with svret
mr_agg_stata_reg_scalars = function(mr, file_prefix="regscalar_") {
  restore.point("mr_agg_stata_reg_scalars")
  glob = paste0(file_prefix, "*",".txt")
  files = list.files(mr$step.dir, glob2rx(glob), full.names=TRUE)
  step.df = mr$step.df
  file = first(files)
  li = lapply(files, function(file) {
    df = read_var_equal_val_file(file,as.numeric = TRUE)
    if (!is.null(df)) {
      step = as_integer(str.between(file, file_prefix, "__"))
      variant = str.between(file, "__", ".txt")
      df$step = rep(step, NROW(step))
      df$variant = rep(variant, NROW(step))
    }
    return(df)
  })
  res = bind_rows(li)
  res
}

# Aggregates regression statistics stored with svret
mr_agg_stata_reg_macros = function(mr, file_prefix="regmacro_") {
  restore.point("mr_agg_stata_reg_macros")
  glob = paste0(file_prefix, "*",".txt")
  files = list.files(mr$step.dir, glob2rx(glob), full.names=TRUE)
  step.df = mr$step.df
  file = first(files)
  li = lapply(files, function(file) {
    df = read_var_equal_val_file(file,as.numeric = FALSE)
    if (!is.null(df)) {
      step = as_integer(str.between(file, file_prefix, "__"))
      variant = str.between(file, "__", ".txt")
      df$step = rep(step, NROW(step))
      df$variant = rep(variant, NROW(step))

    }
    return(df)
  })
  res = bind_rows(li)
  res
}



mr_agg_stata_parmest = function(mr, file_prefix="reg_", missing.step="stop") {
  restore.point("mr_agg_stata_parmest")
  glob = paste0(file_prefix, "*",".dta")
  files = list.files(mr$step.dir, glob2rx(glob), full.names=TRUE)
  if (length(files)==0) return(NULL)

  step.df = mr$step.df
  file = files[1]
  old.cols = c("parm","label","estimate","stderr","dof", "z","p","min95","max95")
  new.cols = c("var","label", "coef","se","dof", "t","p","ci_low","ci_up")




  li = lapply(files, function(file) {
    restore.point("kahkdhskdhk")
    df = haven::read_dta(file)
    df = rename.cols(df, old.cols, new.cols)
    df = df[,intersect(new.cols, colnames(df))]
    if (!is.null(df)) {
      has.variant = has.substr(file,"__")
      if (has.variant) {
        step = as_integer(str.between(file, file_prefix, "__"))
        variant = str.between(file, "__", ".dta")
      } else {
        step = as_integer(str.between(file, file_prefix, ".dta"))
        variant = ""
      }
      df$step = rep(step, NROW(step))
      df$variant = rep(variant, NROW(step))
      df$cmd = step.df$cmd[step]
    }
    return(df)
  })
  res = bind_rows(li)

  if (missing.step %in% "stop") {
    if (any(is.na(res$step))) {
      stop("Could not parse the step number from the parmest output files. Make sure that you have specified correctly the file names in your Stata parmest code and provide the correct prefix. E.g. if your files have a name like 'reg_out_5.dta' where the 5 is the step, you need to set prefix = 'reg_out_'.")
    }
  }

  res
}

# Extract marginal effects for dprobit commands
# Unlike margins, dprobit (and mfx) treats dummy variables automatically as
# discrete, i.e. we have special code.
mr_agg_add_dprobit_coef = function(mr, stata_ct) {
  restore.point("mr_agg_add_dprobit_coef")
  glob = paste0("dprobit_", "*",".csv")
  files = list.files(mr$step.dir, glob2rx(glob), full.names=TRUE)
  if (length(files)==0) return(stata_ct)

  #step.df = mr$step.df
  #new.cols = c("var","label", "coef","se","dof", "t","p","ci_low","ci_up")

  df = lapply(files, function(file) {
    df = read.csv(file)
    if (!is.null(df)) {
      step = as_integer(str.between(file, "dprobit_", ".csv"))
      df$step = rep(step, NROW(step))
    }
    return(df)
  }) %>% bind_rows()
  df$t = df$coef / df$se
  df$ci_low = df$ci_up = NA_real_
  df$variant = rep("sb_mfx", NROW(step))
  df$cmd = "dprobit"

  # p-value is the same as for the original coefficient
  df = left_join(df, stata_ct %>% filter(variant=="sb") %>% select(step, var,p,label), by=c("step","var"))



  return(bind_rows(stata_ct,df))
}

mr_agg_estout_tsv = function(mr, file_prefix="reg_", colnames = c("var","coef","se","t","p","ci_low","ci_up")) {
  restore.point("mr_agg_estout_tsv")
  glob = paste0(file_prefix, "*",".tsv")
  files = list.files(mr$step.dir, glob2rx(glob), full.names=TRUE)
  step.df = mr$step.df
  li = lapply(files, function(file) {
    df = read_estout_tsv(file, colnames)
    if (!is.null(df)) {
      step = as_integer(str.between(file, file_prefix, "__"))
      variant = str.between(file, "__", ".tsv")
      df$step = rep(step, NROW(step))
      df$variant = rep(variant, NROW(step))
      df$cmd = step.df$cmd[step]
    }
    return(df)
  })
  res = bind_rows(li)
  res
}

read_estout_tsv = function(file, colnames=c("var","coef","se","t","p","ci_low","ci_up")) {
  tsv.txt = readLines(file,warn=FALSE)
  if (length(tsv.txt)>2) {
    df = data.table::fread(text=tsv.txt, sep="\t", na.strings=".", skip=2, header=FALSE) %>% as_tibble()
    colnames(df) = colnames
  } else {
    df = NULL
  }
  df
}
