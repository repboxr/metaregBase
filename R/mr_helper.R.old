# Some metareg helper functions

# Loads the data set for a particular regression
# This is not the most efficient way to run a meta study with several
# regressions in an article, but should rather be used if for testing
# purposes you want to first examine a particular regression.
mr_load_reg_data = function(project_dir, step) {
  restore.point("mr_load_reg_data")
  #project_dir = "~/repbox/projects_gha/aejapp_3_4_9"
  #step = 4

  step_fun = function(mr,...) {
    args = list(...)
    restore.point("step_fun")
    mr$my.results = args
    mr
  }

  mr = mr_init_study(project_dir,metaid = "temp",version = 0,step_run_fun = step_fun)
  mr = mr_run(mr, asteps=step,run_stata = FALSE, do_agg=FALSE)
  my.results = mr$my.results
  mr$my.results = NULL
  c(list(mr=mr), my.results)
}

mr_base_variant = function(mr) {
  mr$repdb$reg
}

mr_uses_no_reg = function(mr) {
  asteps = mr_get_asteps(mr)
  length(asteps)==0
}

#' Specify which analysis steps are considered in this metareg study
#'
#' @param x is a data frame that
#'    has columns "step" and a logical column "use_step".
#'    Such a data frame will typically be a
#'    mutated version of reg_df where the colum "use_reg" is added.
mr_specify_used_reg = function(mr, reg_df, use_reg = suppressWarnings(reg_df$use_reg)) {
  if (NROW(reg_df)==0) {
    mr$used_steps = integer(0)
    return(mr)
  }
  if (!all(c("step") %in% names(reg_df))) {
    stop("If you provide a data frame it must have the columns 'step' (TRUE / FALSE).")
  }
  if (!is.null(use_reg)) {
    mr$used_steps = reg_df$step[reg_df$use_reg]
  } else {
    mr$used_steps = reg_df$step
  }
  mr
}

mr_projects_have_file = function(parent.dir, files) {
  restore.point("mr_projects_have_file")

  mr_for_all_projects(parent.dir, verbose=FALSE, function(project_dir) {
    res = tibble(project = basename(project_dir))
    for (file in files) {
      base = basename(file)
      res[[base]] = file.exists(file.path(project_dir, file))
    }
    res
  }) %>% bind_rows()

}


#' Call a function for projects in parent.dir
mr_for_all_projects = function(parent.dir, fun, project.dirs, verbose=TRUE, stop.on.error = TRUE, store.results=TRUE, just.num = NULL) {
  if (missing(project.dirs)) {
    project.dirs = list.dirs(parent.dir,recursive = FALSE)
  }
  if (!is.null(just.num)) {
    project.dirs = project.dirs[just.num]
  }

  res = lapply(seq_along(project.dirs), function(i) {
    project_dir = project.dirs[i]
    if (verbose) {
      cat("\n", i, " of ", length(project.dirs), " ", basename(project), "\n")
    }
    if (stop.on.error) {
      res = fun(project_dir)
    } else {
      res = try(fun(project_dir))
    }
    if (store.results) return(res)
    return(NULL)
  })
  if (store.results) {
    names(res) = basename(project.dirs)
    return(res)
  }
  return(NULL)

}
