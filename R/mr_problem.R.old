# TO DO: Replace  note_problem with repbox_problem everywhere...


# Functions to correctly handle errors and other problems when conduction
# a metareg study


note_problem = function(problem_type, problem_descr, step = get_repbox_problem_step(), stop=TRUE) {
  restore.point("note_problem")
  repbox_problem(msg=msg, type=type)
  return()
  # TO DO: Retrieve problem context
  msg = paste0("Problem type: ", problem_type, "\n\n", problem_descr)

  # We don't yet use repbox_set_problem
  # Instead look at try catch code around mr_analysis_step
  #if (!is.null(step)) {
  #  repbox_set_step_problem(step, problem_type, problem_descr)
  #}
  if (stop) {
    stop(msg)
  } else {
    cat(msg)
    #warning(msg)
  }
}

repbox_set_step_problem = function(step, problem_type, problem_descr) {
  log_li = getOption("repbox.mr.run.step.problems")
  if (is.null(log_li)) log_li = list()
  log_li[[length(log_li)+1]] = tibble(step = step, problem_type = problem_type, problem_descr = problem_descr)
  options(repbox.mr.run.step.problems = log_li)
}

repbox_get_step_problems = function() {
  log_li = getOption("repbox.mr.run.step.problems")
  if (length(log_li)==0) {
    return(NULL)
  }
  bind_rows(log_li)
}

# Currently type is ignored, but we may log different types like "run"
repbox_reset_problem_log = function(type) {
  options(
    repbox.mr.run.step.problems = list()
  )
}

get_repbox_run_problems = function() {
  options(repbox.mr.run.step.problems = list())
}

clear_repbox_problem_step = function() {
  options(repbox.problem.step = NULL)
}

set_repbox_problem_step = function(step) {
  options(repbox.problem.step = step)
}

get_repbox_problem_step = function(step) {
  getOption("repbox.problem.step")
}




clear_problem_files = function(mr) {
  problem.file = file.path(mr$project_dir,"metareg",mr$metaid,"has_problem")
  if (file.exists(problem.file)) file.remove(problem.file)
  problem.file = file.path(mr$project_dir,"metareg",mr$metaid,"no_reg")
  if (file.exists(problem.file)) file.remove(problem.file)
}



mr_set_problem = function(mr, problem, msg=NULL) {
  mr$has_problem = TRUE
  mr$problem=problem
  mr$problem_msg = paste0(problem,"\n", msg)
  #problem.file = file.path(mr$project_dir,"metareg",mr$metaid,"has_problem")
  #writeLines(problem.file, problem)
  if (!is.null(msg)) {
    cat("\n",msg,"\n")
  }
  mr
}

mr_has_problem = function(mr) {
  if (isTRUE(mr$has_problem)) return(TRUE)
  problem.file = file.path(mr$project_dir,"metareg",mr$metaid,"has_problem")
  file.exists(problem.file)
}


mr_write_empty_study = function(mr) {
  file = file.path(mr$project_dir,"metareg",mr$metaid,"no_reg")
  writeLines("TRUE", file)
  return(invisible(mr))
}


mr_infeasible = function(..., step=NA) {
  args = list(...)
  restore.point("mr_infeasible")
  msg.inds = which(names(args)=="")
  val.inds = which(names(args)!="")
  if (length(msg.inds)==0) stop("No msg specified")
  if (length(val.inds)>0) {
    if (max(msg.inds)>min(val.inds))
      stop("You have to first specify all message components as unnamed arguments and afterwards possible named values.")
  }
  msg = paste0(args[msg.inds], collapse = "")
  inf = as_tibble(c(list(step=step, msg=msg), args[val.inds]))
  class(inf) = c("infeasible",class(inf))
  mr_cat(paste0(msg))

  inf
}

mr_is_infeasible = function(x) {
  is(x, "infeasible")
}
