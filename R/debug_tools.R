set_debug_mode = function(debug_mode=TRUE) {
  options(repbox.debug_mode = debug_mode)
}

is_debug_mode = function() {
  isTRUE(getOption("repbox.debug_mode"))
}


source_with_log = function(
  file=NULL,
  log_file=NULL,
  append = TRUE,
  split = TRUE,
  chdir = FALSE,
  envir = parent.frame(),
  echo = TRUE,
  print.eval = TRUE,
  prompt.echo = getOption("prompt"),
  max.deparse.length = Inf,
  keep.source = getOption("keep.source"),
  log_con = NULL, close_con=is.null(log_con)
) {
  restore.point("source_with_log")
  stopifnot(is.character(file), length(file) == 1L)
  options(error = function() mrb_test_traceback())
  if (is.null(log_con)) {
    stopifnot(is.character(log_file), length(log_file) == 1L)
    log_con = file(log_file, open = if (append) "at" else "wt")
  }

  output_sink_before = sink.number(type = "output")
  message_sink_before = sink.number(type = "message")

  warnings_seen = character()
  error_obj = NULL
  value = NULL

  on.exit({
    options(error = function() traceback(3))
    while (sink.number(type = "message") > message_sink_before) {
      sink(type = "message")
    }
    while (sink.number(type = "output") > output_sink_before) {
      sink()
    }
    if (close_con) {
      try(close(log_con), silent=TRUE)
    }
  }, add = TRUE)

  sink(log_con, split = split)
  sink(log_con, type = "message")

  cat("\n")
  cat("### source_with_log start:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("### file:", normalizePath(file, winslash = "/", mustWork = FALSE), "\n\n")

  value = withCallingHandlers(
      source(
        file = file,
        local = envir,
        echo = echo,
        print.eval = print.eval,
        prompt.echo = prompt.echo,
        max.deparse.length = max.deparse.length,
        chdir = chdir,
        keep.source = keep.source
      )
    # tryCatch(
    #   source(
    #     file = file,
    #     local = envir,
    #     echo = echo,
    #     print.eval = print.eval,
    #     prompt.echo = prompt.echo,
    #     max.deparse.length = max.deparse.length,
    #     chdir = chdir,
    #     keep.source = keep.source
    #   ),
    #   error = function(e) {
    #     error_obj <<- e
    #     msg = conditionMessage(e)
    #     cat("\nError in source_with_log:\n", file = stderr(), sep = "")
    #     cat(msg, "\n", file = stderr(), sep = "")
    #     NULL
    #   }
    # )
    ,
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  cat("\n### source_with_log end:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

  invisible(list(
    ok = is.null(error_obj),
    value = value,
    error = error_obj,
    warnings = warnings_seen
  ))
}

mrb_test_traceback = function() {
  traceback(3)
  #restore.point("mrb_test_traceback")
  #print(str)
}
