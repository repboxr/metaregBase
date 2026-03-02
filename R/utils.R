touchFile = function (pathname, ...) {
  if (!exists("Sys.setFileTime", mode = "function")) {
    Sys.setFileTime <- function(path, ...) {
      info <- file.info(pathname)
      if (info$isdir) {
        stop(sprintf("In R v%s, it is not possible to change the timestamp of a directory: %s",
                     getRversion(), pathname))
      }
      con <- NULL
      on.exit({
        if (!is.null(con)) close(con)
      })
      if (info$size == 0) {
        con <- file(pathname, open = "w")
      }
      else {
        con <- file(pathname, open = "r+b")
        seek(con = con, where = 0, origin = "start",
             rw = "read")
        bfr <- readBin(con = con, what = raw(), n = 1)
        seek(con = con, where = 0, origin = "start",
             rw = "write")
        writeBin(con = con, bfr)
      }
      invisible(TRUE)
    }
  }
  pathname <- as.character(pathname)
  nPathnames <- length(pathname)
  if (nPathnames == 0L)
    return(invisible(NULL))
  if (nPathnames > 1L) {
    res <- lapply(pathname, FUN = touchFile, ...)
    res <- Reduce(c, res)
    return(invisible(res))
  }
  if (!file.exists(pathname))
    stop("No such file: ", pathname)
  info <- file.info(pathname)
  oldTimestamp <- info$mtime
  if (!Sys.setFileTime(pathname, time = Sys.time())) {
    cat("\nFailed to set timestamp: ", pathname)
  }
  invisible(oldTimestamp)
}


rstudio_job = function(name, expr, libs=NULL, wdir = getwd(), script.dir = tempdir(), importEnv=TRUE) {
  expr = substitute(expr)
  restore.point("as_job")
  code = deparse1(expr)

  #code = paste0('setwd("',wdir,'")\n', code)
  if (!is.null(libs)) {
    code = paste0(paste0("library(", libs,")", collapse="\n"), "\n", code)
  }
  cat("\nRun \n\n",code,"\n\n")

  script.file = file.path(script.dir, paste0(name,".R"))
  writeLines(code, script.file)
  rstudioapi::jobRunScript(path = script.file,workingDir = wdir, name = name,importEnv = importEnv)

}



fe = function(val) {
  as.factor(val)
}


df.rows.to.list = function(df) {
  lapply(seq_len(NROW(df)), function(i) df[i,])
}

