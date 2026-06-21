### Description of the Issue and Proposed Fix

The issue is caused by Stata's logging mechanism when dealing with nested `do` scripts. When `main.do` is running, it logs its output to `log_3.log`. As it executes `do main_part1`, Stata's behavior replicates the standard output across **all open log files**. During the execution of the included script `repbox_main_part1.do`, a second log file is explicitly opened (`include_1.log`).

This results in the injected text markers (e.g., `#~# INJECT RUNCMD 1 1 2` and `#~# INJECT REG_ERETURN ...`) being printed to **both** `log_3.log` and `include_1.log`. 

When `extract.stata.logs()` processes the logs, it indiscriminately extracts all blocks matching the marker from every log file. As a result, the extracted `log.df` will contain duplicate entries for the commands inside `main_part1.do` and `main_part2.do`. Later, when `extract.stata.results()` performs a `left_join(run.df, log.df, ...)`, the duplication in `log.df` directly multiplies the rows in `run.df` (and thus `stata_run_cmd`).

This downstream duplication wreaks havoc on `repboxDRF`'s `drf$path_df` and causes `drf_get_data` to fail because the pipeline encounters multiple identical data loading scripts with different synthetic `runid`s, breaking dependency chaining and execution tracking.

**Proposed Fix:**
Since each log file name directly embeds the `donum` of the executing script (either as `log_<donum>.log` or `include_<donum>.log`), we can filter the parsed injection blocks. We extract the `donum` from the log filename and only keep the matched blocks where `bdf$donum` corresponds to the log file's `donum`. We apply this filtering logic to both the `extract.stata.logs` and `rsr_extract_stata_reg_output` functions.

### Code Modifications

!MODIFICATION extract.stata.logs repboxStata/R/extract.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/extract.R"
function_name = "extract.stata.logs"
description = "Filter log blocks to only keep those belonging to the current file's donum to prevent duplicate blocks from included do files."
---
```R
extract.stata.logs = function(project_dir) {
  restore.point("extract.stata.logs")
  dir = file.path(project_dir, "repbox/stata/logs")
  files = list.files(dir, pattern = "^(log|include)_.*\\.log$", full.names = TRUE)

  res.li = lapply(files, function(file) {
    txt = readLines(file,warn=FALSE) %>% enc2utf8()
    check.stata.log.for.critical.problems(txt)
    bdf = extract.inject.blocks(txt, type="RUNCMD")

    file_donum = as.integer(str.between(basename(file), "_", ".log"))
    if (!is.na(file_donum) && NROW(bdf) > 0) {
      bdf = bdf[bdf$donum == file_donum, , drop = FALSE]
    }

    if (NROW(bdf) == 0) return(tibble())

    log.df = lapply(seq_len(NROW(bdf)), function(i) {
      str = bdf$str[[i]]
      donum = bdf$donum[i]
      line=bdf$line[i]
      counter=bdf$counter[i]
      ignore = has.substr(str,"#~# INJECT") | has.substr(str,"#~# END INJECT")
      str = str[!ignore]
      if (isTRUE(str[length(str)]==".")) str[-length(str)]
      #str = str[nchar(str)>0]
      logtxt = merge.lines(str)

      # We don't store log of a custom function
      # inside which we store logs again
      if (grepl("!.REPBOX.CUSTOM.PROGRAM>*",logtxt, fixed=TRUE)) {
        logtxt = ""
      }

      # To avoid later invalid multibyte string errors
      logtxt = iconv(logtxt, to="UTF-8", sub="?")
      logtxt[is.na(logtxt)] = ""
      logtxt = gsub("capture:  noisily: ","",logtxt,fixed = TRUE)
      tibble(logfile = rep(basename(file), length(logtxt)), donum=donum, line=line, counter=counter,logtxt=logtxt)
    }) %>% bind_rows()
  })

  log.df = bind_rows(res.li)
  return(log.df)
}
```
!END_MODIFICATION extract.stata.logs repboxStata/R/extract.R

!MODIFICATION rsr_extract_stata_reg_output repboxStataReg/R/extract_reg_stata.R
scope = "function"
file = "/home/rstudio/repbox/repboxStataReg/R/extract_reg_stata.R"
function_name = "rsr_extract_stata_reg_output"
description = "Filter REG_ERETURN log blocks to prevent duplicates from included do files."
---
```R
rsr_extract_stata_reg_output = function(project_dir, run.df=NULL, dotab=NULL, save=TRUE) {
  restore.point("rsr_extract_stata_reg_output")

  #if (is.null(runid_map)) {
  #  runid_map = readRDS(file.path(project_dir, "repbox/stata/runid_repbox_map.Rds"))
  #}

  if (is.null(run.df) | is.null(dotab)) {
    repbox_results = readRDS(file.path(project_dir, "repbox/stata/repbox_results.Rds"))
    run.df = repbox_results$run.df
    dotab = repbox_results$dotab
  }

  artid = basename(project_dir)
  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1. Extract TSV information stored by esttab
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  res.dir = file.path(project_dir,"repbox/stata/tsv")
  files = list.files(res.dir,glob2rx(paste0("*.dta")),full.names = TRUE)

  bfiles = basename(files)
  donum = str.left.of(bfiles, "_") %>% as_integer()
  str = str.right.of(bfiles,"_")
  line = str.left.of(str, "_") %>% as_integer()
  str = str.right.of(str,"_")
  counter = str.remove.ends(str, right=4) %>% as_integer()

  regtab = tibble(regresfile=files,donum=donum,line=line,counter=counter) %>%
    arrange(donum, line, counter) %>%
    group_by(donum, line) %>%
    mutate(run = seq_len(n())) %>%
    ungroup()

  if (NROW(regtab) > 0) {
    regtab$ct = lapply(regtab$regresfile, function(file) {
      restore.point("inner.read.regres")
      regres = haven::read_dta(file)
      old.cols = c("eq","parm","label","estimate","stderr","dof", "z","p","min95","max95")
      new.cols = c("eq","var","label", "coef","se","dof", "t","p","ci_low","ci_up")
      regres = rename.cols(regres, old.cols, new.cols)
      regres = regres[,intersect(new.cols, colnames(regres)), drop=FALSE]
      if (!"eq" %in% colnames(regres)) {
        regres$eq = rep("", NROW(regres))
      }
      regres
    })
  } else {
    regtab$ct = list()
  }

  if ("regresfile" %in% names(regtab)) {
    regtab = select(regtab, -regresfile)
  }

  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2. Extract regression information stored in logs
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  dir = file.path(project_dir, "repbox/stata/logs")
  log.files = list.files(dir, pattern = "^(log|include)_.*\\.log$", full.names = TRUE)

  reg.log = lapply(log.files, function(file) {
    log.txt = readLines(file,warn=FALSE)  %>% enc2utf8()
    bdf = extract.inject.blocks(log.txt, type="REG_ERETURN")

    file_donum = as.integer(str.between(basename(file), "_", ".log"))
    if (!is.na(file_donum) && NROW(bdf) > 0) {
      bdf = bdf[bdf$donum == file_donum, , drop = FALSE]
    }

    if (NROW(bdf) == 0) return(tibble())

    bdf$er = lapply(bdf$str,parse.ereturn.injection)
    bdf
  }) %>% bind_rows()

  if (NROW(reg.log) > 0) {
    regtab = left_join(regtab, select(reg.log, donum, line, counter, er), by=c("donum","line","counter"))
  } else if (NROW(regtab) > 0) {
    regtab$er = vector("list", NROW(regtab))
  }

  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3. Merge with run.df
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  if (NROW(regtab) > 0) {
    regtab = left_join(regtab,run.df, by=c("donum","line","counter"))

    # UPDATE: Only consider regression where missing_data = FALSE
    # Otherwise we likely have faulty regressions that use an earlier data set
    # Also ignore regression results with run error
    regtab = regtab[regtab$has.data & !regtab$runerr,]
  }

  if (NROW(regtab) > 0) {
    regtab$artid = artid

    # merge with dotab to get doid
    regtab = regtab %>% left_join(dotab %>% select(donum, doid), by="donum")

    #colnames(regtab)
    cols = c("artid", "runid", "donum", "doid", "line", "counter","cmd", "cmdline","ct","er", "datasig", "timevar" ,"panelvar", "tdelta",        "runerr",        "runerrcode",    "runerrmsg",     "runsec", "orgline", "in.program", "has.data")

    # Ensure all columns exist to avoid errors
    cols_exist = intersect(cols, colnames(regtab))
    regtab = regtab[,cols_exist]
  }

  if (save) {
    saveRDS(regtab, file.path(project_dir,"repbox/stata/regtab.Rds"))
  }

  regtab
}
```
!END_MODIFICATION rsr_extract_stata_reg_output repboxStataReg/R/extract_reg_stata.R
