You can cleanly achieve this goal by creating a new `stata_cmds_nomrbreg()` function in `repboxStata` and passing it to the original injection parser so that it captures `parmest` output exactly like standard regressions. However, because it receives the `no_mrb_reg` tag in `repboxDRF`, it will be safely ignored when it comes to Path Generation, `mrb_r_base` reconstruction, and the final `regcheck` parcel comparisons.

Here is the plan of changes:
1. **`repboxStata`**: Add `stata_cmds_nomrbreg()` separating `reg3`, `sem`, and `gsem` from `quasi_reg`. Include this new list into the `reg.cmds` default argument for `parse.sup.do`, `stata.inject.and.run`, `inject.do` and `normalized.cmdlines.to.tab` so they receive standard regression injection during the original reproducibility run.
2. **`repboxDRF`**: Add the `"no_mrb_reg"` designation to `drf_stata_cmd_types()` and map the appropriate commands in `drf_stata_cmd_types_vec()`. By not putting it into `drf_acmds()`, these commands are correctly excluded from DRF paths.
3. **`repboxStataReg`**: Update `rsr_make_quasi_and_post_reg_parcels()` to also output a `no_mrb_reg` metadata parcel. The `regcoef_so` coefficients will naturally exist for these runs since `mrb_so` automatically consumes everything stored into `regtab.Rds`.

Here are the requested modifications.

!MODIFICATION stata_cmds_nomrbreg repboxStata/R/stata_cmd_lists.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/stata_cmd_lists.R"
insert_after_fun = "stata_cmds_quasireg"
description = "Add a function that lists regression commands to be parsed but not tracked for metaregBase"
---
```r
stata_cmds_nomrbreg = function() {
  c("reg3", "sem", "gsem")
}
```
!END_MODIFICATION stata_cmds_nomrbreg repboxStata/R/stata_cmd_lists.R

!MODIFICATION stata_cmds_quasireg repboxStata/R/stata_cmd_lists.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/stata_cmd_lists.R"
function_name = "stata_cmds_quasireg"
description = "Remove reg3, sem, gsem from quasi_reg commands"
---
```r
stata_cmds_quasireg = function() {
  c("rd", "rdrobust","psmatch2","leebounds", "a2reg","xtabond2","altrdrobust","hausman","newey2","hetprob","reg2hdfespatial","gmm")
}
```
!END_MODIFICATION stata_cmds_quasireg repboxStata/R/stata_cmd_lists.R

!MODIFICATION parse.sup.do repboxStata/R/parse.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/parse.R"
function_name = "parse.sup.do"
description = "Include no_mrb_reg commands in reg.cmds default argument to allow injection"
---
```r
parse.sup.do = function(file, reg.cmds = c(get.regcmds(), stata_cmds_nomrbreg()), project_dir="", catch.err=TRUE, code=NULL, stop.on.error=FALSE) {
  restore.point("parse.sup.do")
  #if (endsWith(file,"Table6.do")) {
  #  restore.point("jskfjlsfjlsflkjlf")
  #  stop()
  #}
  project=basename(project_dir)
  if (is.null(code)) {
    txt = readLines(file,warn = FALSE)
    # To avoid invalid multibyte string errors
    txt = enc2utf8(txt)
  } else {
    txt = sep.lines(code)
  }
  # NEW: Try to always robustly encode
  txt = iconv(txt, from = "", to = "UTF-8", sub = "")
  txt = enc2utf8(txt)

  err = NULL
  next.cmd = "normalize.do"
  if (stop.on.error) {
    s = repbox.normalize.do(txt,file)
    next.cmd = "make.tab"
    res = repbox.do.table(s)
    tab=res$tab; ph.df = res$ph.df
  } else {
    err = try({
      s = repbox.normalize.do(txt,file)
      next.cmd = "make.tab"
      res = repbox.do.table(s)
      tab=res$tab; ph.df = res$ph.df
    })

  }
  if (is(err,"try-error")) {
    return(tibble(project=project, project_dir=project_dir, file = file,ok=FALSE, save.dta = list(NULL), use.dta = list(NULL), ph = list(NULL), tab=list(NULL),num.reg.lines = 0, reg.lines=list(NULL), parse.err = TRUE, parse.err.type=next.cmd))
  }

  #tab = tab.add.dta.file.ext(tab)
  tab$is.regcmd = tab$cmd %in% reg.cmds
  tab = tab.add.cmd.installed(tab)
  # Find saved data sets

  reg.lines = which(tab$cmd %in% reg.cmds)
  loads.data = sum(tab$cmd %in% c("use","u","us", "import","guse","insheet","gzuse")) > 0
  do = list(project=project, project_dir=project_dir,file=file, dofile = basename(file), doid =tools::file_path_sans_ext(basename(file)),ok=TRUE, ph = list(ph.df), tab=list(tab),num.reg.lines = length(reg.lines), reg.lines = list(reg.lines), parse.err=FALSE, parse.err.type="")

  dta.info = lapply(static.do.use.dta.info(do),function(x) list(x))

  if (!is.null(dta.info$found.dta.tab[[1]]))
    tab = left_join(tab, dta.info$found.dta.tab[[1]], by="line")
  do$tab[[1]] = tab
  do = as_tibble(c(do,dta.info))
  do$loads.data = loads.data
  do

}
```
!END_MODIFICATION parse.sup.do repboxStata/R/parse.R

!MODIFICATION normalized.cmdlines.to.tab repboxStata/R/parse.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/parse.R"
function_name = "normalized.cmdlines.to.tab"
description = "Include no_mrb_reg commands in reg.cmds for 'for any' repair logic"
---
```r
normalized.cmdlines.to.tab = function(txt, ph.df, orglines=NULL) {
  restore.point("normalized.cmdlines.with.ph.to.tab")

  str = txt

  # STRIP LEADING WHITESPACE SO THAT startsWith() MATCHES WORK CORRECTLY
  str = trimws(str)

  saving = str.right.of(str,"saving#~br",not.found = NA_character_)
  srows = which(!is.na(saving))
  if (length(srows)>0) {
    saving[srows] = str.left.of(saving[srows],"~#")
    saving[srows] = paste0("#~br", saving[srows],"~#")
  }

  quietly = rep(NA_character_, length(str))
  capture = rep(NA_character_, length(str))
  noisily = rep(NA_character_, length(str))

  changed = TRUE
  while(changed) {
    changed = FALSE

    rows = startsWith(str, "quietly:")
    if (any(rows)) { quietly[rows] = "quietly:"; str[rows] = trimws(str.right.of(str[rows], "quietly:")); changed = TRUE }
    rows = startsWith(str, "quietly ")
    if (any(rows)) { quietly[rows] = "quietly "; str[rows] = trimws(str.right.of(str[rows], "quietly ")); changed = TRUE }
    rows = startsWith(str, "qui:")
    if (any(rows)) { quietly[rows] = "qui:"; str[rows] = trimws(str.right.of(str[rows], "qui:")); changed = TRUE }
    rows = startsWith(str, "qui ")
    if (any(rows)) { quietly[rows] = "qui "; str[rows] = trimws(str.right.of(str[rows], "qui ")); changed = TRUE }

    rows = startsWith(str, "capture:")
    if (any(rows)) { capture[rows] = "capture:"; str[rows] = trimws(str.right.of(str[rows], "capture:")); changed = TRUE }
    rows = startsWith(str, "capture ")
    if (any(rows)) { capture[rows] = "capture "; str[rows] = trimws(str.right.of(str[rows], "capture ")); changed = TRUE }
    rows = startsWith(str, "cap:")
    if (any(rows)) { capture[rows] = "cap:"; str[rows] = trimws(str.right.of(str[rows], "cap:")); changed = TRUE }
    rows = startsWith(str, "cap ")
    if (any(rows)) { capture[rows] = "cap "; str[rows] = trimws(str.right.of(str[rows], "cap ")); changed = TRUE }

    rows = startsWith(str, "noisily:")
    if (any(rows)) { noisily[rows] = "noisily:"; str[rows] = trimws(str.right.of(str[rows], "noisily:")); changed = TRUE }
    rows = startsWith(str, "noisily ")
    if (any(rows)) { noisily[rows] = "noisily "; str[rows] = trimws(str.right.of(str[rows], "noisily ")); changed = TRUE }
    rows = startsWith(str, "noi:")
    if (any(rows)) { noisily[rows] = "noi:"; str[rows] = trimws(str.right.of(str[rows], "noi:")); changed = TRUE }
    rows = startsWith(str, "noi ")
    if (any(rows)) { noisily[rows] = "noi "; str[rows] = trimws(str.right.of(str[rows], "noi ")); changed = TRUE }
  }


  # change :\ ad :/ as this is part of file path

  str =gsub(":\\","~;~\\", str, fixed=TRUE)
  str =gsub(":/","~;~\\", str, fixed=TRUE)

  colon1 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":")
  colon2 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":")
  colon3 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":") %>% trimws()
  str = gsub("~;~\\",":\\", str, fixed=TRUE)

  # Some commands use : in a different way. Then don't store colon stuff
  no.colon = which(startsWith(txt, "merge"))
  if (length(no.colon) > 0) {
    colon1[no.colon] = colon2[no.colon] = colon3[no.colon] = NA
    str[no.colon] = txt[no.colon]
  }

  str = gsub(","," ,", str, fixed=TRUE)
  cmd = str.left.of(str," ")
  str = paste0(" ",str.right.of(str," "))
  cmd_br = str.right.of(cmd,"#~br",not.found=NA)
  cmd_br = ifelse(is.na(cmd_br),NA,paste0("#~br",cmd_br))
  cmd = str.left.of(cmd, "#")

  opts = str.right.of(str,",",not.found=NA) %>% trimws()
  str = str.left.of(str,",")

  # Extracting weight variables [myweight] got more complicated:
  # if conditions can also contain [] like if id=id[_n-1]
  # we thus suppose that a weight string must have a space before
  # need to check whether that is indeed always the case
  weight = rep("", length(str))
  weight_start = stri_locate_first_regex(str,"(?<![a-z0-9A-Z_])\\[")[,1]
  wrows = which(!is.na(weight_start))
  if (length(wrows)>0) {
    weight_start = weight_start[wrows]
    rstr = substring(str[wrows], weight_start)
    weight_end = stri_locate_first_regex(rstr,"\\](?![a-z0-9A-Z_])")[,1]
    use_wrows = !is.na(weight_end)
    weight[wrows[use_wrows]] = stri_sub(rstr[use_wrows],2,weight_end[use_wrows]-1)
    str[wrows[use_wrows]] = stri_sub(str[wrows[use_wrows]], weight_start[use_wrows])
  }


  #weight = str.between(str,"[","]", not.found=NA) %>% trimws()
  #str = str.left.of(str,"[")


  # Default order is if, in, using
  # but sometimes different order is used like using, if

  res = extract.if.in.using(str)
  str = res$str
  using = res$parts$using
  in_arg = res$parts[["in"]]
  if_arg = res$parts[["if"]]

  exp = str.right.of(str,"=",not.found=NA)  %>% trimws()
  str = str.left.of(str,"=")
  arg_str = str

  cmd2 = str.left.of(trimws(arg_str)," ", not.found=NA_character_) %>% trimws()
  cmd2[startsWith(cmd2,"#~")] = NA_character_

  program = ifelse(startsWith(txt, "program define "), str.between(txt,"program define ", " "), NA)

  txt = replace.ph.keep.lines(txt, ph.df)
  arg_str = replace.ph.keep.lines(arg_str, ph.df)
  exp = replace.ph.keep.lines(exp, ph.df)
  cmd_br = replace.ph.keep.lines(cmd_br, ph.df)
  opts = replace.ph.keep.lines(opts, ph.df)

  na.rows = which(is.na(saving))
  saving = replace.ph.keep.lines(saving, ph.df)
  saving[na.rows] = NA_character_

  tab = data.frame(cmd,cmd_br=cmd_br,arg_str, exp, if_arg, in_arg, using, opts, cmd2, saving, txt, colon1, colon2,colon3, program, quietly, capture, noisily)

  # Special treatment for outdated "for any" command in combi with regression. Like
  # for any y1 y2: reg X z1
  # We cant well handle those regressions and thus change the command name
  rows = which(startsWith(tab$colon1,"for any"))
  if (length(rows)>0) {
    regcmds = c(get.regcmds(), stata_cmds_nomrbreg())
    rows = which(startsWith(tab$colon1,"for any") & tab$cmd %in% regcmds)
    tab$cmd[rows] = paste0("__for_any_", tab$cmd[rows])
  }


  tab = tab.repair.input.cmds(tab)

  if (is.null(orglines))
    tab$orgline = orglines
  tab = filter(tab, nchar(trimws(tab$txt))>0)
  tab$line = seq_len(NROW(tab))

  tab
}
```
!END_MODIFICATION normalized.cmdlines.to.tab repboxStata/R/parse.R

!MODIFICATION repbox.do.table repboxStata/R/parse.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/parse.R"
function_name = "repbox.do.table"
description = "Include no_mrb_reg commands in reg.cmds for 'for any' repair logic"
---
```r
repbox.do.table = function(s=NULL,txt=s$newtxt, ph.df = s$ph.df) {
  restore.point("repa.do.table")

  #orgline.marker = ifelse(is.na(s$orglines),"",paste0("#~oline",s$orglines,"~#"))
  orgline.marker = ifelse(is.na(s$orglines),"",paste0("#~oline",s$orglines,"-", s$end.orglines, "~#"))
  newtxt = paste0(orgline.marker,s$newtxt)
  txt = merge.lines(newtxt)

  # Remove comment placeholders
  co.ph.df = ph.df %>%
    filter(startsWith(ph,"#~c")) %>%
    mutate(content = "")
  txt = replace.placeholders(txt, co.ph.df)

  # Set brackets () into ph
  pho = try(blocks.to.placeholder(txt, start=c("("), end=c(")"), ph.prefix = "#~br"))
  if (is(pho,"try-error")) {
    pho = stepwise.blocks.to.placeholder(txt, ph.df,ph.prefix = "#~br")
  }
  txt = pho$str; br.ph.df = pho$ph.df
  if (any(duplicated(br.ph.df$ph))) {
    stop("Parsing error bracket place holders are duplicated. Need to correct placeholder block code.")
  }


  # Find Mata blocks and replace with placeholder
  mata_pos = locate_mata_blocks(txt)
  if (NROW(mata_pos)>0) {
    pho = pos.to.placeholder(txt, mata_pos,ph.prefix = "#~mata_pa ", ph.df=ph.df)
    txt = pho$str; ph.df = pho$ph.df
  }


  #cat(txt)
  txt = sep.lines(txt)
  has.orgline = startsWith(txt,"#~oline")
  orgline_txt = ifelse(has.orgline, str.between(txt,"#~oline","~#"),"")
  orgline_start = ifelse(has.orgline,as.integer(str.left.of(orgline_txt,"-")),NA_integer_)
  orgline_end = ifelse(has.orgline,as.integer(str.right.of(orgline_txt,"-")),NA_integer_)

  #orgline = ifelse(has.orgline, str.between(txt,"#~oline","~#") %>% as.integer(),NA_integer_)
  txt[has.orgline] = str.right.of(txt[has.orgline],"~#")

  str = txt

  # Replace tabs with spaces
  # Otherwise we wont correctly store the cmd
  # variable
  str = gsub("\t"," ", str, fixed=TRUE)

  # STRIP LEADING WHITESPACE SO THAT startsWith() MATCHES WORK CORRECTLY
  str = trimws(str)

  saving = str.right.of(str,"saving#~br",not.found = NA)
  srows = which(!is.na(saving))
  if (length(srows)>0) {
    saving[srows] = str.left.of(saving[srows],"~#")
    saving[srows] = paste0("#~br", saving[srows],"~#")
  }

  quietly = rep(NA_character_, length(str))
  capture = rep(NA_character_, length(str))
  noisily = rep(NA_character_, length(str))

  changed = TRUE
  while(changed) {
    changed = FALSE

    rows = startsWith(str, "quietly:")
    if (any(rows)) { quietly[rows] = "quietly:"; str[rows] = trimws(str.right.of(str[rows], "quietly:")); changed = TRUE }
    rows = startsWith(str, "quietly ")
    if (any(rows)) { quietly[rows] = "quietly "; str[rows] = trimws(str.right.of(str[rows], "quietly ")); changed = TRUE }
    rows = startsWith(str, "qui:")
    if (any(rows)) { quietly[rows] = "qui:"; str[rows] = trimws(str.right.of(str[rows], "qui:")); changed = TRUE }
    rows = startsWith(str, "qui ")
    if (any(rows)) { quietly[rows] = "qui "; str[rows] = trimws(str.right.of(str[rows], "qui ")); changed = TRUE }

    rows = startsWith(str, "capture:")
    if (any(rows)) { capture[rows] = "capture:"; str[rows] = trimws(str.right.of(str[rows], "capture:")); changed = TRUE }
    rows = startsWith(str, "capture ")
    if (any(rows)) { capture[rows] = "capture "; str[rows] = trimws(str.right.of(str[rows], "capture ")); changed = TRUE }
    rows = startsWith(str, "cap:")
    if (any(rows)) { capture[rows] = "cap:"; str[rows] = trimws(str.right.of(str[rows], "cap:")); changed = TRUE }
    rows = startsWith(str, "cap ")
    if (any(rows)) { capture[rows] = "cap "; str[rows] = trimws(str.right.of(str[rows], "cap ")); changed = TRUE }

    rows = startsWith(str, "noisily:")
    if (any(rows)) { noisily[rows] = "noisily:"; str[rows] = trimws(str.right.of(str[rows], "noisily:")); changed = TRUE }
    rows = startsWith(str, "noisily ")
    if (any(rows)) { noisily[rows] = "noisily "; str[rows] = trimws(str.right.of(str[rows], "noisily ")); changed = TRUE }
    rows = startsWith(str, "noi:")
    if (any(rows)) { noisily[rows] = "noi:"; str[rows] = trimws(str.right.of(str[rows], "noi:")); changed = TRUE }
    rows = startsWith(str, "noi ")
    if (any(rows)) { noisily[rows] = "noi "; str[rows] = trimws(str.right.of(str[rows], "noi ")); changed = TRUE }
  }


  # change :\ ad :/ as this is part of file path

  str =gsub(":\\","~;~\\", str, fixed=TRUE)
  str =gsub(":/","~;~\\", str, fixed=TRUE)

  str = trimws(str)
  opens_block = endsWith(str, "{")
  closes_block = str == "}"

  colon1 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":")
  colon2 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":")
  colon3 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":") %>% trimws()
  str = gsub("~;~\\",":\\", str, fixed=TRUE)

  # Some commands use : in a different way. Then don't store colon stuff
  no.colon = which(startsWith(txt, "merge"))
  if (length(no.colon) > 0) {
    colon1[no.colon] = colon2[no.colon] = colon3[no.colon] = NA
    str[no.colon] = txt[no.colon]
  }

  str = gsub(","," ,", str, fixed=TRUE)
  cmd = str.left.of(str," ")
  str = paste0(" ",str.right.of(str," "))
  cmd_br = str.right.of(cmd,"#~br",not.found=NA)
  cmd_br = ifelse(is.na(cmd_br),NA,paste0("#~br",cmd_br))
  cmd = str.left.of(cmd, "#")
  cmd = str.left.of(cmd,"{")

  opts = str.right.of(str,",",not.found=NA) %>% trimws()
  str = str.left.of(str,",")

  # Extracting weight variables [myweight] got more complicated:
  # if conditions can also contain [] like if id=id[_n-1]
  # we thus suppose that a weight string must have a space before
  # need to check whether that is indeed always the case
  weight = rep("", length(str))
  weight_start = stri_locate_first_regex(str,"(?<![a-z0-9A-Z_])\\[")[,1]
  wrows = which(!is.na(weight_start))
  if (length(wrows)>0) {
    weight_start = weight_start[wrows]
    rstr = substring(str[wrows], weight_start)
    weight_end = stri_locate_first_regex(rstr,"\\](?![a-z0-9A-Z_])")[,1]
    #weight_end = stri_locate_first_fixed(rstr, "]")[,1]
    use_wrows = !is.na(weight_end)
    weight[wrows[use_wrows]] = stri_sub(rstr[use_wrows],2,weight_end[use_wrows]-1)
    str[wrows[use_wrows]] = stri_sub(str[wrows[use_wrows]], weight_start[use_wrows])
  }
  #weight = str.between(str,"[","]", not.found=NA) %>% trimws()
  #str = str.left.of(str,"[")


  # Default order is if, in, using
  # but sometimes different order is used like using, if

  res = extract.if.in.using(str)
  str = trimws(res$str)
  using = res$parts$using
  in_arg = res$parts[["in"]]
  if_arg = res$parts[["if"]]




  exp = str.right.of(str,"=",not.found=NA_character_)  %>% trimws()
  str = str.left.of(str,"=")
  arg_str = str

  cmd2 = str.left.of(trimws(arg_str)," ", not.found=NA_character_) %>% trimws()
  cmd2[startsWith(cmd2,"#~")] = NA_character_



  program = ifelse(startsWith(txt, "program define "), str.between(txt,"program define ", " "), NA)

  txt = replace.ph.keep.lines(txt, br.ph.df)
  arg_str = replace.ph.keep.lines(arg_str, br.ph.df)
  exp = replace.ph.keep.lines(exp, br.ph.df)
  cmd_br = replace.ph.keep.lines(cmd_br, br.ph.df)
  opts = replace.ph.keep.lines(opts, br.ph.df)

  na.rows = which(is.na(saving))
  saving = replace.ph.keep.lines(saving, br.ph.df)
  saving[na.rows] = NA_character_


  tab = data.frame(cmd,cmd_br=cmd_br,arg_str, exp, if_arg, in_arg, using, opts, cmd2, saving, txt, colon1, colon2,colon3, program, opens_block, closes_block, quietly, capture, noisily, orgline=orgline_start, orgline_start=orgline_start, orgline_end=orgline_end)
  tab = filter(tab, nchar(trimws(tab$txt))>0)

  # In do files with #delimit ; commands not always a unique
  # orgline is determined. We want to set that line to orgline
  # in which the cmd starts
  rows = which(tab$orgline_start != tab$orgline_end)
  if (length(rows)>0) {
    # remove first line which is empty and not accounted
    # for in orgline
    org_txt = sep.lines(s$txt)[-1]
    for (r in rows) {
      cmd = tab$cmd[r]
      if (is.na(cmd) || isTRUE(cmd=="")) next
      olines = tab$orgline_start[r]:tab$orgline_end[r]
      # prefer later lines: idea is that more likely
      # a comment before the line contains the command
      # than a comment below the line
      points = startsWith(org_txt[olines],cmd) + has.substr(org_txt[olines],cmd) + olines*1e-6
      tab$orgline[r] = olines[which.max(points)]
    }
  }




  # Special treatment for outdated 'for any' command. Like
  # for any y1 y2: reg X z1
  # We will say that cmd="for" because we cannot handle for any
  # when analysing regressions

  # Special treatment for outdated "for any" command in combi with regression. Like
  # for any y1 y2: reg X z1
  # We cant well handle those regressions and thus change the command name
  rows = which(startsWith(tab$colon1,"for any"))
  if (length(rows)>0) {
    regcmds = c(get.regcmds(), stata_cmds_nomrbreg())
    rows = which(startsWith(tab$colon1,"for any") & tab$cmd %in% regcmds)
    tab$cmd[rows] = paste0("__for_any_", tab$cmd[rows])
  }


  tab = tab.repair.colon.local(tab)
  tab = tab.repair.input.cmds(tab)
  tab = tab.replace.texdoc.do(tab)
  tab = tab.add.block.end(tab)
  tab = tab.repair.stata.version(tab)

  tab$line = seq_len(NROW(tab))
  if (any(is.na(tab$orgline))) {
    stop("Parsing of orgline was not correct. As tab$orgline has NA. Pleas debug parsing code.")
  }

  tab = tab.add.in.program(tab)
  tab  = tab.add.in.loop(tab)
  list(tab=tab, ph.df = ph.df)
}
```
!END_MODIFICATION repbox.do.table repboxStata/R/parse.R

!MODIFICATION stata.inject.and.run repboxStata/R/parse.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/parse.R"
function_name = "stata.inject.and.run"
description = "Include no_mrb_reg commands in default arguments for stata.inject.and.run"
---
```r
stata.inject.and.run = function(do, reg.cmds = c(get.regcmds(), stata_cmds_nomrbreg()), save.changed.data=1, opts=rbs.opts(), start.time = NULL) {
  restore.point("stata.inject.and.run")
  #stop()
  # Check if global project timeout is reached
  if (!is.null(start.time)) {

    runsec = as.numeric(Sys.time())-as.numeric(start.time)
    if (isTRUE(runsec > opts$all.do.timeout)) {
      do$timeout = TRUE
      do$runtime = NA

      msg = paste0("Global timeout (duration: ", opts$all.do.timeout, " sec.) reached. Skipping do file: ", do$dofile, ".")
      repboxUtils::repbox_problem(msg = msg, type = "stata_reproduction_global_timeout", project_dir = do$project_dir, fail_action = "msg")

      return(do)
    }
  }

  res = inject.do(do, reg.cmds=reg.cmds, save.changed.data=save.changed.data)
  do = res$do
  do = run.do(do)
  do
}
```
!END_MODIFICATION stata.inject.and.run repboxStata/R/parse.R

!MODIFICATION inject.do repboxStata/R/inject.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/inject.R"
function_name = "inject.do"
description = "Include no_mrb_reg commands in reg.cmds default argument for inject.do"
---
```r
inject.do = function(do, reg.cmds = c(get.regcmds(), stata_cmds_nomrbreg()), save.changed.data=1, opts=rbs.opts()) {
  restore.point("inject.do")

  project_dir=do$project_dir
  id = tools::file_path_sans_ext(basename(do$file))
  repbox.dir = file.path(project_dir,"repbox/stata")

  tab = do$tab[[1]]
  tab$org_cmd = ""


  do$tab[[1]]$add.path.correction = tab$cmd %in% c("use","u","us","saveold", "save","sa","sav", "import","export","mkdir","erase","rm","guse","gsave","gzuse","gzsave") |
    !is.na(tab$using) |
    !is.na(tab$saving) |
    (tab$cmd %in% c("graph","gr","gra") & tab$cmd2 %in% c("export","save")) |
    (tab$cmd %in% c("estimates","est","estim","estimate") & tab$cmd2 %in% c("save","use")) |
    (tab$cmd %in% "adopath" & tab$cmd2 %in% c("+")) |
    (tab$cmd %in% c("putexcel") & tab$cmd2 %in% c("set")) |
    (tab$cmd == "cd" & trimws(tab$txt)!="cd")

  ph = do$ph[[1]]
  tab = do$tab[[1]]

  tab$run.max = NA_integer_
  if (!is.null(opts$loop.log.cmd.max)) {
    rows = tab$in.program == 1 | tab$in_loop == 1
    tab$run.max[rows] = opts$loop.log.cmd.max
  }

  if (!is.null(opts$loop.log.reg.max)) {
    reg_rows = which(tab$is.regcmd & (tab$in.program == 1 | tab$in_loop == 1))
    if (length(reg_rows) > 0) {
      tab$run.max[reg_rows] = opts$loop.log.reg.max
    }
  }
  do$tab[[1]] = tab


  tab$commented.out = FALSE
  tab$add.capture=FALSE

  org.txt = txt = replace.ph.keep.lines(tab$txt,ph)

  ignore_lines = identify_ignore_lines(tab)
  if (length(ignore_lines) > 0) {
    txt[ignore_lines] = paste0("repbox_ignore: ", txt[ignore_lines])
  }

  new.txt = txt

  block.rows = tab$opens_block & (!(is.na(tab$quietly)) | !is.na(tab$capture))
  if (sum(block.rows) >0) {
    cmds = tab$cmd[block.rows]
    rows = !is.na(tab$capture[block.rows])
    cmds[rows] = trimws(tab$capture[block.rows][rows])

    rows = !is.na(tab$quietly[block.rows])
    cmds[rows] = trimws(tab$quietly[block.rows][rows])

    new.txt[block.rows] = stringi::stri_replace_first(new.txt[block.rows],fixed=cmds, replacement = paste0(cmds, " noisily"))
  }

  rows = startsWith(trimws(new.txt),"quietly:") & !block.rows
  new.txt[rows] = str.right.of(new.txt[rows], "quietly:") %>% trimws()

  rows = startsWith(trimws(new.txt),"quietly ") & !block.rows
  new.txt[rows] = str.right.of(new.txt[rows], "quietly ") %>% trimws()
  rows = startsWith(trimws(new.txt),"qui ") & !block.rows
  new.txt[rows] = str.right.of(new.txt[rows], "qui ") %>% trimws()

  rows = which(tab$cmd == "table")
  if (length(rows)>0) {
    is.pre.table = is.pre.Stata17.table.command(tab$txt[rows])
    if (is.pre.table) {
      new.txt[rows] = paste0("version 16: ", new.txt[rows])
    }
  }

  lines = which(tab$add.path.correction)
  new.txt[lines] = inject.path.correction.change.cmd(new.txt[lines], lines, do=do)

  lines = which(!(
    is.true(tab$opens_block) | tab$in.program >= 2 |
      tab$cmd %in% c("}","foreach","forvalues","forval", "if","else","end", "while")
  ))

  new.txt[lines] = paste0("capture:  noisily: ",  new.txt[lines])
  tab$add.capture[lines] = TRUE

  do$tab[[1]] = tab

  ends_with_rbrace_not_macro = endsWith(trimws(tab$txt), "}") & !grepl("\\$\\{[^}]+\\}$", trimws(tab$txt))
  no.study.lines = which( (trimws(tab$cmd) %in% c("}","end","if","else")) | tab$in.program >= 2 | ends_with_rbrace_not_macro )

  no.study.lines = union(no.study.lines,
    which(
      (tab$opens_block & tab$in.program == 1) |
      (tab$opens_block & lag(tab$in_loop %in% c(1,2)))
    )
  )

  if (!opts$report.inside.program) {
    no.study.lines = union(no.study.lines, which(tab$in.program == 1))
  }

  # Temporarily treat ignored commands as un-studiable to prevent extraction injections
  if (length(ignore_lines) > 0) {
    no.study.lines = union(no.study.lines, ignore_lines)
  }

  special.lines = NULL

  if (do$does.include & do$use.includes) {
    incl.df = do$incl.df[[1]]
    incl.df = adapt.incl.df.for.stata.vars(incl.df,do$project_dir)

    incl.do.df = filter(incl.df, cmd=="do" | cmd == "run")

    lines = incl.do.df$line

    new.txt[lines] = paste0(
      '\ndisplay "#~# START INCLUDE INJECTION ',do$donum,"_", lines,
      incl.do.df$find.file.code,
      '\ncapture: noisily: do "',incl.do.df$repbox.file,'", nostop',
      '\ndisplay "#~# END INCLUDE INJECTION ',do$donum,"_", lines
    )

    incl.do.df = filter(incl.df, cmd=="include")
    lines = incl.do.df$line
    new.txt[lines] = paste0(
      '\ndisplay "#~# START INCLUDE INJECTION ',do$donum,"_", lines,
      '\ninclude "',incl.do.df$repbox.file,'"',
      '\ndisplay "#~# END INCLUDE INJECTION ',do$donum,"_", lines
    )
  }

  before.inject.txt = new.txt

  lines = which(tab$cmd == "clear" & startsWith(trimws(tab$arg_str), "all"))
  if (length(lines)>0) {
    cat(paste0("\nReplace ", length(lines)," 'clear all' command in ", do$dofile," with 'clear' to prevent loss of repbox global variables.\n"))
    new.txt[lines] = stringi::stri_replace_first_regex(new.txt[lines], "\\ball\\b", "")
    #new.txt[lines] = "clear"
  }


  lines = setdiff(
    which(tab$cmd %in% c("save", "sa", "sav", "saveold", "gsave", "gzsave","erase","rm")),
    no.study.lines
  )
  new.txt[lines] = paste0(
    inject.intermediate.data.pre(lines, do, opts),
    new.txt[lines]
  )

  lines = setdiff(
    which(tab$cmd %in% c("erase","rm")),
    no.study.lines
  )
  new.txt[lines] = paste0(
    inject.intermediate.data.pre(lines, do, opts),
    new.txt[lines]
  )

  lines = setdiff(which(tab$cmd %in% c("use","u","us", "save","sa", "sav", "saveold", "clear","import","guse","gsave","gzuse","gzsave","rm","erase")), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.use.etc(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  # CACHE INJECTIONS
  cache_cmds = repbox_always_cache_cmd()
  lines = setdiff(which(tab$cmd %in% cache_cmds), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.cache_always(txt[lines], lines, do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt)


  lines = setdiff(which(tab$cmd %in% c("preserve","restore")), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.preserve.restore(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt)

  lines = setdiff(which(tab$cmd %in% c("esttab") & !is.na(tab$using)), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.esttab.etc(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  lines = setdiff(which(tab$in_loop ==2), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.loop(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  gcmds = get.graphcmds()
  ngcmds = get.nographcmds()
  lines = setdiff(which(tab$cmd %in% gcmds & !(tab$cmd %in% ngcmds$cmd & tab$cmd2 %in% ngcmds)), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.graph.save(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  if (opts$report.inside.program) {
    lines = which(tab$cmd == "program")
    new.txt[lines] = paste0(new.txt[lines],'\ndisplay "!.REPBOX.CUSTOM.PROGRAM>*"')
  }

  if (opts$extract.reg.info) {
    if (!require(repboxStataReg)) {
      cat("\nInjection of specific regression information is planned for a new package repboxReg. That package does not yet exist.\n")
      opts$extract.reg.info = FALSE
    }
  }

  if (opts$extract.reg.info) {
    lines = reg.rows = setdiff(which(tab$cmd %in% reg.cmds), no.study.lines)
    special.lines = c(special.lines, lines)
    inj.txt = injection.reg(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  } else {
    lines = reg.rows = setdiff(which(tab$cmd %in% reg.cmds), no.study.lines)
    special.lines = c(special.lines, lines)
    inj.txt = injection.reg.simple(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  }

  quasi_cmds = stata_cmds_quasireg()
  lines = quasi.rows = setdiff(which(tab$cmd %in% quasi_cmds), c(no.study.lines, special.lines))
  if (length(lines) > 0) {
    special.lines = c(special.lines, lines)
    inj.txt = injection.reg.simple(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  }


  if (isTRUE(opts$extract.scalar.vals)) {
    lines = reg.rows = setdiff(which(tab$cmd %in% "scalar"), no.study.lines)
    special.lines = c(special.lines, lines)
    inj.txt = injection.scalar(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  }

  lines = which(startsWith(new.txt, "set maxvar"))
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  lines = which(tab$cmd %in% c("br","browse", "pause","cls","stop") | (tab$cmd == "set" & is.true(startsWith(tab$cmd2,"trace"))))
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  lines = which(tab$cmd %in% c("log","translate") )
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  if (isTRUE(opts$comment.out.install)) {
    lines = which(has.substr(new.txt, "ssc ") & has.substr(new.txt, " install "))
    no.study.lines = c(no.study.lines, lines)
    new.txt[lines] = paste0("*", new.txt[lines])
    tab$commented.out[lines] = TRUE

    lines = which(has.substr(new.txt, "sysdir ") & has.substr(new.txt, " set "))
    no.study.lines = c(no.study.lines, lines)
    new.txt[lines] = paste0("*", new.txt[lines])
    tab$commented.out[lines] = TRUE

  }

  if (!do$use.includes) {
    lines = which(tab$cmd %in% c("do","include","run"))
    no.study.lines = c(no.study.lines, lines)
    new.txt[lines] = paste0("*", new.txt[lines])
    tab$commented.out[lines] = TRUE
  }

  # Remove ignore commands from no.study.lines so they are logged explicitly as 'repbox_ignore:'
  if (length(ignore_lines) > 0) {
    no.study.lines = setdiff(no.study.lines, ignore_lines)
  }

  lines = setdiff(seq_len(NROW(tab)), c(special.lines, no.study.lines))
  inj.txt = injection.other(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt)

  lines = setdiff(which(!tab$commented.out), no.study.lines)
  inj.txt = pre.injection(txt[lines],lines,do)
  new.txt[lines] = paste0(inj.txt,new.txt[lines])

  lines = setdiff(which(!is.na(tab$run.max)), no.study.lines)
  new.txt[lines] = inject.loop.max.run(new.txt[lines], before.inject.txt[lines], lines, do)

  tab$new.txt = new.txt

  org.file = do$file
  do.dir = dirname(org.file)
  org.base = basename(org.file)
  new.base = paste0("repbox_", org.base)
  new.file = file.path(do.dir, new.base)

  log.file = normalizePath(file.path(repbox.dir,"logs", paste0("log_", do$donum,".log")), mustWork=FALSE,winslash = "/")

  incl.log.file = normalizePath(file.path(repbox.dir,"logs", paste0("include_", do$donum,".log")), mustWork=FALSE, winslash = "/")

  log.name = paste0("repbox_log_", do$donum)

  start.timer.file = paste0(project_dir,"/repbox/stata/timer/start.txt")
  end.timer.file = paste0(project_dir,"/repbox/stata/timer/end.txt")

  txt = c(paste0('
file open repbox_timer_file using "', start.timer.file,'", write append
file write repbox_timer_file "', do$donum,';`c(current_time)\';`c(current_date)\'"
file write repbox_timer_file _n
file close repbox_timer_file

if "$repbox_cmd_count" == "" {
  set_defaults _all
  set more off
  global repbox_cmd_count = 0
  global repbox_root_donum = ', do$donum,'
  log using \"',log.file,'\", replace name(',log.name,')
  ', adopath.injection.code(project_dir),'
}
else {
  log using \"',incl.log.file,'\", replace name(',log.name,')
}
'),
          new.txt,
          paste0('
display "#~# FINISHED DO",
capture log close ', log.name,'

file open repbox_timer_file using "', end.timer.file,'", write append
file write repbox_timer_file "', do$donum,';`c(current_time)\';`c(current_date)\'"
file write repbox_timer_file _n
file close repbox_timer_file
'
          ))

  writeLines(txt, new.file)
  return(list(do=do,txt=invisible(txt)))
}
```
!END_MODIFICATION inject.do repboxStata/R/inject.R

!MODIFICATION drf_stata_cmd_types repboxDRF/R/cmd_types.R
scope = "function"
file = "/home/rstudio/repbox/repboxDRF/R/cmd_types.R"
function_name = "drf_stata_cmd_types"
description = "Include no_mrb_reg from repboxStata in drf_stata_cmd_types"
---
```r
drf_stata_cmd_types = function() {
  #
  # IMPORTANT: IF YOU ADD A COMMAND HERE
  # ALSO ADD IT IN stata.data.cmd.types.vec
  # Otherwise strange errors occur
  #
  list(
    load = c("u","us", "use","insheet","infix","import","sysuse","guse","gzuse"),
    preserve = c("preserve"),
    restore = c("restore"),
    # Commands that modify data and can be translated to R
    mod = c("g","ge","gen","gene","gener","genera","generat", "generate","replace","clonevar", "drop","keep","rename","ren","rena","renam","merge","egen","xtset","tsset","iis", "xtile","pctile","append","tabulate","tabul","tabu", "tab","ta","encode","predict","xi", "collapse", "sort","so","sor", "tab1", "winsor", "tostring", "destring", "recode", "expand", "reshape", "contract", "carryforward", "cem","char", "dfbeta", "rowranks",

          # additional commands proposed by ChatGPT
"input","decode", "mvencode", "mvdecode", "split",
"separate", "ipolate","range", "insobs", "compress", "recast",
"joinby", "cross", "stack", "xpose", "fillin", "expandcl",
"sample", "bsample", "splitsample", "drawnorm", "statsby",
"rolling","gsort", "order", "aorder", "label", "notes",
"duplicates", "describe","stsplit", "stjoin", "svyset", "mi",
"frame", "frames","predictnl","winsor2", "ereplace",
"fcollapse", "fmerge",
"gcollapse", "gegen", "gcontract", "greshape", "gduplicates",
"gstats", "gquantiles", "hashsort",
"rangestat", "rangejoin", "asrol", "asgen", "astile", "fastxtile",
"renvars", "labmask", "labgen", "labrecode", "sxpose",

"zscore", "zscore06",
"hprescott"


      ),
    #scalar = c("scalar"),

    # Commands that modify data but cannot be translated to R
    # predict also needs cache because it may use
    # results from previous regression
    #need_cache = c("merge","xtile","pctile","append","tabulate","tabul","tabu", "tab","ta","encode","predict","xi","collapse","sort","so","tab1", "winsor"),

    # Commands that under certain conditions modify the data
    # tabulate creates vpidiables if called with the gen option
    possible_mod = c("tabulate","tabul","tabu", "tab","ta","tab1"),
    xtset = c("xtset","tsset","iis", "stset","tis"),
    reg = repboxStata::stata_cmds_reg(),
    quasi_reg = repboxStata::stata_cmds_quasireg(),
    post_reg = repboxStata::stata_cmds_postreg(),
    no_mrb_reg = repboxStata::stata_cmds_nomrbreg()
  )
}
```
!END_MODIFICATION drf_stata_cmd_types repboxDRF/R/cmd_types.R

!MODIFICATION drf_stata_cmd_types_vec repboxDRF/R/cmd_types.R
scope = "function"
file = "/home/rstudio/repbox/repboxDRF/R/cmd_types.R"
function_name = "drf_stata_cmd_types_vec"
description = "Update drf_stata_cmd_types_vec to map reg3, sem, gsem to no_mrb_reg"
---
```r
drf_stata_cmd_types_vec = function() {
  return(c(
u="load", us="load", use="load", insheet="load", infix="load", import="load", sysuse="load", guse="load", gzuse="load", preserve="preserve", restore="restore", g="mod", ge="mod", gen="mod", gene="mod", gener="mod",genera="mod",generat="mod", generate="mod", clonevar="mod", replace="mod", char="mod", drop="mod", keep="mod", rename="mod",ren="mod", rena="mod",renam="mod", merge="mod", egen="mod", xtset="mod", tsset="mod", xtile="mod", pctile="mod", append="mod", tabulate="mod", tabul="mod", tabu="mod", tab="mod", ta="mod", encode="mod", predict="mod", xi="mod", collapse="mod", sort="mod",sor="mod", so="mod", tab1="mod", winsor="mod", tostring="mod", destring="mod", recode="mod", expand="mod", reshape="mod", contract="mod", carryforward="mod", rowranks="mod", cem="mod", tabulate="mod", tabul="mod", tabu="mod", tab="mod", ta="mod", tab1="mod",

    # additional commands proposed by ChatGPT
input="mod", decode="mod", mvencode="mod", mvdecode="mod", split="mod", separate="mod", ipolate="mod", range="mod", insobs="mod", compress="mod", recast="mod", joinby="mod", cross="mod", stack="mod", xpose="mod", fillin="mod", expandcl="mod", sample="mod", bsample="mod", splitsample="mod", drawnorm="mod", statsby="mod", rolling="mod", gsort="mod", order="mod", aorder="mod", label="mod", notes="mod", duplicates="mod", describe="mod", stsplit="mod", stjoin="mod", svyset="mod", mi="mod", frame="mod", frames="mod", predictnl="mod", winsor2="mod", ereplace="mod", fcollapse="mod", fmerge="mod", gcollapse="mod", gegen="mod", gcontract="mod", greshape="mod", gduplicates="mod", gstats="mod", gquantiles="mod", hashsort="mod", rangestat="mod", rangejoin="mod", asrol="mod", asgen="mod", astile="mod", fastxtile="mod", renvars="mod", labmask="mod", labgen="mod", labrecode="mod", sxpose="mod",

    zscore = "mod", zscore06 = "mod",
    hprescott = "mod",


    reg="reg", areg="reg", ivregress="reg", ivreg="reg", ivreg2="reg", sureg="reg", reghdfe="reg", reg2hdfe="reg", xtreg="reg", xtivreg2="reg", xtlogit="reg", xtprobit="reg", xttobit="reg", regress="reg", cgmreg="reg", intreg="reg", boxcox="reg", qreg="reg", truncreg="reg", cnsreg="reg", eivreg="reg", nl="reg", rreg="reg", bsqreg="reg", sqreg="reg", iqreg="reg", vwls="reg",  glm="reg", cloglog="reg", logit="reg", logistic="reg", blogit="reg", glogit="reg", binreg="reg", scobit="reg", probit="reg", dprobit="reg", ivprobit="reg", bprobit="reg", gprobit="reg", hetprobit="reg", heckprobit="reg", biprobit="reg", tobit="reg", ivtobit="reg", clogit="reg", oprobit="reg", ologit="reg", heckoprobit="reg", rologit="reg", asroprobit="reg", slogit="reg", mlogit="reg", asclogit="reg", nlogit="reg", asmprobit="reg", mprobit="reg", poisson="reg", ivpoisson="reg", nbreg="reg", gnbreg="reg", tpoisson="reg", tnbreg="reg", zip="reg", zinb="reg", exlogistic="reg", expoisson="reg", arch="reg", frontier="reg", heckman="reg", etregress="reg", etpoisson="reg", arima="reg", arfima="reg", newey="reg", var="reg", svar="reg", vec="reg", dfactor="reg", ppmlhdfe="reg", svyreg="reg", ppml="reg",
  givregress="reg",ivfprobit="reg",ivqregress="reg",spivregress="reg",


    rd="quasi_reg", rdrobust="quasi_reg", sem="no_mrb_reg", psmatch2="quasi_reg", leebounds="quasi_reg", a2reg="quasi_reg", xtabond2="quasi_reg", reg3="no_mrb_reg", altrdrobust="quasi_reg", hausman="quasi_reg", gsem="no_mrb_reg", gmm="quasi_reg", stcox="reg", xtivreg="reg", ivreghdfe="reg", condivreg="reg", xtpoisson="reg", newey2="reg", hetprob="quasi_reg", reg2hdfespatial="quasi_reg", outreg2="post_reg", estadd="post_reg", estimates="post_reg", predict="mod", dfbeta="mod", test="post_reg", matrix="post_reg", est="post_reg", outreg="post_reg", eststo="post_reg", testparm="post_reg", lincom="post_reg", esttab="post_reg", margins="post_reg", estout="post_reg", estat="post_reg", nlcom="post_reg", plotcoeffs="post_reg", mfx="post_reg", boottest="post_reg", regsave="post_reg", center_estimates="post_reg", estimate="post_reg", est2vec="post_reg", suest="post_reg", fitstat="post_reg", parmest="post_reg", post="post_reg", estpost="post_reg", savereg="post_reg", wild="post_reg", rivtest="post_reg", sigstar2="post_reg", modl="post_reg", svmat="post_reg", sig_p="post_reg", ttest="post_reg", lincomestadd="post_reg", coefplot="post_reg", eret2="post_reg", get_coef="post_reg", cgmwildboot="post_reg", vareffects="post_reg", bootwildct="post_reg", post_param="post_reg", avplot="post_reg", addtotable="post_reg", margin="post_reg", vce2way="post_reg", save_results="post_reg", ivstack="post_reg", predictnl="post_reg", spatdiag="post_reg", parmby="post_reg", testnl="post_reg", b_xt="post_reg", V_xt="post_reg", p_vals="post_reg", inteff="post_reg", est2tex="post_reg", meff="post_reg", marginsplot="post_reg", iv_stack="post_reg", dfuller="post_reg", ivhettest="post_reg", pValueFormatting="post_reg", estwrite="post_reg", dfbeta="post_reg", margeff="post_reg", modltbl="post_reg", outsheet="post_reg", outtex="post_reg", lincomest="post_reg", mfx2="post_reg", addstars="post_reg",
    xtset="xtset",tsset="xtset",iis="xtset", stset="xtset"
    ))
}
```
!END_MODIFICATION drf_stata_cmd_types_vec repboxDRF/R/cmd_types.R

!MODIFICATION rsr_make_quasi_and_post_reg_parcels repboxStataReg/R/rsr.R
scope = "function"
file = "/home/rstudio/repbox/repboxStataReg/R/rsr.R"
function_name = "rsr_make_quasi_and_post_reg_parcels"
description = "Create and save no_mrb_reg parcel in repboxStataReg"
---
```r
#' Generate quasi_reg and post_reg parcels
rsr_make_quasi_and_post_reg_parcels = function(project_dir, parcels = list(), run_df = NULL) {
  restore.point("rsr_make_quasi_and_post_reg_parcels")

  if (is.null(run_df)) {
    parcels = repboxDB::repdb_load_parcels(project_dir, "stata_run_cmd", parcels = parcels)
    run_df = repboxDRF::repbox_get_run_df(project_dir = project_dir, parcels = parcels)
  }

  if (is.null(run_df) || NROW(run_df) == 0) return(parcels)

  repdb_dir = file.path(project_dir, "repdb")

  parcels = repboxDB::repdb_load_parcels(project_dir, "xtvar", parcels = parcels)
  xtvar = parcels$xtvar

  if (!is.null(xtvar) && nrow(xtvar) > 0) {
    xt_join = xtvar %>% dplyr::select(dplyr::any_of(c("runid", "timevar", "panelvar", "tdelta")))
    run_df = run_df %>%
      dplyr::select(-dplyr::any_of(c("timevar", "panelvar", "tdelta"))) %>%
      dplyr::left_join(xt_join, by = "runid")
  }

  if (!"timevar" %in% names(run_df)) run_df$timevar = NA_character_
  if (!"panelvar" %in% names(run_df)) run_df$panelvar = NA_character_
  if (!"tdelta" %in% names(run_df)) run_df$tdelta = NA_character_

  # 1. quasi_reg parcel
  qrows = which(run_df$cmd_type == "quasi_reg")
  if (length(qrows) > 0) {
    quasi_reg = run_df[qrows, , drop = FALSE] %>%
      dplyr::transmute(
        runid = as.integer(runid),
        variant = "so", # "so" because this is based on the Stata Original run
        cmd = as.character(cmd),
        cmdline = as.character(cmdline),
        flags = "",
        has_err = errcode != 0,
        timevar = as.character(repboxUtils::na.val(timevar, "")),
        panelvar = as.character(repboxUtils::na.val(panelvar, "")),
        tdelta = as.character(repboxUtils::na.val(tdelta, ""))
      )
    parcels[["quasi_reg"]] = quasi_reg
    repboxDB::repdb_save_parcels(parcels["quasi_reg"], dir = repdb_dir, check = FALSE)
  }

  # 2. post_reg parcel
  prows = which(run_df$cmd_type == "post_reg")
  if (length(prows) > 0) {
    map_df = repboxDRF::drf_get_post_reg_reg_runid(run_df)

    post_reg = run_df[prows, , drop = FALSE] %>%
      dplyr::left_join(map_df, by = "runid") %>%
      dplyr::transmute(
        runid = as.integer(runid),
        variant = "so",
        reg_runid = as.integer(reg_runid),
        cmd = as.character(cmd),
        cmdline = as.character(cmdline),
        flags = "",
        has_err = errcode != 0
      )
    parcels[["post_reg"]] = post_reg
    repboxDB::repdb_save_parcels(parcels["post_reg"], dir = repdb_dir, check = FALSE)
  }
  
  # 3. no_mrb_reg parcel
  nrows = which(run_df$cmd_type == "no_mrb_reg")
  if (length(nrows) > 0) {
    no_mrb_reg = run_df[nrows, , drop = FALSE] %>%
      dplyr::transmute(
        runid = as.integer(runid),
        variant = "so",
        cmd = as.character(cmd),
        cmdline = as.character(cmdline),
        flags = "",
        has_err = errcode != 0,
        timevar = as.character(repboxUtils::na.val(timevar, "")),
        panelvar = as.character(repboxUtils::na.val(panelvar, "")),
        tdelta = as.character(repboxUtils::na.val(tdelta, ""))
      )
    parcels[["no_mrb_reg"]] = no_mrb_reg
    repboxDB::repdb_save_parcels(parcels["no_mrb_reg"], dir = repdb_dir, check = FALSE)
  }

  parcels
}
```
!END_MODIFICATION rsr_make_quasi_and_post_reg_parcels repboxStataReg/R/rsr.R
