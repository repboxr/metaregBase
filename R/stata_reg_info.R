# 1. Run parse.stata.regs
# 2. Load reg data set and call stata reg.var.info


# Characterize stata regression variables given a data set

example.vi.from.stata.reg = function() {
  library(repboxMain)
  library(repboxReg)

  project = "testsupp"
  project_dir = file.path("~/repbox/projects_reg",project)
  run.project.with.reg(project_dir)

  reg.df = readRDS(file.path(project_dir, "repbox/stata/regtab.Rds"))
  reg.df$creg.num = seq_len(NROW(reg.df))
  reg.df = stata.regs.add.parsing.info(reg.df)
  reg.df$depvar
  reg = reg.df[3,]
  dat = load.reg.dta(project_dir, reg)

  #reg$cols_info = list(make_cols_info(dat))
  #cols_info = list(make_cols_info(dat))

  vi = vi.from.stata.reg(reg, dat)
  reg$vi = list(vi)


  reg$depvar


  rstudioapi::filesPaneNavigate(paste0(project_dir,"/repbox"))
  rstudioapi::filesPaneNavigate("~/repbox/repboxReg/R")
  rstudioapi::filesPaneNavigate("~/repbox/repboxStata/R")

}

stata.reg.se.info = function(reg) {
  restore.point("stata.reg.se.info")

  opts.df = reg$opts.df[[1]]
  rhs.form =function(vars) {
    as.formula(paste0("~", paste0(vars, collapse=" + ")))
  }

  if (reg$cmd == "newey") {
    restore.point("stata.reg.se.info.newey")
    row = opts.df$opt == "lag"
    lag = as_integer(opts.df$opt_args[row])


    se.info = tibble(
      se_category = "robust",
      se_type = "nw",
      se_param = paste0("lag=",lag),
      iid_se = FALSE,
      stata_se = "newey",
      clustervar = list(NULL),
      sandwich_fun = "NeweyWest",
      sandwich_opts = list(list(lag=lag, prewhite=FALSE, adjust=TRUE)),
      #sandwich_opts = list(list(lag=lag, prewhite=FALSE, adjust=TRUE, order.by = as.formula(paste0("~", reg$timevar))))
    )
    return(se.info)

  }

  # There are different ways how standard errors can be
  # specified in Stata. All the following 4 versions
  # specify the same robust standard errors:

  # reg y x, vce(robust)
  # reg y x, vce(r)
  # reg y x, robust
  # reg y x, rob

  # Our code deals with the different version

  abbr.li = list(
    robust = c("robust","robus","robu","rob","ro","r"),
    cluster = c("cluster","cluste","clust","clus","clu","cl"),
    bootstrap = c("bootstrap","bootstra","bootstr","bootst","boots","boot"),
    jackknife = c("jackknife","jackknif","jack")
  )

  se_type = ""
  vce.row = which(opts.df$opt=="vce")
  if (length(vce.row)>0) {
    # using tolower causes errors if variable name for
    # clustered standard errors is not in lower case
    # se_str = tolower(opts.df$opt_args[vce.row])
    se_str = opts.df$opt_args[vce.row]
    se_type = str.left.of(se_str, " ")
    se_type = expand_stata_abbr_one_val(se_type, abbr.li)
    se_args = str.right.of(se_str, " ",not.found = "") %>%
      trimws() %>% ws_to_single_space() %>%
      strsplit(" ")
    se_args = se_args[[1]]
  } else {
    abbr.row = which(opts.df$opt %in% unlist(abbr.li))

    # cluster se may have option: robust cluster(myvar)
    # we then just set option to cluster
    if (length(abbr.row)==2) {
      cl_ind = which(startsWith(opts.df$opt[abbr.row],"cl"))
      if (length(cl_ind)>0) {
        abbr.row = abbr.row[cl_ind]
      }
    }
    if (length(abbr.row)==1) {
      se_type = opts.df$opt[[abbr.row]]
      se_type = expand_stata_abbr_one_val(se_type, abbr.li)
      # using tolower causes errors if variable name for
      # clustered standard errors is not in lower case
      # se_str = tolower(opts.df$opt_args[abbr.row])
      se_str = opts.df$opt_args[abbr.row]
      se_args = se_str %>%
        trimws() %>% ws_to_single_space() %>%
        strsplit(" ")
      se_args = se_args[[1]]
    } else if (length(abbr.row)>1) {
      stop("Regression options match multiple standard error abbreviations. Need to adapt stata.reg.se.info")
    }
  }

  # No specific se option
  if (se_type == "") {
    se.info = tibble(
      se_category = "iid",
      se_type = "iid",
      iid_se = TRUE,
      stata_se = "default",
      clustervar = list(NULL),
      sandwich_fun = "vcovHC",
      sandwich_opts = list(list(type="const"))
    )
    return(se.info)
  }

  if (se_type=="robust") {
    se.info = tibble(
      se_category = "robust",
      se_type = "hc1",
      iid_se = FALSE,
      stata_se = "robust",
      clustervar = list(NULL),
      sandwich_fun = "vcovHC",
      sandwich_opts = list(list(type="HC1")),
      fixest_se = "hetero",
      fixest_opts = list(list()),
      estimtr_se = "stata"
    )
    return(se.info)
  }

  if (se_type=="cluster") {
    se.info = tibble(
      se_category = "cluster",
      se_type = "cluster",
      iid_se = FALSE,
      stata_se = "cluster",
      clustervar = list(se_args),
      sandwich_fun = "vcovCL",
      sandwich_opts = list(list(cluster=rhs.form(se_args))),
      fixest_se = "cluster",
      fixest_opts = list(list(cluster=rhs.form(se_args))),
      estimtr_se = "cluster"
    )
    return(se.info)
  }

  if (tolower(se_type) %in% c("hc0", "hc1","hc2","hc3","hc4","hc5") ) {
    se_type = tolower(se_type)
    se.info = tibble(
      se_category = "robust",
      se_type = se_type,
      iid_se = FALSE,
      stata_se = se_type,
      clustervar = list(NULL),
      sandwich_fun = "vcovHC",
      sandwich_opts = list(list(type=toupper(se_type)))
    )
    if (se_type == "hc1") {
      se.info$fixest_se = "hetero"
      se.info$fixest_opts = list(list())
    }
    if (se_type %in% c("hc0","hc1","hc2","hc3")) {
      se.info$estimatr_se = toupper(se_type)
    }
    return(se.info)
  }

  stop(paste0("Have not yet implemented parsing of Stata standard error of type ", se_type))
  return(NULL)
}

example.expand.stata.var.patterns = function(){
  cols = c("abc","ab1","b2","ba2","d4", "b3", "ab","cb")
  expand.stata.var.patterns(c("abc","a*","b2-b3"),cols)

  expand.stata.var.patterns("*potentialYield", colnames(data))
}

# Expand Stata patterns used as abbreviations in variable lists, like myvar* or var1-var5
expand.stata.var.patterns = function(pattern, cols, unlist=TRUE, uses_xi=FALSE) {
  restore.point("expand.stata.var.patterns")

  if (uses_xi) {
    # Also split |.
    # Note that xi reg y f|x differs from xi reg y f*x
    # since f|x does not generate main effects for f
    # we will currently ignore this. This will be relevant
    # when translating to R.
    if (!is.null(pattern)) {
      pattern = stri_replace_all_fixed(pattern, "|","#")

      # replace i.year*c.d => i.year##c.d
      # but keep ye*r
      #stri_detect_regex(pattern, "(([\\.][a-zA-Z0-9_]+))(\\*)")
      pattern = stri_replace_all_regex(pattern, "(([\\.][a-zA-Z0-9_]+))(\\*)","$1##")

    }
  }
  # If there are interaction terms: split them up
  ia.rows = which(has.substr(pattern,"#"))
  if (length(ia.rows)>0) {
    restore.point("expand.stata.var.patterns2")
    has.double = has.substr(pattern[ia.rows],"##")
    sep = ifelse(has.double,"##","#")
    for (i in seq_along(ia.rows)) {
      row = ia.rows[i]
      parts = strsplit(pattern[row],sep[i],fixed=TRUE)[[1]]
      parts = expand.stata.var.patterns(parts,cols=cols, unlist=TRUE, uses_xi=uses_xi)
      pattern[row] = paste0(parts, collapse=sep)
    }
    not.ia.rows = setdiff(seq_along(pattern),ia.rows)
    if (length(not.ia.rows)>0) {
      pattern[not.ia.rows] = expand.stata.var.patterns(pattern[not.ia.rows], cols=cols, uses_xi=uses_xi,unlist=FALSE)
    }
    if (unlist) return(unlist(pattern))
    return(pattern)
  }

  restore.point("expand.stata.var.patterns3")

  star.rows = which(has.substr(pattern,"*") | has.substr(pattern, "?"))
  minus.rows = which(has.substr(pattern,"-"))

  normal.rows = setdiff(seq_along(pattern), c(star.rows, minus.rows))


  # A Stata pattern like i.x* will be split
  # into lhs=i. and rhs=x*
  pattern.rhs = str.right.of(pattern,".")
  pattern.lhs = str.left.of(pattern,".",not.found = rep("", length(pattern)))
  pattern.lhs = ifelse(pattern.lhs == "","", paste0(pattern.lhs, "."))

  # If a column is uniquely identified Stata can use abbreviations
  # E.g. reg y x  works if only the columns y and x1 exist.
  # Here x is an abbreviation for x
  no.match.rows = normal.rows[which(!(pattern.rhs[normal.rows] %in% cols))]
  if (length(no.match.rows)>0) {
    for (row in no.match.rows) {
      mcols = which(startsWith(cols, pattern.rhs[row]))
      if (length(mcols)>1) {
        cat("\nThe regression variable ", pattern.rhs[row], " could not directly be found in the data set, but as abbreviatoon it matches multiple variables.\n")
        pattern[row] =paste0(pattern.lhs[row] ,cols[mcols[1]])
      } else if (length(mcols)==0) {
        msg = paste0("\nThe regression variable ", pattern.rhs[row], " could not be matched with any variable in the data set.\n")
        repbox_problem(msg,"regvar_no_match")
        stop()
      } else {
        pattern[row] = paste0(pattern.lhs[row],cols[mcols[1]])
      }
    }
  }


  if (length(star.rows)+length(minus.rows)==0) return(pattern)
  vars = as.list(pattern)

  # 1. Replace var* patterns

  rows = star.rows
  if (uses_xi) {
    has.dot.star = pattern.lhs[rows]!="" & has.substr(pattern[rows],"*")
  }
  rx = glob2rx(pattern.rhs[rows])

  i = 1
  for (i in seq_along(rows)) {
    r = rows[i]

    mvars = cols[grepl(rx[i],cols)]
    vars[[r]] = paste0(pattern.lhs[r],mvars)

    # If xi is used an example like i.d2*d1
    # means an interaction term.
    # This interaction term usage seems to be the case
    # if both conditions are satisfied
    # 1. xi is called
    # 2. terms with star also has a dot like i.d2
    # If xi is not used the expression i.d* would use wildcard
    # matching. With xi i.d* would not be a valid pattern in Stata as
    # an interaction term after the * is missing
    if (uses_xi) {
      if (has.dot.star[i]) {
        vars[[r]] = pattern[rows[i]]
      }
    }
  }

  # 2. Replace var1-var5 patterns

  # Stata can have an expression like
  # regress y i.d1-i1
  # It means that the i. will be added to
  # all matched variables

  # Luckily the following is no valid Stata code:
  # regress y i.d1-i.i1

  rows = minus.rows
  for (i in seq_along(rows)) {
    r = rows[i]
    pat = pattern.rhs[r]
    from.var = str.left.of(pat,"-") %>% trimws()
    to.var = str.right.of(pat,"-") %>% trimws()
    range = sort(which(cols %in% c(from.var, to.var)))
    vars[[r]] = paste0(pattern.lhs[r],cols[range[1]:range[2]])
  }

  if (unlist) return(unlist(vars))
  vars
}



# These functions dont require a data set
stata.regs.add.parsing.info = function(reg.df) {
  reg.info = stata.regs.parse(reg.df)
  left_join_overwrite(reg.df, select(reg.info,-cmdline, -cmd), by = "creg.num")
}

stata.reg.parse = function(stata_code, run=NULL, creg.num = 0) {
  stata.regs.parse(cmdlines = stata_code, creg.num = creg.num, timevar=run$timevar, panelvar=run$panelvar, tdelta=run$tdelta)
}

stata.regs.parse = function(reg.df=NULL, cmdlines=reg.df$cmdline, creg.num = reg.df$creg.num, timevar = reg.df$timevar, panelvar=reg.df$panelvar, tdelta = reg.df$tdelta) {
  restore.point("parse.stata.regs")
  str = cmdlines

  str = trimws(str)
  str = remove.start.strings(str,c("quietly:","quiet:","qui:","quietly ","quiet ","qui ", "capture ","capture:","cap ", "cap:","capt ", "capt:"))

  colon1 = str.left.of(str, ":",not.found = rep("", length(str))) %>% trimws()
  str = str.right.of(str, ":") %>% trimws()
  colon2 = str.left.of(str, ":",not.found = rep("", length(str))) %>% trimws()
  str = str.right.of(str, ":") %>% trimws()
  colon3 = str.left.of(str, ":",not.found = rep("", length(str))) %>% trimws()
  str = str.right.of(str, ":") %>% trimws()

  uses_xi = colon1 == "xi" | colon2 == "xi" | colon3 == "xi"

  str = gsub(","," ,", str, fixed=TRUE)

  cmd = str.left.of(str," ") %>% trimws()
  str = str.right.of(str, " ") %>% trimws()

  var.df = parse.stata.reg.vars(cmd, str)
  str = var.df$str

  # We now want to parse the string after the varlist
  # Here is an example of a real complex form

  # reg y x1 if (i==1) | i==2 | inlist(f1, "A" "B") [aw=x1] in 5/25, robust

  # 1. Replace brackets with placeholder
  #    Implicitly assumes that there are no string constants
  #    that contain non-matched brackets
  txt = paste0(str, collapse = "\n")
  pho = try(blocks.to.placeholder(txt, start=c("("), end=c(")"), ph.prefix = "#~br"))
  if (is(pho,"try-error")) {
    pho = stepwise.blocks.to.placeholder(str)
  }

  # In our example we have now
  # str = " if #~br1~# | i==2 | inlist#~br2~# [aw=x1] in 5/25 , robust"
  str = strsplit(pho$str,split = "\n")[[1]]
  ph.df = pho$ph.df
  if (length(str)==0) str = ""

  # Any commas in if condition should now be part of the brackets
  # placeholder, and options start after comma in str
  opts_str = str.right.of(str,",",not.found=NA) %>% trimws()

  str = str.left.of(str,",")

  # Parsing weights is a bit more complex because
  # if conditions can also contain [] like if id=id[_n-1]
  # we thus suppose that a weight string must have a space before
  # need to check whether that is indeed always the case

  weight_left = stri_locate_first_regex(str,"(?<![a-z0-9A-Z_])\\[")[,1]
  weight_right = rep(NA_integer_, length(str))
  wrows = which(!is.na(weight_left))
  if (length(wrows)>0) {
    rstr = stri_sub(str[wrows], weight_left[wrows])
    weight_right[wrows] = stri_locate_first_fixed(rstr, "]")[,1] + weight_left[wrows]-1
  }

  weights_str = substring(str, weight_left+1, weight_right-1)
  # Note: Usually the weight specification should not have any ()
  #       so we don't need to replace placeholders. But better add
  #       a check
  if (isTRUE(any(has.substr(weights_str,"#~br"), na.rm = TRUE))) {
    repbox_problem("We encountered a weight specifications in regression that use parenthesis","weight_with_parenthesis",fail_action = "msg")
    weights_str =  replace.placeholders(weights_str, ph.df)
  }
  weights_type = substring(trimws(weights_str),1,1)
  weights_var = str.right.of(weights_str,"=") %>% trimws()


  rows = !is.na(weights_str)
  str[rows] = str.cutout(str, weight_left, weight_right)


  if_left =  stri_locate_first_fixed(str, " if ")[,1]
  in_left =  stri_locate_first_fixed(str, " in ")[,1]

  if_right = pmin(nchar(str),in_left-1, na.rm=TRUE)
  in_right = pmin(nchar(str),if_left-1, na.rm=TRUE)

  if_str = substring(str, if_left+3, if_right) %>% trimws()
  if_str = replace.placeholders(if_str, ph.df) # replace possible ()

  in_str = substring(str, in_left+3, in_right) %>% trimws()
  in_str =  replace.placeholders(in_str, ph.df) # replace possible ()

  if (is.null(timevar)) timevar = NA
  if (is.null(panelvar)) panelvar = NA
  if (is.null(tdelta)) tdelta = NA

  reg.info = tibble(creg.num = creg.num, cmdline=cmdlines, cmd, depvar=var.df$depvar, xformula=var.df$xformula, exo_parts=var.df$exo_parts, endo_parts=var.df$endo_parts, instr_parts=var.df$instr_parts, weights_type=weights_type, weights_var=weights_var, weights_str, opts_str, if_str, in_str, uses_xi, colon1, colon2, colon3, timevar = timevar, panelvar=panelvar, tdelta=tdelta)

  reg.info = add_stata_reg_opts(reg.info, pho=pho)
  reg.info
}

# This function inverts the parsing functions below
build_stata_formula_from_reg_var_info = function(cmd, rv) {
  if (!is.null(rv$endo_parts)) {
    paste0(c(rv$depvar,rv$exo_parts), collapse=" ")
  } else {
    paste0(c(rv$depvar,rv$exo_parts), collapse=" ")

  }
}

parse.stata.reg.vars = function(cmd, str) {
  restore.point("parse.stata.reg.vars")

  # If there is a formula like
  # regress y x1 - x5
  # We want to change it to
  # regress y x1-x5
  # Otherwise we will have parsing problems
  str = trimws_around(str, "-")


  # Correct rows like
  # reg y x1 (ib1.i1 )#i.year, robust
  # reg y x1 (ib1.i1 )#(i.year), robust
  # reg y x1 ib1.i1##(i.year##i.i2), robust

  # Note that reg y (x) would not be allowed
  # so we can use the # as crucial pattern
  str = stri_replace_all_regex(str,"\\([ ]*([a-zA-Z0-9_.#]+)[ ]*\\)#","$1#")
  str = stri_replace_all_regex(str,"#\\([ ]*([a-zA-Z0-9_.#]+)[ ]*\\)","#$1")



  rem_rows = seq_along(str)

  res1 = res2 = res3 = NULL

  rows = which(cmd %in% c("ivregress"))
  if (length(rows)>0) {
    res1 = parse.stata.reg.vars.iv(cmd[rows], str[rows], rows, has.method=TRUE)
    rem_rows = setdiff(rem_rows, rows)
  }

  rows = which(cmd %in% c("ivreg2","ivreg","xtivreg2","xtivreg"))
  if (length(rows)>0) {
    res2 = parse.stata.reg.vars.iv(cmd[rows], str[rows], rows, has.method=FALSE)
    rem_rows = setdiff(rem_rows, rows)
  }

  rows = which(cmd == "reghdfe")
  if (length(rows)>0) {
    res3 = parse.stata.reg.vars.reghdfe(cmd[rows], str[rows], rows, has.method=FALSE)
    rem_rows = setdiff(rem_rows, rows)
  }

  rows = rem_rows
  res_default = parse.stata.reg.vars.default(cmd[rows], str[rows], rows)

  res = bind_rows(res1,res2,res3, res_default) %>% arrange(row)

}

parse.stata.reg.vars.iv = function(cmd, str, rows, has.method = FALSE) {
  if (length(rows)==0) return(NULL)
  org_str = str
  restore.point("parse.reg.vars.iv")
  if (has.method) {
    method = str.left.of(str," ") %>% trimws()
    str = str.right.of(str, " ") %>% trimws()
  } else {
    method = ""
  }
  depvar = str.left.of(str," ") %>% trimws()

  # We add a space to detect if xformula starts with if or in
  str = str.right.of(str, " ") %>% trimws() %>% paste0(" ",.)
  end.pos = stringi::stri_locate_first_regex(str, "(,)|( if )|(\\[)|( in )|$")[,1]-1

  #str = str.right.of(str, " ") %>% trimws()
  #end.pos = stringi::stri_locate_first_regex(str, "(,)|( if )|(\\[)|( in )|$")[,1]-1

  xformula = substring(str,1, end.pos) %>%
    #stri_replace_all_fixed(" _I"," i.")  %>%
    trimws() %>% shorten.spaces() %>%
    # In Stata we can have something like
    # reg y x1 - x5
    # We want to normalize it to reg y x1-x5
    # so that we can later expand that Stata pattern
    trimws_around("-")


  br.start = stringi::stri_locate_first_fixed(str, "(")[,1]
  br.end = stringi::stri_locate_first_fixed(str, ")")[,1]
  equal.pos = stringi::stri_locate_first_fixed(str, "=")[,1]

  # Default would be
  # ivreg y i2 (x1 = i1)
  #
  # But also this is valid syntax (running OLS)
  # ivreg y x1, robust cluster(i2)
  #
  # We thus check if there is a comma left of the first bracket
  comma_pos = stringi::stri_locate_first_fixed(str, ",")[,1]
  if (isTRUE(comma_pos < br.start)) {
    return(parse.stata.reg.vars.default(cmd,str = org_str,rows = rows))
  }

  endo = substring(str,br.start+1, equal.pos-1) %>% trimws() %>% shorten.spaces() %>% trimws_around("-")
  endo_parts = strsplit(endo," ",fixed = TRUE)

  instr = substring(str,equal.pos+1, br.end-1) %>% trimws() %>% shorten.spaces() %>% trimws_around("-")
  instr_parts = strsplit(instr," ",fixed = TRUE)

  # Replace first ( with " " and then cut out bracket
  substring(str, br.start, br.end) = " "
  str = str.cutout(str, br.start+1, br.end)

  end.pos = stringi::stri_locate_first_regex(str, "(,)|( if )|(\\[)|( in )|$")[,1]-1
  exo = substring(str,1, end.pos) %>% trimws() %>% shorten.spaces() %>% trimws_around("-")
  exo_parts = strsplit(exo," ",fixed = TRUE)

  str = substring(str, end.pos+1)
  tibble(row = rows, method = method, depvar = depvar, xformula = xformula, exo_parts = exo_parts, endo_parts = endo_parts, instr_parts = instr_parts, str=str)

}

parse.stata.reg.vars.reghdfe = function(cmd, str, rows, has.method = FALSE) {
  restore.point("parse.stata.reg.vars.reghdfe")

  end.pos = stringi::stri_locate_first_regex(str, "(,)|( if )|(\\[)|( in )|$")[,1]-1
  before_opt_str = stringi::stri_sub(str,1, end.pos)

  if (has.substr(before_opt_str, "(")) {
    parse.stata.reg.vars.iv(cmd, str, rows, has.method)
  } else {
    parse.stata.reg.vars.default(cmd, str, rows)
  }
}

parse.stata.reg.vars.default = function(cmd, str, rows) {
  restore.point("parse.reg.vars.default")
  depvar = str.left.of(str," ") %>% trimws()

  # We add a space to detect if xformula starts with if or in
  str = str.right.of(str, " ") %>% trimws() %>% paste0(" ",.)
  end.pos = stringi::stri_locate_first_regex(str, "(,)|( if )|(\\[)|( in )|$")[,1]-1

  xformula = substring(str,1, end.pos) %>%
    #stri_replace_all_fixed(" _I"," i.")  %>%
    trimws() %>% shorten.spaces() %>%
    # In Stata we can have something like
    # reg y x1 - x5
    # We want to normalize it to reg y x1-x5
    # so that we can later expand that Stata pattern
    trimws_around("-")
  exo_parts = strsplit(xformula," ",fixed = TRUE)

  str = substring(str, end.pos+1)
  tibble(row = rows, method = "", depvar = depvar, xformula = xformula, exo_parts = exo_parts, endo_parts = vector("list", length(cmd)), instr_parts = vector("list", length(cmd)), str=str)

}


add_stata_reg_opts = function(reg.info, pho=NULL) {
  restore.point("add_stata_reg_opts")
  #opts_str[1] = "jd asb xhb(sdg)"
  opts_str = reg.info$opts_str

  # We have examples like absorb (var)r
  # we need to transform it to absorb(var) r
  # Note that (var) will be replaced by a placeholder
  # and we have absorb #~br1~#r which becomes absorb#~br1~# r
  opts_str = stri_replace_all_regex(opts_str,"~#(?=[a-zA-Z])","~# ")
  opts_str = stri_replace_all_regex(opts_str,"[ \t]*#~br","#~br")

  if (all(is.na(opts_str))) {
    reg.info$opts.df = vector("list",NROW(reg.info))
    return(reg.info)
  }


  opts_str[is.na(opts_str)] = "-"


  if (is.null(pho)) {
    txt = paste0(opts_str, collapse = "\n")
    txt = shorten.spaces(txt)
    pho = try(blocks.to.placeholder(txt, start=c("("), end=c(")"), ph.prefix = "#~br"))
    if (is(pho,"try-error")) {
      pho = stepwise.blocks.to.placeholder(opts_str)
    }
    txt = pho$str; ph.df = pho$ph.df
    txt = strsplit(txt,"\n",fixed = TRUE)[[1]]
  } else {
    txt = opts_str
    txt = shorten.spaces(txt)
    ph.df = pho$ph.df
  }
  txt[txt == "-"] = ""

  opts_li = strsplit(txt, " ", fixed=TRUE)
  opts_mat = stringi::stri_split_fixed(txt, " ", simplify=TRUE)
  nr = length(txt); nc = ncol(opts_mat)
  opts_grid = data.frame(.ROW = rep(1:nr, each=nc), creg.num = rep(reg.info$creg.num, each=nc), opt_num = rep(1:nc, times=nr), opt_str = as.vector(t(opts_mat)))
  opts_grid = opts_grid[opts_grid$opt_str != "",]

  opts_grid$opt_str = replace.placeholders(opts_grid$opt_str, ph.df)

  not.found = rep("", NROW(opts_grid))
  opts_grid$opt = str.left.of(opts_grid$opt_str, "(") %>% trimws()
  opts_grid$opt_args = str.right.of(opts_grid$opt_str,"(", not.found = not.found) %>% trimws() %>% str.remove.ends(0,1)

  nested_df = opts_grid %>% group_by(.ROW) %>% tidyr::nest()
  colnames(nested_df)[2] = "opts.df"

  reg.info$.ROW = seq_len(NROW(reg.info))

  reg.info = left_join(reg.info, nested_df, by=".ROW")
  reg.info = select(reg.info, -.ROW)
  return(reg.info)
}

