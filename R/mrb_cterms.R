# Parse and map the shown coefficient names (terms) in regression
# ouputs of different stata and r functions.
#
# The canonical terms will be refered to as cterm
#
# a=5:L2@b (the cterm assuming that a and c are factors)
#


# Take an expr from a Stata regression command and convert it to
# cterm representation

# Example:
#
# i.i1##c.d1 -> i1:d1
#
# L2.x -> L2@x
#
#
#
# cterm will not contain info on whether the variable is used
# as factor or not.
# FILE: mrb_cterms.R
stata_expr_to_cterm = function(stata_expr) {
  restore.point("stata_expr_to_cterm")

  cterm = stringi::stri_replace_all_regex(stata_expr,"(#+)|(\\|)|(\\*)","#")
  cterm = gsub(" ","", cterm)

  if (any(has.substr(cterm, "."))) {
    restore.point("cterm_ts_op")
  }

  cterm = stringi::stri_replace_all_regex(cterm, "(#|^)[iI]([0-9]+)\\.([a-zA-Z_0-9]+)","$1$3=$2" )
  cterm = gsub("#[ic]\\.","#", cterm, ignore.case=TRUE)
  cterm = gsub("^[ic]\\.","", cterm, ignore.case=TRUE)
  cterm = gsub("#[ic]([LlFfDdSsOo][0-9]*\\.)","#\\1", cterm, ignore.case=TRUE)
  cterm = gsub("^[ic]([LlFfDdSsOo][0-9]*\\.)","\\1", cterm, ignore.case=TRUE)
  cterm = stringi::stri_replace_all_regex(cterm, "#[iI]?[bB]([0-9]+)\\.","#" )
  cterm = stringi::stri_replace_all_regex(cterm, "^[iI]?[bB]([0-9]+)\\.","" )

  # Expand missing dots between TS operators (e.g. L1D1. -> L1.D1., LD. -> L.D.)
  old_cterm = ""
  while (any(old_cterm != cterm)) {
    old_cterm = cterm
    cterm = gsub("(?<=^|#|\\.)([LlFfDdSsOo][0-9]*)([LlFfDdSsOo][0-9]*\\.)", "\\1.\\2", cterm, perl=TRUE)
  }

  old_cterm = ""
  while (any(old_cterm != cterm)) {
    old_cterm = cterm
    cterm = gsub("(?<=^|#|\\.)([LlFfDdSsOo])1?\\.", "\\U\\1\\E.", cterm, perl=TRUE)
    cterm = gsub("(?<=^|#|\\.)([LlFfDdSsOo])([2-9]|[1-9][0-9]+)\\.", "\\U\\1\\E\\2.", cterm, perl=TRUE)
  }

  cterm = gsub(".","@", cterm, fixed=TRUE)
  cterm = stringi::stri_replace_all_regex(cterm, "(^|#)([0-9]+)@([a-zA-Z_][a-zA-Z_0-9]*)", "$1$3=$2")
  cterm = sort_interaction_terms(cterm)

  cterm
}


canonical.stata.output.terms = function(terms,labels, cmd=NULL) {
  restore.point("canonical.stata.output.terms")

  terms = canonical.output.terms.stata.default(terms)
  terms = canonical.output.terms.stata.xi(terms, labels)
  terms = sort_interaction_terms(terms)

  return(terms)
}


adapt.stata.prefix.notation = function(cterm) {
  cterm = gsub(".","@", cterm,fixed = TRUE)
  cterm
}

canonical.output.terms.stata.default = function(terms, ...) {
  restore.point("canonical.output.stata.default")
  cons = which(terms %in% c("_cons","o._cons"))
  terms[cons] = "(Intercept)"

  rows = which(has.substr(terms,"#"))
  if (length(rows)>0) {
    lhs = str.left.of(terms[rows], "#")
    rhs = str.right.of(terms[rows], "#")
    terms[rows] = paste0(
      canonical.output.terms.stata.default(lhs, labels[rows]),"#",
      canonical.output.terms.stata.default(rhs, labels[rows]))
  }

  rows = dot.rows = has.substr(terms,".") & !is.na(suppressWarnings(as_integer(substring(terms,1,1))))
  base = str.right.of(terms[rows], ".")
  level = str.left.of(terms[rows], ".")

  match_lvl = stringi::stri_match_first_regex(trimws(level), "^([0-9]+)([bo]?)([a-zA-Z]*[0-9]*)$")
  matched = !is.na(match_lvl[, 1])

  if (any(matched)) {
    num_val = match_lvl[matched, 2]
    ts_op   = toupper(match_lvl[matched, 4])
    ts_op = gsub("^([LFDSO])1$", "\\1", ts_op)
    has_ts = nchar(ts_op) > 0
    if (any(has_ts)) {
      update_idx = which(matched)[has_ts]
      base[update_idx] = paste0(ts_op[has_ts], "@", base[update_idx])
    }
    level[matched] = num_val
  }

  unmatched_idx = which(!matched)
  if (length(unmatched_idx) > 0) {
    brows = endsWith(trimws(level[unmatched_idx]), "b") | endsWith(trimws(level[unmatched_idx]), "o")
    if (any(brows)) {
      b_idx = unmatched_idx[brows]
      level[b_idx] = str.remove.ends(level[b_idx], right = 1)
    }
  }

  terms[rows] = paste0(base, "=", level)

  terms = remove.unused.stata.prefixes(terms)

  # Expand missing dots between TS operators (e.g. L1D1. -> L1.D1., LD. -> L.D.)
  old_terms = ""
  while (isTRUE(any(old_terms != terms, na.rm=TRUE))) {
    old_terms = terms
    terms = gsub("(?<=^|#|\\.)([LlFfDdSsOo][0-9]*)([LlFfDdSsOo][0-9]*\\.)", "\\1.\\2", terms, perl=TRUE)
  }

  old_terms = ""
  while (isTRUE(any(old_terms != terms, na.rm=TRUE))) {
    old_terms = terms
    terms = gsub("(?<=^|#|\\.)([LlFfDdSsOo])1?\\.", "\\U\\1\\E.", terms, perl=TRUE)
    terms = gsub("(?<=^|#|\\.)([LlFfDdSsOo])([2-9]|[1-9][0-9]+)\\.", "\\U\\1\\E\\2.", terms, perl=TRUE)
  }

  terms = adapt.stata.prefix.notation(terms)
  terms
}

remove.unused.stata.prefixes = function(terms) {
  terms = gsub("(^o\\.|^co\\.|^c\\.)","", terms)
  terms = gsub("^[ic]([LlFfDdSsOo][0-9]*\\.)","\\1", terms, ignore.case=TRUE)
  terms
}

canonical.output.terms.stata.xi = function(terms, labels, do.subst=TRUE, xi.rows=NULL) {
  restore.point("canonical.output.stata.xi")
  #stop()
  # A term for fator using xi e.g. i.farmass
  # _Ifarmass_2
  if (is.null(xi.rows)) {
    xi.rows = which(startsWith(terms, "_I") & has.substr(labels,"=="))
    if (length(xi.rows)==0) return(terms)
  }


  # To do: work with interaction terms
  res = rep("", length(xi.rows))
  str = labels[xi.rows]
  if (do.subst) {
    str = gsub("(","", str, fixed=TRUE)
    str = gsub(")","", str, fixed=TRUE)
    str = gsub("*","&", str, fixed=TRUE)
  }

  ia.rows = which(has.substr(str,"&"))
  if (length(ia.rows)>0) {
    lhs = str.left.of(str[ia.rows], "&")
    rhs = str.right.of(str[ia.rows], "&")
    terms[xi.rows[ia.rows]] = paste0(
      canonical.output.terms.stata.xi(lhs,lhs,  do.subst=FALSE, xi.rows = seq_along(ia.rows)),"#",
      canonical.output.terms.stata.xi(rhs,rhs,  do.subst=FALSE, xi.rows = seq_along(ia.rows)))
  }

  rows = setdiff(which(has.substr(str,"==")), ia.rows)
  if (length(rows)>0) {
    loc = stringi::stri_locate_first_fixed(str[rows],"==")
    base = substring(str[rows], 1, loc[,1]-1)
    level = substring(str[rows], loc[,2]+1)
    terms[xi.rows[rows]] = paste0(base, "=", level)
  }

  terms = trimws(terms)
  terms = remove.unused.stata.prefixes(terms)
  terms = adapt.stata.prefix.notation(terms)
  terms
}

# Replace the existing canonical.r.output.terms function:
canonical.r.output.terms = function(terms, vi=NULL, rcmd=NULL, from.stata=TRUE) {
  restore.point("canonical.r.output.terms")

  rcmd = rep(rcmd, length.out = length(terms))

  rows = which(rcmd %in% c("fixest","feols","mr_fixest"))
  terms[rows] = canonical.output.terms.fixest(terms[rows], vi, from.stata=from.stata)

  rows = setdiff(seq_along(terms), rows)
  terms[rows] = canonical.output.terms.fixest(terms[rows], from.stata=from.stata)

  terms = sort_interaction_terms(terms)
  terms
}

canonical.output.terms.r.default = function(terms, from.stata=TRUE) {
  restore.point("canonical.output.terms.fixest")

  # factor in form
  # factor(i1)4:factor(d1)1
  # to i4=4°°d1=1
  factor.rx = "factor\\(([a-zA-Z0-9_.]*)\\)"
  terms = gsub(factor.rx,"\\1=",terms,fixed=FALSE)

  # Replace . by @ if . was a Stata prefix
  if (from.stata) {
    terms = gsub(".","@", terms, fixed=TRUE)
  }


  terms
}



canonical.output.terms.fixest = function(terms, vi, from.stata=TRUE) {
  restore.point("canonical.output.terms.fixest")

  # factor in form: farmass_q::2
  # to farmass_q=2
  terms = gsub("`","", terms, fixed=TRUE)
  terms = gsub("::","=", terms, fixed=TRUE)
  #terms = gsub(" ","", terms, fixed=TRUE)

  # factor in form
  # factor(i1)4:factor(d1)1
  # to i1=4:d1=1
  factor.rx = "factor\\(([a-zA-Z0-9_.]*)\\)"
  terms = gsub(factor.rx,"\\1=",terms,fixed=FALSE)

  # Replace . by @ if . was a Stata prefix
  if (from.stata) {
    terms = gsub(".","@", terms, fixed=TRUE)
  }



  # In an IV regression feols adds "fit_" to the result
  # variable. We want to remove that
  rows = which(startsWith(terms,"fit_"))
  if (length(rows)>0) {
    terms.no.fit = substring(terms[rows], 5)
    change = (!terms[rows] %in% vi$ia_cterm) & (terms.no.fit %in% vi$ia_cterm)
    terms[rows[change]] = terms.no.fit[change]
  }


  # We currently specify the fixest representation
  # as the canonical representation. So we can directly
  # return them.

  terms
}


create_cterm_cols = function(dat, cterms, timevar=NA, panelvar=NA, tdelta=NA) {
  restore.point("create_cterm_cols")
  new.cterms = cterms[!cterms %in% c("(Intercept)",colnames(dat))]
  for (cterm in new.cterms) {
    #cat(cterm,"\n")
    dat = create_cterm_col(dat, cterm, timevar=timevar, panelvar=panelvar, tdelta=tdelta)
  }
  dat
}

create_cterm_col = function(dat, cterm, timevar=NA, panelvar=NA, tdelta=NA, check.abbreviation=TRUE) {
  restore.point("create_cterm_col")
  is_ia = cterm_is_ia(cterm)
  has_level = cterm_has_level(cterm)
  has_prefix = cterm_has_prefix(cterm)


  if (!is_ia & !has_level & !has_prefix) {
    if (cterm %in% colnames(dat)) return(dat)

    # Unfortunately Stata also allows variable name abbreviations in formulas
    # E.g. regress gdp_ger infl_germany
    # would work if there is a column gdp_germany which will be used for gdp_ger
    if (check.abbreviation) {
      abbr.ind = which(startsWith(colnames(dat),cterm))
      if (length(abbr.ind)>0) {
        col = colnames(dat)[abbr.ind[1]]
        dat[[cterm]] = dat[[col]]
        return(dat)
      }
    }

    dat[[cterm]] = NA
    # lnalpha is just shown in nbreg output but not a variable in the data set
    if (!isTRUE(cterm=="lnalpha")) {
      msg = paste0("Column ", cterm, " does not exist in data set and thus I cannot generate the cterm ", cterm)
      repbox_problem(type="regvar_no_match", msg=msg,fail_action = "error")

    }
    return(dat)

  } else if (!is_ia & has_level & !has_prefix) {
    var = str.left.of(cterm, "=")
    val = str.right.of(cterm, "=")
    # Convert to correct class
    cval = as(val,  last(class(dat[[var]])))

    # Create dummy variable
    dat[[cterm]] = 1L*(dat[[var]]==cval)
    return(dat)
  } else if (!is_ia & !has_level & has_prefix) {
    dat = create_prefix_nolevel_cterm_col(dat, cterm,panelvar=panelvar, timevar=timevar, tdelta=tdelta)
    return(dat)
  } else if (!is_ia & has_level & has_prefix) {
    repbox_problem(type = "parse_reg_formula", msg=paste0("Cannot yet generate columns for cterm ", cterm, " that contains a prefix and a factor level."), fail_action = "error")
    dat[[cterm]] = NA
    return(dat)
  }

  # Interaction effects
  cterms = cterm_split_ia(cterm)[[1]]

  # Create all main effects
  for (cte in cterms) {
    dat = create_cterm_col(dat, cte, timevar=timevar, panelvar=panelvar, tdelta=tdelta)
  }

  # If any of the cterms is a factor just paste them
  if (any(sapply(dat[cterms], is.character))) {
    dat[[cterm]] = as.character(dat[[cterms[[1]]]])
    for (i in 2:length(cterms)) {
      dat[[cterm]] = paste0(dat[[cterm]],"#",dat[[ cterms[i] ]])
    }
    return(dat)
  }

  # Multiply the main effects
  dat[[cterm]] = dat[[ cterms[1] ]]
  for (i in 2:length(cterms)) {
    dat[[cterm]] = dat[[cterm]]*dat[[ cterms[i] ]]
  }
  dat
}


# Variables with time series operators. See
# See https://www.stata.com/manuals/u11.pdf#u11.4.4
# We will deal with operators like:

# L2.x1
# but we currently ignore things like
# L(0/4).x1
# FILE: mrb_cterms.R
# Replace the existing create_prefix_nolevel_cterm_col function:

create_prefix_nolevel_cterm_col = function(dat,cterm, panelvar=NA, timevar=NA, tdelta=NA) {
  restore.point("create_prefix_nolevel_cterm_col")

  prefix  = cterm_extract_prefix(cterm)
  basevar = cterm_extract_base(cterm)

  # Recursively generate nested prefix variables (e.g. S2@y before L@S2@y)
  if (!has.col(dat, basevar)) {
    if (cterm_has_prefix(basevar)) {
      dat = create_cterm_col(dat, basevar, timevar=timevar, panelvar=panelvar, tdelta=tdelta)
    }
  }

  dat[[cterm]] = NA

  # Fixed check to target basevar
  if (!has.col(dat, basevar)) {
    msg = paste0("Column ", basevar, " does not exist in data set and thus I cannot generate the cterm ", cterm)
    repbox_problem(type="missing_var",msg=msg, fail_action = "error")
    return(dat)
  }

  if (prefix == "") {
    return(dat)
  }

  baseval = dat[[basevar]]
  if (prefix=="log") {
    dat[[cterm]] = log(baseval)
    return(dat)
  }

  prefix.type = toupper(substring(prefix,1,1))
  prefix.num = substring(prefix,2)

  tdelta = as.numeric(tdelta)
  if (is.na(tdelta)) tdelta = 1


  if (any(has.substr(prefix.num,"("))) {
    repbox_problem(type="parse_reg_formula", "\nCannot yet deal with time series prefixes like L(0/2).", fail_action = "error")
    return(dat)
  }

  args = list(x=dat[[basevar]])
  if (!is_empty(timevar)) {
    args$t = dat[[timevar]]
  }
  if (!is_empty(panelvar)) {
    args$g = dat[[panelvar]]
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

  } else if (prefix.type == "O") {
    # o. means that the variable shall be omitted in
    # the regression. The variables will be just
    # the original variables.
    fun = identity
  } else {
    stop(paste0("No code yet to create variables for cterm prefix ", prefix.type))
  }


  if (tdelta > 1 & prefix.type %in% c("L","F","D","S")) {
    # Note this code requires a sufficiently new
    # collapse version like 1.9.6
    args$t = dat[[timevar]]
    args$n = args$n * tdelta
  }

  # Explicit length check to satisfy collapse's requirement
  n_val = if (!is.null(args$n)) abs(args$n) else 1
  diff_val = if (!is.null(args$diff)) abs(args$diff) else 1

  if (prefix.type %in% c("L", "F", "D", "S") && length(args$x) <= n_val * diff_val) {
    dat[[cterm]] = rep(NA_real_, length(args$x))
    return(dat)
  }

  dat[[cterm]] = do.call(fun, args)
  dat
}


cterm_has_level = function(cterm) {
  has.substr(cterm, "=")
}

cterm_has_prefix = function(cterm) {
  has.substr(cterm, "@")
}

cterm_is_ia = function(cterm) {
  has.substr(cterm, "#")
}

cterm_split_ia = function(cterm) {
  strsplit(cterm, "#", fixed=TRUE)
}

cterm_extract_prefix = function(cterm) {
  str.left.of(cterm, "@", not.found=rep("",length(cterm)))
}

cterm_extract_base = function(cterm, keep.level = FALSE) {
  base = str.right.of(cterm, "@")
  if (!keep.level) {
    base = str.left.of(base, "=")
  }
  base
}

cterm_extract_level = function(cterm) {
  str.right.of(cterm, "=")
}


sort_interaction_terms = function(terms) {
  rows = which(has.substr(terms, "#"))
  if (length(rows) > 0) {
    terms[rows] = split_and_sort(terms[rows], split = "#")
    #terms[rows] = sapply(strsplit(terms[rows], "#", fixed=TRUE), function(x) paste0(sort(x), collapse="#"))
  }
  terms
}

