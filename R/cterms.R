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
stata_expr_to_cterm = function(stata_expr) {
  restore.point("stata_expr_to_cterm")
  cterm = stringi::stri_replace_all_regex(stata_expr,"(#+)|(\\|)|(\\*)","#")
  cterm = gsub(" ","", cterm)

  # i2000.year => year=2000
  # generates a full dummy set. Not only numeric values allowed.
  #cterm = "b1990.x##I2000.year"
  cterm = stringi::stri_replace_all_regex(cterm, "(#|^)[iI]([0-9]+)\\.([a-zA-Z_0-9]+)","$1$3=$2" )

  # i.year => year
  # c.year => year
  cterm = gsub("#[ic]\\.","#", cterm, ignore.case=TRUE)
  cterm = gsub("^[ic]\\.","", cterm, ignore.case=TRUE)

  # ib2000.year => year
  # b2000.year => year
  # will generate full dummy set with reference level 2000
  cterm = stringi::stri_replace_all_regex(cterm, "#[iI]?[bB]([0-9]+)\\.","#" )
  cterm = stringi::stri_replace_all_regex(cterm, "^[iI]?[bB]([0-9]+)\\.","" )

  # L1.year => L1@year

  # remove i. in the beginning of a variable name
  cterm = gsub(".","@", cterm, fixed=TRUE)

  # Update: Replace stuff like
  #      1.disab#0.vid_late
  # with disab=1#vid_late=0
  cterm = stri_replace_all_regex(cterm, "(^|#)([0-9]+)@([a-zA-Z_][a-zA-Z_0-9]*)", "$1$3=$2")


  cterm
}

#
# cterm_to_cvar = function(cterm) {
#   rx1 = "=[a-zA-Z_0-9.]*:"
#   str = stringi::stri_replace_all_regex(cterm,rx1,"#")
#   rx2 = "=[a-zA-Z_0-9.]*$"
#   str = stringi::stri_replace_all_regex(str,rx2,"")
#   str
#
# }



canonical.stata.output.terms = function(terms,labels, cmd=NULL) {
  restore.point("canonical.stata.output.terms")

  terms = canonical.output.terms.stata.default(terms)
  terms = canonical.output.terms.stata.xi(terms, labels)
  return(terms)


  cmd = rep(cmd, length.out = length(terms))
  canonical = rep(NA, NROW(terms))

  rows = which(is.na(canonical))
  canonical[rows] = canonical.output.terms.stata.default(terms[rows])

  na.rows = which(is.na(canonical))
  if (length(na.rows)>0) {
    stop(paste0("Cannot yet parse stata output terms for ", paste0(unique(cmd[na.rows]), collapse = ", "), ". Update canonical.stata.output.terms"))
  }
  canonical
}

adapt.stata.prefix.notation = function(cterm) {
  cterm = gsub(".","@", cterm,fixed = TRUE)
  cterm
}

canonical.output.terms.stata.default = function(terms, ...) {
  restore.point("canonical.output.stata.default")
  cons = which(terms %in% c("_cons","o._cons"))
  terms[cons] = "(Intercept)"

  # Default interaction term with #

  #terms = c("4.x#2.b","1.c#2.dg#3b.l","c")
  # Note that before the . there is a number for factors
  # if we have L2.x1 then we keep L2.x1 as it indicates a time lag

  rows = which(has.substr(terms,"#"))
  if (length(rows)>0) {
    lhs = str.left.of(terms[rows], "#")
    rhs = str.right.of(terms[rows], "#")
    terms[rows] = paste0(
      canonical.output.terms.stata.default(lhs, labels[rows]),"#",
      canonical.output.terms.stata.default(rhs, labels[rows]))
  }

  # Remove a o. before a variable name: (means ommited for xi)
  #before.dot = str.left.of(terms, ".", not.found = rep("", NROW(terms)))
  #rows = has.s(terms,"o.")
  #terms[rows] = substring(terms[rows], 3)

  # Factor variable (no string only integer before .)
  rows = dot.rows = has.substr(terms,".") & !is.na(suppressWarnings(as_integer(substring(terms,1,1))))
  base = str.right.of(terms[rows], ".")
  level = str.left.of(terms[rows], ".")

  # remove b at end of level: this means that the variable is dropped
  brows = endsWith(level,"b")  | endsWith(level, "o")
  level[brows] = str.remove.ends(level[brows],right = 1)

  terms[rows] = paste0(base, "=", level)

  # Remove certain prefixes like o. or co.

  rows = has.substr(terms,".") & !is.na(suppressWarnings(as_integer(substring(terms,1,1))))

  terms = remove.unused.stata.prefixes(terms)
  terms = adapt.stata.prefix.notation(terms)
  terms
}

remove.unused.stata.prefixes = function(terms) {
  terms = gsub("(^o\\.|^co\\.|^c\\.)","", terms)
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

canonical.r.output.terms = function(terms, vi=NULL, rcmd=NULL, from.stata=TRUE) {
  restore.point("canonical.r.output.terms")

  rcmd = rep(rcmd, length.out = length(terms))

  rows = which(rcmd %in% c("fixest","feols","mr_fixest"))
  terms[rows] = canonical.output.terms.fixest(terms[rows], vi, from.stata=from.stata)

  rows = setdiff(seq_along(terms), rows)
  terms[rows] = canonical.output.terms.fixest(terms[rows], from.stata=from.stata)

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
    dat = create_cterm_col(dat, cterm, timevar=NA, panelvar=NA, tdelta=NA)
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
    dat = create_cterm_col(dat, cte)
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
create_prefix_nolevel_cterm_col = function(dat,cterm, panelvar=NA, timevar=NA, tdelta=NA) {
  restore.point("create_numeric_cterm_col")

  prefix  = cterm_extract_prefix(cterm)
  basevar = cterm_extract_base(cterm)

  dat[[cterm]] = NA

  if (!has.col(dat, cterm)) {
    msg = paste0("Column ", basevar, " does not exist in data set and thus I cannot generate the cterm ", cterm)
    repbox_problem(type="missing_var",msg=msg, fail_action="msg", fail_action = "error")
    return(dat)
  }
  if (prefix == "") {
    return(dat)
  }

  baseval = dat[[basevar]]
  if (prefix=="log") {
    dat[[cterm]] = log(baseval)
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



#
# example = function() {
#   # Test example 1: Create lag variables for x1 using L(0/2).x1
#   # This generates columns L0.x1, L1.x1, and L2.x1
#   dat1 <- data.frame(time = 1:10, x1 = 101:110)
#   dat1 <- create_time_series_range(dat1, "L(0/2)", "x1", timevar = "time")
#   cat("Test example 1 (lags):\n")
#   print(head(dat1))
#
#   # Test example 2: Create lead variables for x1 using F(0/3).x1
#   # This generates columns F0.x1, F1.x1, F2.x1, and F3.x1
#   dat2 <- data.frame(time = 1:10, x1 = 101:110)
#   dat2 <- create_time_series_range(dat2, "F(0/3)", "x1", timevar = "time")
#   cat("\nTest example 2 (leads):\n")
#   print(head(dat2))
#
#   # Test example 3: Create difference variables for x1 using D(1/2).x1
#   # This generates columns D1.x1 and D2.x1
#   dat3 <- data.frame(time = 1:10, x1 = 101:110)
#   dat3 <- create_time_series_range(dat3, "D(1/2)", "x1", timevar = "time")
#   cat("\nTest example 3 (differences):\n")
#   print(head(dat3))
#
# }
# # Helper function to check if a value is empty
# #is_empty <- function(x) {
# #  return(is.null(x) || length(x) == 0 || (is.character(x) && x == ""))
# #}
#
# # Subfunction to handle time series range expressions like L(0/4).x1
# create_time_series_range <- function(dat, prefix, basevar, panelvar = NA, timevar = NA, tdelta = 1) {
#   # Expect prefix in the form "L(0/4)" (or F, D, S, etc)
#   # Get the operator letter (e.g., "L")
#   op <- toupper(substr(prefix, 1, 1))
#
#   # Extract the range string between the parentheses (e.g., "0/4")
#   range_str <- sub("^[A-Za-z]\\(([^)]+)\\).*", "\\1", prefix)
#
#   # Split the range into start and end values
#   parts <- strsplit(range_str, "/")[[1]]
#   if (length(parts) != 2) stop("Invalid time series range format in prefix: ", prefix)
#   start <- as_integer(trimws(parts[1]))
#   end   <- as_integer(trimws(parts[2]))
#
#   if (is.na(tdelta)) tdelta <- 1
#
#   # Loop over the sequence and, for each value, create a new column
#   for(n in seq(start, end)) {
#     # Construct the new variable name, e.g., "L0.x1", "L1.x1", etc.
#     new_cterm <- paste0(op, n, ".", basevar)
#
#     # Build the argument list (using timevar and panelvar if available)
#     args <- list(x = dat[[basevar]])
#     if (!is_empty(timevar))  args$t <- dat[[timevar]]
#     if (!is_empty(panelvar)) args$g <- dat[[panelvar]]
#
#     # For lags and leads we use collapse::flag; for differences we use collapse::fdiff.
#     # For leads (F) we use a negative shift.
#     if (op == "L") {
#       fun <- collapse::flag
#       args$n <- n * tdelta
#     } else if (op == "F") {
#       fun <- collapse::flag
#       args$n <- -n * tdelta
#     } else if (op == "D") {
#       fun <- collapse::fdiff
#       args$diff <- n
#     } else if (op == "S") {
#       fun <- collapse::fdiff
#       args$n <- n
#     } else {
#       stop("Operator ", op, " not supported in time series range expressions")
#     }
#     # Generate the new column by calling the appropriate function
#     dat[[new_cterm]] <- do.call(fun, args)
#   }
#   return(dat)
# }
#
#
#
#
