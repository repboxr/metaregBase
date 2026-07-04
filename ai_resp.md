The problem occurs because the Stata regression command contains interaction terms inside parentheses (e.g., `c.gpa#(c.mgpa c.sdgpa c.mgpa#c.sdgpa)`), which are nested *inside* the Instrumental Variable (IV) block `(endo = instr)`. 

When `cmdpart_parse_iv_varlist` attempts to isolate the IV block using `regexpr("\\)", varlist_str)`, it incorrectly stops at the **first** closing parenthesis it encounters. Consequently, it shatters the varlist, ripping equations in half and creating unbalanced parenthesis fragments like `c.gpa#(c.mgpa`. These broken fragments then cascade into `distribute_stata_parens`, which blindly stitches them back together with surrounding tokens, inadvertently bundling unrelated variables into one massive invalid string (e.g. `"appordr male young old hbo gpa"`).

To fix this, we need to:
1. Make `cmdpart_parse_iv_varlist` find the **outermost** parenthesis block using `locate_1st_level_braces` and safely extract the IV block around the equal sign `=`.
2. Implement a new tokenizer `extract_varlist_tokens` that intelligently splits variables by spaces, but refuses to break inside parenthesis groups. 
3. Wire `cmdpart_parse_default_varlist` and `cmdpart_parse_iv_varlist` to use this new robust tokenizer.

Here are the fixes for `repboxUtils/R/cmdpart_reg.R`.

!MODIFICATION cmdpart_parse_varlist in /home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R
scope = "function"
file = "/home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R"
function_name = "cmdpart_parse_varlist"
description = "Update cmdpart_parse_varlist and its sub-functions to correctly tokenize varlists containing parentheses using the new extract_varlist_tokens logic."
---
```r
#' Parse a Stata varlist syntactically based on the command type
cmdpart_parse_varlist = function(cmd, varlist_str) {
  varlist_str = trimws(varlist_str)

  # Normalize spaces around dashes to keep ranges (e.g., "var1 - var5") as a single token
  varlist_str = gsub("\\s*-\\s*", "-", varlist_str)

  iv_commands = c("ivregress", "ivreg2", "ivreg", "xtivreg2", "xtivreg", "ivreghdfe", "reg2hdfespatial")
  is_iv_cmd = cmd %in% iv_commands
  is_reghdfe = cmd == "reghdfe"

  # Check if there is an explicit Instrumental Variable block: (endo = instr)
  has_iv_syntax = grepl("\\(.*=.*\\)", varlist_str)

  if ((is_iv_cmd || is_reghdfe) && has_iv_syntax) {
    return(cmdpart_parse_iv_varlist(cmd, varlist_str))
  } else {
    return(cmdpart_parse_default_varlist(varlist_str))
  }
}
```
!END_MODIFICATION cmdpart_parse_varlist in /home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R

!MODIFICATION cmdpart_parse_iv_varlist in /home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R
scope = "function"
file = "/home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R"
function_name = "cmdpart_parse_iv_varlist"
description = "Fix IV block extraction using locate_1st_level_braces to avoid early matching of closing parentheses in nested expressions."
---
```r
#' Parse IV varlists including subcommands and parenthesis blocks
cmdpart_parse_iv_varlist = function(cmd, varlist_str) {
  subcmd = NULL

  # Extract specific subcmd for `ivregress` (e.g., "2sls", "gmm")
  if (cmd == "ivregress") {
    parts = strsplit(varlist_str, "\\s+", perl = TRUE)[[1]]
    subcmd = parts[1]
    # Strip subcmd from the beginning of the varlist
    varlist_str = sub(paste0("^", subcmd, "\\s+"), "", varlist_str)
    varlist_str = trimws(varlist_str)
  }

  # Locate the (endo = instr) block safely
  brace_pos = locate_1st_level_braces(varlist_str, open="(", close=")")
  
  iv_block_idx = NA
  if (!is.null(brace_pos) && nrow(brace_pos) > 0) {
    for (i in seq_len(nrow(brace_pos))) {
      block_str = substr(varlist_str, brace_pos[i, 1] + 1, brace_pos[i, 2] - 1)
      
      # Mask inner braces to find top-level '='
      inner_braces = locate_1st_level_braces(block_str, open="(", close=")")
      safe_block = block_str
      if (!is.null(inner_braces) && nrow(inner_braces) > 0) {
        for (j in seq_len(nrow(inner_braces))) {
          len = inner_braces[j, 2] - inner_braces[j, 1] + 1
          substring(safe_block, inner_braces[j, 1], inner_braces[j, 2]) <- strrep(" ", len)
        }
      }
      
      if (grepl("=", safe_block)) {
        iv_block_idx = i
        break
      }
    }
  }

  if (!is.na(iv_block_idx)) {
    br_start = brace_pos[iv_block_idx, 1]
    br_end = brace_pos[iv_block_idx, 2]
    
    iv_block = substr(varlist_str, br_start + 1, br_end - 1)

    # Everything outside the parentheses are either the depvar or exogenous variables
    outside_before = trimws(substr(varlist_str, 1, br_start - 1))
    outside_after = trimws(substr(varlist_str, br_end + 1, nchar(varlist_str)))
    outside_str = trimws(paste(outside_before, outside_after))

    # Parse IV block
    # Find '=' at the top level of iv_block
    inner_braces = locate_1st_level_braces(iv_block, open="(", close=")")
    safe_block = iv_block
    if (!is.null(inner_braces) && nrow(inner_braces) > 0) {
      for (j in seq_len(nrow(inner_braces))) {
        len = inner_braces[j, 2] - inner_braces[j, 1] + 1
        substring(safe_block, inner_braces[j, 1], inner_braces[j, 2]) <- strrep(" ", len)
      }
    }
    eq_pos = regexpr("=", safe_block)

    if (eq_pos > 0) {
      endo_str = trimws(substr(iv_block, 1, eq_pos - 1))
      instr_str = trimws(substr(iv_block, eq_pos + 1, nchar(iv_block)))

      endo = extract_varlist_tokens(endo_str)
      instr = extract_varlist_tokens(instr_str)
    } else {
      # Fallback if '=' is missing inside parenthesis
      endo = character(0)
      instr = extract_varlist_tokens(iv_block)
    }

    # Parse Outside block
    outside_tokens = extract_varlist_tokens(outside_str)

    depvar = if (length(outside_tokens) > 0) outside_tokens[1] else character(0)
    exo = if (length(outside_tokens) > 1) outside_tokens[2:length(outside_tokens)] else character(0)

  } else {
    # Fallback to standard OLS parsing if IV regex logic fails
    return(cmdpart_parse_default_varlist(varlist_str))
  }

  # Construct token dataframe
  df = tibble::tibble(
    part = c(if (!is.null(subcmd)) "subcmd" else character(0),
             if (length(depvar) > 0) "v" else character(0),
             rep("v", length(exo) + length(endo) + length(instr))),
    tag = c(if (!is.null(subcmd)) "" else character(0),
            if (length(depvar) > 0) "depvar" else character(0),
            rep("exo", length(exo)),
            rep("endo", length(endo)),
            rep("instr", length(instr))),
    content = c(if (!is.null(subcmd)) subcmd else character(0),
                depvar, exo, endo, instr),
    parent = c(if (!is.null(subcmd)) "varlist" else character(0),
               rep("varlist", length(depvar) + length(exo) + length(endo) + length(instr)))
  )

  return(df)
}
```
!END_MODIFICATION cmdpart_parse_iv_varlist in /home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R

!MODIFICATION cmdpart_parse_default_varlist in /home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R
scope = "function"
file = "/home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R"
function_name = "cmdpart_parse_default_varlist"
description = "Update cmdpart_parse_default_varlist to use extract_varlist_tokens."
---
```r
#' Parse standard OLS varlists
cmdpart_parse_default_varlist = function(varlist_str) {
  tokens = extract_varlist_tokens(varlist_str)
  
  if (length(tokens) == 0) return(NULL)

  depvar = tokens[1]
  exo = if (length(tokens) > 1) tokens[2:length(tokens)] else character(0)

  df = tibble::tibble(
    part = "v",
    tag = c("depvar", rep("exo", length(exo))),
    content = c(depvar, exo),
    parent = "varlist"
  )
  return(df)
}
```
!END_MODIFICATION cmdpart_parse_default_varlist in /home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R

!MODIFICATION extract_varlist_tokens in /home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R
scope = "function"
file = "/home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R"
insert_after_fun = "cmdpart_parse_default_varlist"
description = "Add a new helper function extract_varlist_tokens to tokenize a varlist string without breaking terms enclosed in parentheses."
---
```r
#' Split a string by spaces, but keep content within parentheses/brackets together.
extract_varlist_tokens = function(str) {
  if (is.na(str) || trimws(str) == "") return(character(0))
  
  chars = strsplit(str, "")[[1]]
  tokens = character()
  
  # preallocate
  token_chars = character(length(chars))
  token_len = 0
  
  level = 0
  for (ch in chars) {
    if (ch == "(") level = level + 1
    else if (ch == ")") {
      if (level > 0) level = level - 1
    }
    
    if (level == 0 && (ch == " " || ch == "\t" || ch == "\n" || ch == "\r")) {
      if (token_len > 0) {
        tokens = c(tokens, paste(token_chars[1:token_len], collapse = ""))
        token_len = 0
      }
    } else {
      token_len = token_len + 1
      token_chars[token_len] = ch
    }
  }
  if (token_len > 0) {
    tokens = c(tokens, paste(token_chars[1:token_len], collapse = ""))
  }
  
  tokens
}
```
!END_MODIFICATION extract_varlist_tokens in /home/rstudio/repbox/repboxUtils/R/cmdpart_reg.R
