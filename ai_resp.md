
!MODIFICATION stata_expr_to_cterm in mrb_cterms.R
scope = "function"
file = "mrb_cterms.R"
function_name = "stata_expr_to_cterm"
description = "Use the shared regtranslate TS-prefix normalizer instead of blanket dot replacement so syntax-derived cterms match output-derived cterms."
---------------------------------------------------------------------------------------------------------------------------------------------------------

```r
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

  cterm = stringi::stri_replace_all_regex(stata_expr, "(#+)|(\\|)|(\\*)", "#")
  cterm = gsub(" ", "", cterm)

  if (any(has.substr(cterm, "."))) {
    restore.point("cterm_ts_op")
  }

  cterm = stringi::stri_replace_all_regex(
    cterm,
    "(#|^)[iI]([0-9]+)\\.([a-zA-Z_0-9]+)",
    "$1$3=$2"
  )
  cterm = gsub("#[ic]\\.", "#", cterm, ignore.case = TRUE)
  cterm = gsub("^[ic]\\.", "", cterm, ignore.case = TRUE)
  cterm = gsub("#[ic]([LlFfDdSsOo][0-9]*\\.)", "#\\1", cterm, ignore.case = TRUE)
  cterm = gsub("^[ic]([LlFfDdSsOo][0-9]*\\.)", "\\1", cterm, ignore.case = TRUE)
  cterm = stringi::stri_replace_all_regex(cterm, "#[iI]?[bB]([0-9]+)\\.", "#")
  cterm = stringi::stri_replace_all_regex(cterm, "^[iI]?[bB]([0-9]+)\\.", "")

  # Convert plain factor notation before TS-prefix normalization:
  # 2.x1 -> x1=2
  cterm = stringi::stri_replace_all_regex(
    cterm,
    "(^|#)([0-9]+)\\.([a-zA-Z_][a-zA-Z_0-9]*)",
    "$1$3=$2"
  )

  # Use the shared TS-prefix normalizer so syntax-derived cterms match
  # output-derived cterms.
  cterm = regtranslate:::replace_cterms_dot(cterm)
  cterm = sort_interaction_terms(cterm)

  cterm
}
```

!END_MODIFICATION stata_expr_to_cterm in mrb_cterms.R
