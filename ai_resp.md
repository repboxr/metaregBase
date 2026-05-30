You are absolutely right. The Stata parser typically registers the `cmd` column using just the first word of the command line, meaning `"import excel"` is recorded as `"import"`. Since `"import"` is already mapped to `"load"` in `drf_stata_cmd_types_vec()`, modifying the mapping lists would indeed have zero effect. Good catch!

The actual issue lies in **`replace_stata_cmdline_path()`** in `repboxDRF/R/drf_tools.R`. The `metaregBase` pipeline uses this function to rewrite data-loading paths to point to the `drf/org_data` folder. However, the static parser inside `repboxStata::replace.files.and.paths.with.ph()` frequently misses file paths in `import` commands. When it fails to find the path, it silently returns the unmodified command, leaving the relative path (`"Data for Main Analysis.xlsx"`) intact, which inevitably breaks when running `mrb_stata.do`.

Additionally, we need to adapt **`drf_stata_ensure_use_clear()`** so that it properly appends the `, clear` option to `import`, `insheet`, `infile`, and `infix` commands, preventing Stata from throwing a "data in memory would be lost" error when loading the new data.

Here are the fixes implementing a robust regex fallback to catch unreplaced file paths in `load` commands:

!MODIFICATION replace_stata_cmdline_path in repboxDRF/R/drf_tools.R
scope = "function"
file = "/home/rstudio/repbox/repboxDRF/R/drf_tools.R"
function_name = "replace_stata_cmdline_path"
description = "Add a fallback to aggressively replace file paths in load commands like 'import excel' that the static parser misses."
---
```r
#' Replace file paths in cleaned Stata command lines
#'
#' @param cmdline Character vector of cleaned Stata commands one per line
#' @param replacement String to insert in place of the file path
#' @return Character vector of commands with paths replaced
replace_stata_cmdline_path = function(cmdline, replacement = '"`r(my_custom_path)\'"', add_clear=TRUE) {
  restore.point("replace_stata_cmdline_paths")

  if (length(replacement)>1 & length(replacement)!= length(cmdline)) {
    stop("cmdline and replacement must have same length.")
  }

  # Ensure replacement is correctly vectorized
  replacement = rep(replacement, length.out = length(cmdline))

  tab = repboxStata::repbox.re.cmdlines.to.tab(cmdline)

  empty_ph = data.frame(ph = character(0), content = character(0))
  res_paths = repboxStata::replace.files.and.paths.with.ph(tab, empty_ph)

  if (nrow(res_paths$ph) == 0) {
    final_cmds = cmdline
  } else {
    fph = res_paths$ph
    fph$content = replacement
    final_cmds = replace.ph.keep.lines(res_paths$txt, fph)
  }

  # Fallback for commands where repboxStata static parsing didn't find the path
  failed_to_replace = (final_cmds == cmdline)
  if (any(failed_to_replace)) {
    for (i in which(failed_to_replace)) {
      cmd = final_cmds[i]
      # Escape potential '$' symbols in file paths for regex engine
      safe_repl = gsub("$", "\\$", replacement[i], fixed = TRUE)

      # 1. Try to replace first quoted string, as import/use paths are typically quoted
      if (stringi::stri_detect_regex(cmd, '"[^"]+"')) {
        final_cmds[i] = stringi::stri_replace_first_regex(cmd, '"[^"]+"', safe_repl)
      } 
      # 2. Look for 'using <path>' unquoted
      else if (stringi::stri_detect_regex(cmd, "\\busing\\s+([^\\s,]+)", case_insensitive=TRUE)) {
        final_cmds[i] = stringi::stri_replace_first_regex(cmd, "(?i)\\b(using\\s+)([^\\s,]+)", paste0("$1", safe_repl))
      } 
      # 3. Look for bare command followed by unquoted path e.g. `import excel filename.xlsx, clear`
      else if (stringi::stri_detect_regex(cmd, "^\\s*(import|use|insheet|infix|infile)\\b", case_insensitive=TRUE)) {
        final_cmds[i] = stringi::stri_replace_first_regex(cmd, "(?i)^(\\s*(?:import|use|insheet|infix|infile)\\b(?:\\s+(?:excel|delimited|sas|spss))?\\s+)([^\\s,]+)", paste0("$1", safe_repl))
      }
    }
  }

  add_clear = rep(add_clear, length.out = length(final_cmds))

  if (any(add_clear)) {
    final_cmds[add_clear] = drf_stata_ensure_use_clear(final_cmds[add_clear])
  }

  final_cmds
}
```
!END_MODIFICATION replace_stata_cmdline_path in repboxDRF/R/drf_tools.R


!MODIFICATION drf_stata_ensure_use_clear in repboxDRF/R/drf_tools.R
scope = "function"
file = "/home/rstudio/repbox/repboxDRF/R/drf_tools.R"
function_name = "drf_stata_ensure_use_clear"
description = "Ensure clear option is also appended for import, insheet, infix, and infile commands."
---
```r
drf_stata_ensure_use_clear = function(cmdline) {
  restore.point("drf_stata_ensure_use_clear")

  if (length(cmdline) == 0) {
    return(cmdline)
  }

  cmd = stringi::stri_trim_both(cmdline)

  is_use_or_import = stringi::stri_detect_regex(
    cmd,
    "^(u|us|use|import|insheet|infix|infile)\\b",
    case_insensitive = TRUE
  )

  if (!any(is_use_or_import)) {
    return(cmdline)
  }

  has_clear = stringi::stri_detect_regex(
    cmd,
    "(^|[,[:space:]])clear([,[:space:]]|$)",
    case_insensitive = TRUE
  )

  add = is_use_or_import & !has_clear
  if (!any(add)) {
    return(cmdline)
  }

  has_comma = stringi::stri_detect_fixed(cmd[add], ",")

  cmd[add] = ifelse(
    has_comma,
    paste0(cmd[add], " clear"),
    paste0(cmd[add], ", clear")
  )

  cmdline[add] = cmd[add]
  cmdline
}
```
!END_MODIFICATION drf_stata_ensure_use_clear in repboxDRF/R/drf_tools.R
