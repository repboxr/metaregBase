# FILE: cmdpart_test.R

# Tests for the pure syntactic cmdpart Stata regression parser.

example = function() {
  cmdpart_unit_tests()
}

#' Run the test suite for syntactic Stata regression parsing
cmdpart_unit_tests <- function() {


  # Non-trivial Stata regression commands to test syntactic parsing
  test_cmds <- c(
    # 1. Basic OLS with if (containing parentheses), in, weights, and options
    "regress y x1 x2 if (age > 18) | (income == 500) [aw=weight_var] in 1/100, robust cluster(id)",

    # 2. IV regression with subcmd, exogenous vars, and cluster option
    "ivregress 2sls y x1 x2 (endo1 endo2 = instr1 instr2 instr3) if valid == 1, vce(cluster state)",

    # 3. High-dimensional FE with wildcards, ranges, and multiple options
    "quietly xi: reghdfe y x1-x5 i.year* i.region#c.income, absorb(id year) vce(hc1)",

    # 4. IV regression without subcmd, no exogenous variables, and probability weights
    "ivreg2 y (x1 = z1 z2 z3)[pw=prob_wt], cluster(county)"
  )

  cat("Running cmdpart syntactic parsing tests...\n")

  # Parse all commands at once using the refactored function
  # (Assumes cmdparts_of_stata_reg and cmdpart_regvar.R functions are loaded)
  parsed_df <- cmdparts_of_stata_reg(test_cmds)

  # Helper function to extract specific tokens for a given string index
  get_tokens <- function(df, str_idx, part_name, tag_name = NULL) {
    sub_df <- df[df$str_row == str_idx & df$part == part_name, ]
    if (!is.null(tag_name)) {
      sub_df <- sub_df[sub_df$tag == tag_name, ]
    }
    return(sub_df$content)
  }

  # -------------------------------------------------------------------------
  # Test Case 1: regress y x1 x2 if (age > 18) | (income == 500)[aw=weight_var] in 1/100, robust cluster(id)
  # -------------------------------------------------------------------------
  cat("  Testing Case 1 (OLS + if/in/weights)...\n")
  stopifnot(get_tokens(parsed_df, 1, "cmd") == "regress")
  stopifnot(get_tokens(parsed_df, 1, "v", "depvar") == "y")
  stopifnot(identical(get_tokens(parsed_df, 1, "v", "exo"), c("x1", "x2")))
  stopifnot(get_tokens(parsed_df, 1, "if_str") == "if (age > 18) | (income == 500)")
  stopifnot(get_tokens(parsed_df, 1, "in_str") == "in 1/100")
  stopifnot(get_tokens(parsed_df, 1, "weight_var") == "weight_var")
  stopifnot(get_tokens(parsed_df, 1, "weight_type") == "aw")

  opt_args <- get_tokens(parsed_df, 1, "opt_arg")
  opts <- get_tokens(parsed_df, 1, "opt")
  opts
  stopifnot("robust" %in% opts)
  stopifnot("cluster" %in% opts && opt_args[1] == "id")

  # -------------------------------------------------------------------------
  # Test Case 2: ivregress 2sls y x1 x2 (endo1 endo2 = instr1 instr2 instr3) if valid == 1, vce(cluster state)
  # -------------------------------------------------------------------------
  cat("  Testing Case 2 (ivregress with subcmd)...\n")
  stopifnot(get_tokens(parsed_df, 2, "cmd") == "ivregress")
  stopifnot(get_tokens(parsed_df, 2, "subcmd") == "2sls")
  stopifnot(get_tokens(parsed_df, 2, "v", "depvar") == "y")
  stopifnot(identical(get_tokens(parsed_df, 2, "v", "exo"), c("x1", "x2")))
  stopifnot(identical(get_tokens(parsed_df, 2, "v", "endo"), c("endo1", "endo2")))
  stopifnot(identical(get_tokens(parsed_df, 2, "v", "instr"), c("instr1", "instr2", "instr3")))
  stopifnot(get_tokens(parsed_df, 2, "if_str") == "if valid == 1")

  opts <- get_tokens(parsed_df, 2, "opt")
  opt_args <- get_tokens(parsed_df, 2, "opt_arg")
  stopifnot("vce" %in% opts && opt_args[opts == "vce"] == "cluster state")

  # -------------------------------------------------------------------------
  # Test Case 3: quietly xi: reghdfe y x1-x5 i.year* i.region#c.income, absorb(id year) vce(hc1)
  # -------------------------------------------------------------------------
  cat("  Testing Case 3 (reghdfe with prefixes, globs, ranges)...\n")

  # Check prefixes (quietly, xi)

  pre_tags <- parsed_df$tag[parsed_df$str_row == 3 & parsed_df$part == "pre"]
  stopifnot("quietly" %in% pre_tags && "xi" %in% pre_tags)
  parsed_df[parsed_df$str_row == 3 & parsed_df$part == "pre",]

  stopifnot(get_tokens(parsed_df, 3, "cmd") == "reghdfe")
  stopifnot(get_tokens(parsed_df, 3, "v", "depvar") == "y")

  # Ensure globs and ranges are retained natively (no data resolution needed yet)
  exo_vars <- get_tokens(parsed_df, 3, "v", "exo")
  stopifnot(identical(exo_vars, c("x1-x5", "i.year*", "i.region#c.income")))

  opts <- get_tokens(parsed_df, 3, "opt")
  opt_args <- get_tokens(parsed_df, 3, "opt_arg")
  stopifnot("absorb" %in% opts && opt_args[opts == "absorb"] == "id year")
  stopifnot("vce" %in% opts && opt_args[opts == "vce"] == "hc1")

  # -------------------------------------------------------------------------
  # Test Case 4: ivreg2 y (x1 = z1 z2 z3) [pw=prob_wt], cluster(county)
  # -------------------------------------------------------------------------
  cat("  Testing Case 4 (ivreg2 without subcmd, pure IV)...\n")
  stopifnot(get_tokens(parsed_df, 4, "cmd") == "ivreg2")
  stopifnot(length(get_tokens(parsed_df, 4, "subcmd")) == 0) # No subcmd expected
  stopifnot(get_tokens(parsed_df, 4, "v", "depvar") == "y")

  # Should have 0 exogenous variables
  stopifnot(length(get_tokens(parsed_df, 4, "v", "exo")) == 0)

  stopifnot(get_tokens(parsed_df, 4, "v", "endo") == "x1")
  stopifnot(identical(get_tokens(parsed_df, 4, "v", "instr"), c("z1", "z2", "z3")))

  stopifnot(get_tokens(parsed_df, 4, "weight_var") == "prob_wt")
  stopifnot(get_tokens(parsed_df, 4, "weight_type") == "pw")

  opts <- get_tokens(parsed_df, 4, "opt")
  opt_args <- get_tokens(parsed_df, 4, "opt_arg")
  stopifnot("cluster" %in% opts && opt_args[opts == "cluster"] == "county")

  cat("\nAll syntactic cmdpart tests passed successfully! Phase 1 logic is sound.\n")
}
