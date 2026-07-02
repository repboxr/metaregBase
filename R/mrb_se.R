# Parses SE information from a Stata regression and
# returns the relevant SE info stored in the repdb reg table

get_se_parser_version = function() {
  return(0)
}

se_cmdpart_first = function(cmdpart, part) {
  if (is.null(cmdpart) || NROW(cmdpart) == 0) {
    return(NA_character_)
  }

  rows = which(cmdpart$part == part)
  if (length(rows) == 0) {
    return(NA_character_)
  }

  trimws(as.character(cmdpart$content[rows[1]]))
}


se_cmdpart_has_prefix = function(cmdpart, prefix) {
  if (is.null(cmdpart) || NROW(cmdpart) == 0) {
    return(FALSE)
  }

  rows = which(cmdpart$part == "pre")
  if (length(rows) == 0) {
    return(FALSE)
  }

  pre = tolower(trimws(as.character(cmdpart$content[rows])))
  any(startsWith(pre, tolower(prefix)))
}


se_stata_is_abbr = function(x, full, min_chars=1L) {
  x = tolower(trimws(as.character(x)))

  !is.na(x) &
    nchar(x) >= min_chars &
    startsWith(rep(full, length(x)), x)
}


se_stata_words = function(x) {
  if (length(x) == 0 || is.na(x[1])) {
    return(character(0))
  }

  x = trimws(as.character(x[1]))
  if (!nzchar(x)) {
    return(character(0))
  }

  # Commas inside vce() separate modifiers such as dfadjust.
  x = gsub(",", " ", x, fixed=TRUE)
  x = ws_to_single_space(x)

  strsplit(x, " ", fixed=TRUE)[[1]]
}


se_collapse_words = function(x) {
  x = as.character(x)
  x = x[!is.na(x) & nzchar(x)]

  if (length(x) == 0) {
    return("")
  }

  paste0(x, collapse=" ")
}


se_combine_args = function(...) {
  x = unlist(list(...), use.names=FALSE)
  x = as.character(x)
  x = x[!is.na(x) & nzchar(x)]

  if (length(x) == 0) {
    return("")
  }

  paste0(x, collapse=";")
}


se_stata_to_repdb = function(cmd, opts_df = cmdpart_to_opts_df(cmdpart), cmdpart=NULL, panelvar=NA_character_) {

  restore.point("se_stata_to_repdb")

  cmd = tolower(trimws(cmd))

  if (is.null(opts_df) || NROW(opts_df) == 0) {
    opts_df = tibble(
      opt = character(0),
      opt_arg = character(0)
    )
  }

  if (!"opt" %in% names(opts_df)) {
    opts_df$opt = character(NROW(opts_df))
  }
  if (!"opt_arg" %in% names(opts_df)) {
    opts_df$opt_arg = rep(NA_character_, NROW(opts_df))
  }

  opts_df$opt = tolower(trimws(as.character(opts_df$opt)))
  opts_df$opt_arg = as.character(opts_df$opt_arg)

  # newey reports Newey-West HAC standard errors. lag(0) corresponds
  # to heteroskedasticity-robust standard errors without serial lags.
  if (cmd == "newey") {
    lag_row = which(se_stata_is_abbr(opts_df$opt, "lag", min_chars=3L))

    if (length(lag_row) == 1) {
      lag = as_integer(opts_df$opt_arg[lag_row])
    } else {
      lag = NA_integer_
    }

    se = tibble(
      se_category = "robust",
      se_type = "nw",
      se_args = paste0("lag=", lag)
    )
    return(se)
  }

  abbr.li = list(
    robust = c("robust", "robus", "robu", "rob", "ro", "r"),
    unadjusted = c(
      "unadjusted", "unadjuste", "unadjust", "unadjus",
      "unadju", "unadj", "unad", "una", "un"
    ),
    conventional = c(
      "conventional", "conventiona", "convention", "conventio",
      "conventi", "convent", "conven", "conve", "conv"
    ),
    iid = "iid",
    ols = "ols",
    oim = "oim",
    opg = "opg",
    cluster = c("cluster", "cluste", "clust", "clus", "clu", "cl"),
    bootstrap = c(
      "bootstrap", "bootstra", "bootstr", "bootst", "boots", "boot"
    ),
    jackknife = c("jackknife", "jackknif", "jack"),
    hc0 = "hc0",
    hc1 = "hc1",
    hc2 = "hc2",
    hc3 = "hc3",
    hc4 = "hc4",
    hc5 = "hc5",
    hac = "hac",
    dkraay = "dkraay"
  )

  canonical_se_type = function(x) {
    if (length(x) == 0 || is.na(x[1])) {
      return("")
    }

    x = tolower(trimws(as.character(x[1])))

    matches = names(abbr.li)[vapply(
      abbr.li,
      function(values) x %in% values,
      logical(1)
    )]

    if (length(matches) == 1) {
      return(matches[1])
    }

    # Preserve unknown VCE names for the final fallback.
    x
  }

  se_type = ""
  se_args = character(0)
  se_source = ""

  # Survey and multiple-imputation standard errors depend on additional
  # design or imputation information. Leave these unknown here.
  if (se_cmdpart_has_prefix(cmdpart, "svy")) {
    se_type = "svy"
  } else if (se_cmdpart_has_prefix(cmdpart, "mi estimate")) {
    se_type = "mi"
  } else if (se_cmdpart_has_prefix(cmdpart, "bootstrap")) {
    se_type = "bootstrap"
  } else if (se_cmdpart_has_prefix(cmdpart, "jackknife")) {
    se_type = "jackknife"
  }

  # First look for an explicit vce() option.
  if (!nzchar(se_type)) {
    vce_row = which(opts_df$opt == "vce")

    if (length(vce_row) > 1) {
      repbox_problem(
        "Multiple vce() options were found. Use the first one.",
        "multiple_vce_options",
        fail_action="msg"
      )
      vce_row = vce_row[1]
    }

    if (length(vce_row) == 1) {
      se_words = se_stata_words(opts_df$opt_arg[vce_row])

      if (length(se_words) == 0) {
        se_type = "vce"
      } else {
        se_type = canonical_se_type(se_words[1])
        se_args = se_words[-1]
        se_source = "vce"
      }
    }
  }

  # Some Stata commands allow robust or cluster as standalone options.
  if (!nzchar(se_type)) {
    standalone_types = c(
      "robust", "cluster", "bootstrap", "jackknife"
    )
    standalone_abbr = unlist(
      abbr.li[standalone_types],
      use.names=FALSE
    )

    se_rows = which(opts_df$opt %in% standalone_abbr)

    if (length(se_rows) > 0) {
      candidate_types = vapply(
        opts_df$opt[se_rows],
        canonical_se_type,
        character(1)
      )

      # If robust and cluster are both present, cluster determines the
      # dependence structure of the VCE.
      if (
        "cluster" %in% candidate_types &&
        all(candidate_types %in% c("robust", "cluster"))
      ) {
        row = se_rows[which(candidate_types == "cluster")[1]]
        se_type = "cluster"
        se_args = se_stata_words(opts_df$opt_arg[row])
        se_source = "standalone"

      } else if (length(unique(candidate_types)) == 1) {
        row = se_rows[1]
        se_type = candidate_types[1]
        se_args = se_stata_words(opts_df$opt_arg[row])
        se_source = "standalone"

      } else {
        se_type = paste0(unique(candidate_types), collapse="+")
        se_args = opts_df$opt_arg[se_rows]

        repbox_problem(
          paste0(
            "Regression options match multiple standard error types: ",
            se_collapse_words(unique(candidate_types))
          ),
          "multiple_se_options",
          fail_action="msg"
        )
      }
    }
  }

  # Determine command-specific defaults when no VCE was specified.
  if (!nzchar(se_type)) {
    weight_type = tolower(se_cmdpart_first(cmdpart, "weight_type"))
    has_pweight = !is.na(weight_type) &&
      weight_type %in% c("pweight", "pw")

    if (has_pweight) {
      # For standard estimation commands, probability weights imply a
      # sandwich VCE. Keep the generic robust label because this is not
      # necessarily identical to unweighted OLS HC1.
      se_type = "robust"
      se_source = "pweight"

    } else if (cmd == "ivregress" | cmd=="ivreg") {
      subcmd = tolower(se_cmdpart_first(cmdpart, "subcmd"))

      if (cmd == "ivreg") {
        se_type = "unadjusted"
        se_source = "default"
      } else {
        # Sometimes the estimator is parsed as a "pre" or "v" token instead of "subcmd"
        if (is.na(subcmd)) {
          cand = tolower(as.character(cmdpart$content[cmdpart$part %in% c("pre", "v")]))
          found = intersect(c("2sls", "liml", "gmm"), cand)
          if (length(found) > 0) {
            subcmd = found[1]
          }
        }

        if (!is.na(subcmd) && subcmd %in% c("2sls", "liml")) {
          # ivregress 2sls and liml default to vce(unadjusted).
          se_type = "unadjusted"
          se_source = "default"

        } else if (!is.na(subcmd) && subcmd == "gmm") {
          wmatrix_row = which(
            se_stata_is_abbr(opts_df$opt, "wmatrix", min_chars=2L)
          )

          if (length(wmatrix_row) > 0) {
            wmatrix_words = se_stata_words(
              opts_df$opt_arg[wmatrix_row[1]]
            )

            if (length(wmatrix_words) == 0) {
              se_type = "wmatrix"
            } else {
              se_type = canonical_se_type(wmatrix_words[1])
              se_args = wmatrix_words[-1]
              se_source = "wmatrix"
            }
          } else {
            # The default GMM weighting matrix and VCE are robust.
            se_type = "robust"
            se_source = "default"
          }

        } else {
          # Default to unadjusted (2SLS) if we cannot identify the subcommand cleanly
          se_type = "unadjusted"
          se_source = "default"
        }
      }
    } else {
      iid_default_cmds = c(
        "reg","regr","regre","regres",
        "regress",
        "areg",
        "xtreg",
        "xtivreg",
        "reghdfe",
        "ivreghdfe",
        "ivreg2",
        "xtivreg2"
      )

      oim_default_cmds = c(
        "logit",
        "logistic",
        "probit",
        "clogit",
        "cloglog",
        "poisson",
        "nbreg",
        "mlogit",
        "ologit",
        "oprobit",
        "tobit",
        "intreg",
        "truncreg",
        "heckman",
        "glm",
        "xtlogit",
        "xtprobit",
        "xtpoisson",
        "xtnbreg"
      )

      if (cmd %in% iid_default_cmds) {
        # "iid" is used here as the RepDB category for conventional or
        # model-based VCEs. For panel estimators this need not mean that
        # all observation-level errors are literally IID.
        se_type = "iid"
        se_source = "default"

      } else if (cmd %in% oim_default_cmds) {
        # Most maximum-likelihood commands default to the inverse observed
        # information matrix rather than a sandwich VCE.
        se_type = "oim"
        se_source = "default"

      } else {
        # Stata defaults differ across commands. Do not silently assume
        # conventional standard errors for an unimplemented command.
        se_type = "default"
        se_args = paste0("cmd=", cmd)
      }
    }
  }

  has_small = (cmd == "ivregress" | cmd=="ivreg") && any(opts_df$opt == "small")
  small_arg = if (has_small) "small=true" else character(0)

  # Conventional or model-based VCEs.
  if (se_type %in% c(
    "iid", "unadjusted", "conventional", "ols", "oim", "opg"
  )) {
    if (length(se_args) == 0) {
      out_type = se_type

      if (se_type %in% c(
        "iid", "unadjusted", "conventional", "ols"
      )) {
        out_type = "iid"
      }

      se = tibble(
        se_category = "iid",
        se_type = out_type,
        se_args = se_combine_args(small_arg)
      )
      return(se)
    }

    repbox_problem(
      paste0(
        "Problem in parsing se: se_type is ", se_type,
        " but there are se_args: ",
        paste0(se_args, collapse=", ")
      ),
      "se_args",
      fail_action="msg"
    )
  }

  # Plain heteroskedasticity-robust VCE.
  if (se_type == "robust") {
    if (length(se_args) == 0) {
      xt_robust_cluster_cmds = c(
        "xtreg",
        "xtivreg",
        "xtlogit",
        "xtprobit",
        "xtpoisson",
        "xtnbreg",
        "xttobit",
        "xtcloglog",
        "xtgee"
      )

      if (cmd %in% xt_robust_cluster_cmds) {
        # For these xt commands, vce(robust) clusters on the panel
        # identifier; it is not an observation-level HC estimator.
        if (!is.na(panelvar) && nzchar(panelvar)) {
          se = tibble(
            se_category = "cluster",
            se_type = "cluster",
            se_args = se_combine_args(
              paste0("cluster1=", panelvar),
              small_arg
            )
          )
          return(se)
        }

        se_args = "implicit_cluster_var=missing"

      } else if (cmd == "clogit") {
        group_row = which(
          se_stata_is_abbr(opts_df$opt, "group", min_chars=2L) |
            se_stata_is_abbr(opts_df$opt, "strata", min_chars=3L)
        )

        if (length(group_row) > 0) {
          groupvar = trimws(opts_df$opt_arg[group_row[1]])
        } else {
          groupvar = NA_character_
        }

        # clogit, vce(robust) clusters on the variable in group().
        if (!is.na(groupvar) && nzchar(groupvar)) {
          se = tibble(
            se_category = "cluster",
            se_type = "cluster",
            se_args = se_combine_args(
              paste0("cluster1=", groupvar),
              small_arg
            )
          )
          return(se)
        }

        se_args = "implicit_group_var=missing"

      } else {
        out_type = "robust"

        # Explicit vce(robust) for regress and areg is the usual HC1
        # covariance estimator with Stata's finite-sample correction.
        # A pweight-induced VCE is kept as generic robust.
        if (
          cmd %in% c("regress", "areg") &&
          se_source %in% c("vce", "standalone")
        ) {
          out_type = "hc1"
        }

        se = tibble(
          se_category = "robust",
          se_type = out_type,
          se_args = se_combine_args(small_arg)
        )
        return(se)
      }

    } else {
      repbox_problem(
        paste0(
          "Problem in parsing se: se_type is robust",
          " but there are se_args: ",
          paste0(se_args, collapse=", ")
        ),
        "se_args",
        fail_action="msg"
      )
    }
  }

  # Explicit HC estimators.
  if (se_type %in% c("hc0", "hc1", "hc2", "hc3", "hc4", "hc5")) {
    arg_lower = tolower(as.character(se_args))

    is_dfadjust = se_stata_is_abbr(
      arg_lower,
      "dfadjust",
      min_chars=3L
    )
    is_hansen = se_stata_is_abbr(
      arg_lower,
      "hansen",
      min_chars=3L
    )

    modifier_args = character(0)

    if (any(is_dfadjust)) {
      modifier_args = c(modifier_args, "dfadjust=true")
    }
    if (any(is_hansen)) {
      # Stata's Hansen adjustment also incorporates the degrees-of-freedom
      # adjustment.
      modifier_args = c(
        modifier_args,
        "hansen=true",
        "dfadjust=true"
      )
    }

    modifier_args = unique(modifier_args)
    cluster_vars = se_args[!(is_dfadjust | is_hansen)]
    cluster_vars = cluster_vars[
      !is.na(cluster_vars) & nzchar(cluster_vars)
    ]

    if (se_type %in% c("hc2", "hc3")) {
      # xtreg, vce(hc2) and vce(hc3) implicitly use the panel variable
      # as the clustering variable.
      if (
        cmd == "xtreg" &&
        length(cluster_vars) == 0 &&
        !is.na(panelvar) &&
        nzchar(panelvar)
      ) {
        cluster_vars = panelvar
      }

      if (length(cluster_vars) == 1) {
        # vce(hc2 clustvar) is essentially a CR2 cluster-robust VCE with
        # a leverage adjustment. vce(hc3 clustvar) is the corresponding
        # more conservative CR3-style estimator.
        se = tibble(
          se_category = "cluster",
          se_type = se_type,
          se_args = se_combine_args(
            paste0("cluster1=", cluster_vars),
            modifier_args
          )
        )
        return(se)
      }

      if (length(cluster_vars) == 0 && cmd != "xtreg") {
        # Without a clustering variable, HC2 and HC3 are ordinary
        # leverage-adjusted heteroskedasticity-robust estimators.
        se = tibble(
          se_category = "robust",
          se_type = se_type,
          se_args = se_combine_args(modifier_args)
        )
        return(se)
      }

      if (
        cmd == "xtreg" &&
        length(cluster_vars) == 0 &&
        (is.na(panelvar) || !nzchar(panelvar))
      ) {
        se_args = se_combine_args(
          "implicit_cluster_var=missing",
          modifier_args
        )
      } else if (length(cluster_vars) > 1) {
        repbox_problem(
          paste0(
            se_type,
            " was parsed with multiple possible cluster variables: ",
            paste0(cluster_vars, collapse=", ")
          ),
          "hc_cluster_args",
          fail_action="msg"
        )
      }

    } else if (length(se_args) == 0) {
      se = tibble(
        se_category = "robust",
        se_type = se_type,
        se_args = ""
      )
      return(se)

    } else {
      repbox_problem(
        paste0(
          "Problem in parsing se: se_type is ", se_type,
          " but there are se_args: ",
          paste0(se_args, collapse=", ")
        ),
        "se_args",
        fail_action="msg"
      )
    }
  }

  # Ordinary one-way or multiway cluster-robust VCE.
  if (se_type == "cluster") {
    clustervar = as.character(se_args)
    clustervar = clustervar[
      !is.na(clustervar) & nzchar(clustervar)
    ]

    num_clustervar = length(clustervar)

    if (num_clustervar == 0) {
      repbox_problem(
        paste0(
          "We have clustered se of type ", se_type,
          " but no cluster variables can be found in option."
        ),
        "se_cluster_args",
        fail_action="msg"
      )

    } else {
      # Several variables in vce(cluster ...) request multiway clustering,
      # not clustering on the interaction of those variables.
      if (num_clustervar == 1) {
        out_type = "cluster"
      } else if (num_clustervar == 2) {
        out_type = "twoway"
      } else {
        out_type = "multiway"
      }

      cluster_args = paste0(
        "cluster",
        seq_along(clustervar),
        "=",
        clustervar
      )

      se = tibble(
        se_category = "cluster",
        se_type = out_type,
        se_args = se_combine_args(
          cluster_args,
          small_arg
        )
      )
      return(se)
    }
  }

  # HAC estimators allow heteroskedasticity and serial correlation.
  # Preserve the kernel, lag, and bandwidth specification as raw arguments.
  if (se_type %in% c("hac", "dkraay")) {
    hacspec = se_collapse_words(se_args)

    if (nzchar(hacspec)) {
      se = tibble(
        se_category = "robust",
        se_type = se_type,
        se_args = paste0("hacspec=", hacspec)
      )
      return(se)
    }
  }

  # Ensure that unknown arguments produce one tibble row.
  se_args = se_collapse_words(se_args)

  repbox_problem(paste0("We have not yet implemented parsing of Stata standard error of type ", se_type,". Set to unknown category."), "unkown_se_type","msg")
  se = tibble(
    se_category = "unknown",
    se_type = se_type,
    se_args = se_args
  )
  return(se)
}

repdb_parse_se_args = function(se_args, as_df=FALSE) {
  restore.point("repdb_parse_se_args")
  #se_args = c("cluster1=i1;cluster2=i2","cluster2=i2")[1]
  if (length(se_args)>1) {
    stop("repdb_se_args_to_list is not yet vectorized.")
  }

  str = strsplit(se_args,";", fixed=TRUE)[[1]]
  eq_pos = stri_locate_first_fixed(str,"=")[,1]
  var = substring(str,1,eq_pos-1)
  val = substring(str,eq_pos+1)
  if (!as_df) {
    names(val) = var
    return(val)
  }
  tibble(arg_name=var, arg_val=val)
}
