# Parses SE information from a Stata regression and
# returns the relevant SE info stored in the repdb reg table

get_se_parser_version = function() {
  return(0)
}



se_stata_to_repdb = function(cmd, opts_df = cmdpart_to_opts_df(cmdpart), cmdpart=NULL) {
  restore.point("se_stata_to_repdb")

  if (cmd == "newey") {
    row = opts_df$opt == "lag"
    lag = as_integer(opts_df$opt_arg[row])
    se = tibble(
      se_category = "robust",
      se_type = "nw",
      se_args = paste0("lag=",lag)
    )
    return(se)
  }

  abbr.li = list(
    robust = c("robust","robus","robu","rob","ro","r"),
    cluster = c("cluster","cluste","clust","clus","clu","cl"),
    boot = c("bootstrap","bootstra","bootstr","bootst","boots","boot"),
    jack = c("jackknife","jackknif","jack")
  )

  se_type = ""; se_args=NULL
  vce_row = which(opts_df$opt=="vce")

  if (length(vce_row)>0) {
    se_str = opts_df$opt_arg[vce_row]
    if (is.na(se_str)) se_str = ""
    se_words = se_str %>%
      trimws() %>% ws_to_single_space() %>%
      strsplit(" ")
    se_words = se_words[[1]]
    if (length(se_words) > 0 && se_words[1] != "") {
      se_type = expand_stata_abbr_one_val(se_words[1], abbr.li)
      se_args = se_words[-1]
    } else {
      se_type = ""
      se_args = character(0)
    }
  } else {
    abbr.row = which(opts_df$opt %in% unlist(abbr.li))
    if (length(abbr.row)==2) {
      cl_ind = which(startsWith(opts_df$opt[abbr.row],"cl"))
      if (length(cl_ind)>0) {
        abbr.row = abbr.row[cl_ind]
      }
    }

    if (length(abbr.row)==1) {
      se_type = opts_df$opt[[abbr.row]]
      se_type = expand_stata_abbr_one_val(se_type, abbr.li)
      se_str = opts_df$opt_arg[abbr.row]
      if (is.na(se_str)) se_str = ""
      se_args = se_str %>%
        trimws() %>% ws_to_single_space() %>%
        strsplit(" ")
      se_args = se_args[[1]]
    } else if (length(abbr.row)>1) {
      stop("Regression options match multiple standard error abbreviations. Need to adapt stata.reg.se.info")
    }
  }

  if (cmd %in% c("xtreg", "xtivreg")) {
    if (se_type == "conventional") se_type = "iid"
  } else if (cmd %in% c("reghdfe", "ivreghdfe", "xtivreg2")) {
    if (startsWith(se_type,"un")) se_type = "iid"
  }

  if (se_type %in% c("","iid")) {
    if (length(se_args)>0) {
      restore.point("Problem in parsing se: se_type is iid but there are se_args")
      stop("Problem in parsing se: se_type is iid but there are se_args")
    }
    se = tibble(
      se_category = "iid",
      se_type = "iid",
      se_args = ""
    )
    return(se)
  }

  if (se_type=="robust" | tolower(se_type) %in% c("hc0", "hc1","hc2","hc3","hc4","hc5")) {
    if (length(se_args)>0) {
      restore.point("Problem in parsing se: se_type is robust but there are se_args")
      stop(paste0("Problem in parsing se: se_type is ", se_type," but there are se_args"))
    }
    if (se_type=="robust") se_type = "hc1"
    se = tibble(
      se_category = "robust",
      se_type = se_type,
      se_args = ""
    )
    return(se)
  }

  if (se_type=="cluster") {
    # FIX: Don't wrap in list() otherwise paste0 creates literal c("i", "year") strings
    clustervar = as.character(se_args)
    clustervar = clustervar[clustervar != ""]
    num_clustervar = length(clustervar)

    if (num_clustervar==1) {
      se_type = "cluster"
    } else if (num_clustervar==2) {
      se_type = "twoway"
    } else if (num_clustervar > 2) {
      se_type = "multiway"
    } else {
      stop("We have clustered se but no cluster variables can be found in options.")
    }
    se_args = paste0("cluster",seq_along(clustervar),"=", clustervar, collapse=";")

    se = tibble(
      se_category = "cluster",
      se_type = se_type,
      se_args = se_args
    )
    return(se)
  }

  stop(paste0("Have not yet implemented parsing of Stata standard error of type ", se_type))
  return(NULL)
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
