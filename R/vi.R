# Create variable information as mainly stored in regvar table

vi.from.stata.reg = function(reg, dat) {
  # TO DO:
  # 1. * is possible used as abbreviation
  #          and as interaction expansion.
  #
  # https://www.stata.com/manuals/rxi.pdf
  # https://www.stata.com/manuals/u11.pdf#u11.4varnameandvarlists
  #
  # Alternatives to interaction expansion are # and ## and |

  # 2. One can also use ~ instead of * but gets an error
  #    if more than one variable is matched...


  restore.point("vi.from.stata.reg")
  cols = colnames(dat)


  exo_parts = reg$exo_parts[[1]]
  endo_parts = reg$endo_parts[[1]]
  instr_parts = reg$instr_parts[[1]]

  exo_parts = expand.stata.var.patterns(exo_parts, cols, uses_xi=reg$uses_xi, unlist=TRUE)
  endo_parts = expand.stata.var.patterns(endo_parts, cols, uses_xi=reg$uses_xi, unlist=TRUE)
  instr_parts = expand.stata.var.patterns(instr_parts, cols, uses_xi=reg$uses_xi, unlist=TRUE)

  depvar = expand.stata.var.patterns(reg$depvar, cols, uses_xi=FALSE)

  vi = tibble(
    ia_expr = c(depvar, exo_parts,endo_parts, instr_parts)) %>%
    mutate(
      main_pos = seq_len(n()),
      is_ia = grepl("(\\|)|(#)|(\\*)",ia_expr),
      role = c("dep", rep("exo",length(exo_parts)),rep("endo",length(endo_parts)), rep("instr",length(instr_parts))),
      option = ""
    )

  # Add absorb(var) option from areg
  if (reg$cmd %in% c("areg","reghdfe")) {
    opts.df = reg$opts.df[[1]]
    # Update "a" is an abbreviation for absorb
    if (reg$cmd %in% c("areg", "reghdfe")) {
      row = which(opts.df$opt %in% c("absorb","a","ab","abs","abso","absor"))
    } else {
      row = which(opts.df$opt=="absorb")
    }
    if (length(row)>0) {
      vars = opts.df$opt_args[row[1]] %>% shorten.spaces() %>% strsplit(" ", fixed=TRUE)
      vars = vars[[1]]
      vi = bind_rows(vi, tibble(ia_expr = vars, main_pos = NROW(vi)+1:length(vars), is_ia=FALSE,role="exo", option="absorb"))
    }
  }

  # FE xtreg
  if (reg$cmd %in% c("xtreg") & "fe" %in% reg$opts.df[[1]]$opt) {
    fe.var = reg$panelvar
    if (is_empty(fe.var)) {
      #note_problem(problem_type="xtreg_fe_no_panelvar","xtreg specifies fe but no panelvar is set",stop = FALSE )
      stop("xtreg specifies fe but no panelvar is set")
    } else {
      vi = bind_rows(vi, tibble(ia_expr = fe.var, main_pos = NROW(vi)+1, is_ia=FALSE,role="exo", option="fe"))
    }
  }

  # Add cluster se variables
  clustervar = reg$se.info[[1]]$clustervar[[1]]
  if (length(clustervar)>0) {
    clustervar = expand.stata.var.patterns(clustervar, cols, uses_xi=FALSE)
    vi = bind_rows(vi, tibble(ia_expr = clustervar, main_pos = seq_along(clustervar), is_ia=FALSE,role="cluster", option="se"))
  }

  # Add weights var
  weights_var = reg$weights_var
  if (!is_empty(weights_var)) {
    weights_var = expand.stata.var.patterns(weights_var, cols, uses_xi=FALSE)
    vi = bind_rows(vi, tibble(ia_expr = reg$weights_var, main_pos = 1, is_ia=FALSE,role="weight", option=""))
  }

  # Expand interaction effects
  rows = which(vi$is_ia)
  vi$var_expr = as.list(vi$ia_expr)
  vi$var_expr[rows] = strsplit(vi$ia_expr[rows],"(##)|(#)|(\\|)|(\\*)")
  vi = unnest(vi, var_expr) %>%
    group_by(ia_expr) %>%
    mutate(
      ia_num = n(),
      ia_pos = 1:n()
    ) %>%
    ungroup()


  cols_info = make_cols_small_info(dat)


  # Split L1.x into prefix = L1; var = x
  #   L1.S2.x into prefix = L1S2 var = x
  prefix.start = stri_locate_last_fixed(vi$var_expr,".")[,1]

  vi$prefix = ifelse(is.na(prefix.start),
    "",
    stri_sub(vi$var_expr,1,prefix.start-1) %>%
      stri_replace_all_fixed(".","")
  )
  vi$var = ifelse(is.na(prefix.start),
    vi$var_expr,
    stri_sub(vi$var_expr,prefix.start+1)
  )

  vi = vi %>%
    mutate(
      prefix = case_when(
        # Transforms e.g. ib2005.year into equivalent code b2005.year
        # to always have a single digit prefix type
        startsWith(tolower(prefix),"ib") ~ paste0("b", substring(prefix,3)),
        TRUE ~ prefix
      )
    ) %>%
    left_join(cols_info, by=c("var"="col")) %>%
    mutate(
      is_factor = class %in% c("character","factor"),
      fe_type = case_when(
        prefix %in% c("i","I") ~ "i",
        startsWith(prefix, "b") ~ "b",
        # in x1#x2 x1 and x2 are always treated as factor
        # in x1#c.x2 x2 is treated as continuous
        has.substr(ia_expr,"#") & !startsWith(prefix,"c") ~ "#",
        option %in% c("absorb","fe") ~ option,
        is_factor ~ class,
        TRUE ~ ""
      ),
      absorbed_fe = fe_type %in% c("absorb","fe"),
      is_fe = fe_type != "",
      varclass = class,
      class = ifelse(is_fe & !is_factor, "fe", class),
      # Note this might not work correctly for rare cases like x1#x2##x3
      # where only a subset of main effects is included. But that is likely rare.
      add_main_effects = is_ia & (has.substr(ia_expr,"##") | has.substr(ia_expr, "*"))
    ) %>%
    #select(-x_star, -has_star) %>%
    select(ia_expr, var_expr, var, role, prefix, option, class, fe_type, is_fe, distinct_num, ia_num, ia_pos,main_pos, everything())

  # Create canoncial variable names
  vi$ia_cterm = stata_expr_to_cterm(vi$ia_expr)
  vi$cterm = stata_expr_to_cterm(vi$var_expr)
  vi$basevar = stata_expr_to_cterm(vi$var)

  # Deals with specifications like
  # i2005.year which has cterm: year=2005
  vi$class = ifelse(has.substr(vi$cterm,"="),"dummy",vi$class)

  # Add type of interaction effect
  vi = vi.add.ia.type(vi)

  # Used by repdb and colstat
  vi = vi %>% mutate(
    var_org_type = varclass %>% change_val(c("fe","character"),"factor"),
    var_reg_type = class %>% change_val(c("fe","character"),"factor") %>% change_val("logical","dummy"),
    ia_reg_type = ia_type %>%
      change_val("fe","factor") %>%
      change_val("fe_numeric","factor_numeric") %>%
      change_val("fe_logical","factor_dummy")
    ) %>%
    mutate(
      var_reg_type = ifelse(role=="cluster", "factor", var_reg_type),
      ia_reg_type = ifelse(role=="cluster", "factor", ia_reg_type)
    )

  #vi$omit = tolower(vi$prefix) == "o"
  vi
}


# Characterize the type of interaction terms
vi.add.ia.type = function(vi) {
  # TO DO: Specify whether in interaction A*B also A and B
  #        should be included or not.
  vi %>%
    group_by(is_ia, ia_cterm, role, ia_num, main_pos) %>%
    arrange(desc(is_fe), desc(class=="dummy")) %>%
    mutate(
      ia_distinct_num = prod(distinct_num),
      ia_type = case_when(
        all(!is_fe & class=="dummy") ~ "dummies",
        all(!is_fe) ~ "numeric",
        all(is_fe | class=="dummy") ~ "fe",
        ia_num == 2 & class[1] == "dummy" & (!is_fe[2] & class[2] != "dummy") ~ "dummy_numeric",
        ia_num == 2 & is_fe[1] & !is_fe[2] ~ "fe_numeric",
        TRUE ~ "unknown"
      )
    ) %>%
    ungroup()
}


vi.add.main.effects = function(vi) {
  restore.point("vi.add.main.effects")

  if (!any(vi$add_main_effects)) return(vi)
  vi.li = split(vi,vi$ia_cterm)
  bind_rows(lapply(vi.li, make.vi.of.ia.main.effects))
}

make.vi.of.ia.main.effects = function(vi.of.ia) {
  restore.point("make.vi.of.ia.main.effects")

  if (NROW(vi.of.ia)<2) return(vi.of.ia)
  if (!any(vi.of.ia$add_main_effects)) return(vi.of.ia)

  vi.of.ia$org_ia_pos = vi.of.ia$ia_pos

  if (NROW(vi.of.ia)==2) {
    me = tibble(
      main_effect_id = c("1", "2"),
      ia_num = c(1, 1),
      org_ia_pos = c(1, 2)
    )

  } else if (NROW(vi.of.ia)==3) {
    me = tibble(
      main_effect_id = c("12","12", "13","13", "23","23", "01", "02", "03"),
      ia_num = c(2,2, 2,2, 2,2, 1, 1, 1),
      org_ia_pos = c(1,2, 1,3, 2,3, 1, 2, 3)
    )
  } else if (NROW(via.of.ia) > 3) {
    stop("Cannot yet handle more than tripple interactions.")
  }
  me = me %>%
    left_join(vi.of.ia %>% rename(org_ia_cterm = ia_cterm) %>% select( - ia_num), by="org_ia_pos") %>%
    group_by(main_effect_id) %>%
    mutate(
      ia_cterm = paste0(cterm, collapse="#")
    ) %>%
    ungroup() %>%
    select(-main_effect_id)

  bind_rows(vi.of.ia, me)

}

