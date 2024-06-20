### Functions for formatting results in Latex syntax

#' Get Latex for baseline variable descriptive summaries
#' 
#' @param descrip Dataframe with descriptive summary
make_baseline_descrip_latex <- function(descrip, keep_vars=NULL) {
  descrip_latex <- descrip |>
    mutate(latex = sprintf("%d & %0.3g (%0.3g) & [%0.3g, %0.3g] & --- & ---",
                           n, mean, sd, min, max)) |>
    select(group, all_of(keep_vars), var, latex) |>
    pivot_wider(names_from = group,
                values_from = latex,
                names_sep = '_',
                names_vary = 'slowest') |>
    mutate(latex = str_c(`1`, `2`, sep=' & ')) |>
    select(var, all_of(keep_vars), latex)
  return(descrip_latex)
}

#' Get Latex for descriptive summaries of variables at 4-week, 8-week, 12-week timetpoints
#' 
#' @param descrip Dataframe with descriptive summary
make_timept_descrip_latex <- function(descrip, keep_vars=NULL) {
  delta_descrip_fmted <- descrip |>
    select(-n) |>
    filter(str_detect(var, '^delta_')) |>
    mutate(var = str_remove(var, '^delta_')) |>
    rename_with(~str_c('delta', .x, sep='_'), .cols=c(mean, sd, min, max))
  
  descrip_latex <- descrip |> 
    inner_join(delta_descrip_fmted, by=c('group', 'var', keep_vars)) |>
    mutate(latex = sprintf("%d & %0.3g (%0.3g) & [%0.3g, %0.3g] & %0.3g (%0.3g) & [%0.3g, %0.3g]",
                           n, mean, sd, min, max, 
                           delta_mean, delta_sd, delta_min, delta_max)) |>
    select(group, all_of(keep_vars), var, latex) |>
    pivot_wider(names_from = group,
                values_from = latex,
                names_sep = '_',
                names_vary = 'slowest') |>
    mutate(latex = str_c(`1`, `2`, sep=' & ')) |>
    select(var, all_of(keep_vars), latex)
}

#' Get Latex for treatment effect estimates
#' 
#' @param res_reg Dataframe with regression analysis results
#' @param res_ttest If not NULL, dataframe with t-test results
#' @param keep_vars List of variables to keep in the output 
#' @param intxn Boolean flag for whether or not interaction terms were present in analysis
make_timept_est_latex <- function(res_reg, res_ttest=NULL, keep_vars=NULL, intxn=FALSE) {
  res_reg <- res_reg |>
    mutate(latex_reg = sprintf("%.3g (%.3g; [%.3g, %.3g])",
                               beta, beta_pval, beta_q025, beta_q975))
  if (intxn) {
    res_reg <- res_reg |>
      mutate(latex_reg = 
               case_when(!is.na(beta_intxn) ~ str_c(latex_reg, 
                                                   sprintf("%0.3g (%.3g; [%.3g, %.3g])",
                                                           beta_intxn, beta_intxn_pval, beta_intxn_q025, beta_intxn_q975),
                                                   sep = " & "),
                         TRUE ~ str_c(latex_reg, 
                                      "---",
                                      sep=" & ")
               )
      )
  }
  res_reg_latex <- res_reg |>
    select(method, all_of(keep_vars), latex_reg) |>
    pivot_wider(names_from = method,
                names_prefix = 'latex_',
                values_from = latex_reg,
                names_vary = 'slowest')
  if (is.null(res_ttest)) {
    res_latex <- res_reg_latex
  } else {
    res_ttest_latex <- res_ttest |>
      mutate(latex_ttest = sprintf("%0.3g (%0.3g; %0.3g; [%0.3g, %0.3g])",
                                   estimate, statistic, p.value, conf.low, conf.high)) |>
      select(all_of(keep_vars), latex_ttest)
    
    if (is.null(keep_vars)) {
      res_latex <- bind_cols(res_ttest_latex, res_reg_latex)
    } else {
      res_latex <- res_ttest_latex |> inner_join(res_reg_latex, by=keep_vars)
    }
  }
  
  res_latex <- res_latex |>
    rowwise() |>
    mutate(latex = paste(across(starts_with('latex')), collapse =' & ')) |>
    ungroup() |>
    select(all_of(keep_vars), latex)
}

#' Get Latex for within-group effect estimates
#' 
#' @param res_reg Dataframe with within-group effect estimate results
#' @param res_ttest If not NULL, dataframe with t-test results
#' @param keep_vars List of variables to keep in the output 
make_timept_within_group_latex <- function(descrip_res, res_ttest=NULL, keep_vars=NULL) {
  if (!is.null(res_ttest)) {
    res <- descrip_res |>
      left_join(res_ttest, by=c('group', 'var')) |>
      mutate(latex = sprintf("%d & %0.3g (%0.3g) & %0.3g & t(%d) = %0.3g, p = %0.3g",
                             n, mean, sd, cohen_d, parameter, statistic, p.value))
  } else {
    res <- descrip_res |>
      mutate(latex = sprintf("%d & %0.3g (%0.3g) & %0.3g & ---",
                             n, mean, sd, cohen_d))
  }
  res_latex <- res |>
    select(group, all_of(keep_vars), var, latex) |>
    pivot_wider(names_from = group,
                values_from = latex,
                names_sep = '_',
                names_vary = 'slowest') |>
    mutate(latex = str_c(`1`, `2`, sep=' & ')) |>
    select(var, all_of(keep_vars), latex)
}
