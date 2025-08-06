### Functions for running t-tests

#' Run t-test to compare change scores across groups
#' 
#' @param dat Data
#' @param outcome_var Outcome variable name
#' @param retain_var Retention status variable name
#' @param save_dir Directory to save results to
#' @returns Dataframe of t-test results
run_ttest <- function(dat, outcome_var, retain_var, save_dir) {
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  save_fname <- str_c('ttest_', outcome_var, '.csv', sep='')
  dat <- dat |> filter(!!sym(retain_var) == 1)
  
  formula <- as.formula(paste0(outcome_var, " ~ group"))
  ttest_res <- t.test(formula, dat) |>
    broom::tidy()
  
  write_csv(ttest_res, file.path(save_dir, save_fname))
  
  return (ttest_res)
}

#' Get Cohen's d for between-group effect size
#' 
#' 
get_cohens_d <- function(dat, outcome_var, retain_var, save_dir) {
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  save_fname <- str_c('cohens_d_', outcome_var, '.csv', sep='')
  dat <- dat |> filter(!!sym(retain_var) == 1)
  
  cohens_d_res <- dat |>
    group_by(group) |>
    summarize(n = sum(!is.na(!!sym(outcome_var))),
              mean = mean(!!sym(outcome_var), na.rm=TRUE),
              sd = sd(!!sym(outcome_var), na.rm=TRUE)) |>
    pivot_wider(names_from = group,
                values_from = c(n, mean, sd)) |>
    mutate(pooled_sd = sqrt(((n_1 - 1)*sd_1^2 + (n_2 - 1)*sd_2^2) / (n_1 + n_2 - 2)),
           mean_diff = mean_1 - mean_2,
           cohens_d = (mean_diff / (pooled_sd)))
  
  write_csv(cohens_d_res, file.path(save_dir, save_fname))
  
  return (cohens_d_res)
}

#' Run paired t-test to compare the baseline-to-timept change in outcome within a group
#' 
#' @param dat Data
#' @param group_lbl Name of group
#' @param timept Timepoint to compare baseline to ('mid', 'eot', 'followup')
#' @param outcome_var Outcome variable name
#' @param retain_var Retention status variable name
#' @param save_dir Directory to save results to
run_paired_ttest <- function(dat, group_lbl, timept, outcome_var, retain_var, save_dir) {
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  save_fname <- sprintf('paired_ttest_group-%s_delta_%s.csv', group_lbl, str_c(timept, outcome_var, sep='_')) 
  dat <- dat |> 
    filter(group == group_lbl) |>
    filter(!!sym(retain_var) == 1)
  outcome_t1 <- dat |> pull(outcome_var)
  outcome_t2 <- dat |> pull(str_c(timept, outcome_var, sep='_'))
  
  ttest_res <- t.test(outcome_t2, outcome_t1, paired=TRUE) |>
    broom::tidy()
  write.csv(ttest_res, file.path(save_dir, save_fname), row.names = FALSE)
  
  return (ttest_res)
}