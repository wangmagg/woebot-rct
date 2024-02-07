source('0-Config/0-config.R')

# Define directory for saving results
save_dir <- file.path(LATEXIFY_OUT_DIR)

if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

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

make_timept_est_latex <- function(res_reg, res_ttest=NULL, keep_vars=NULL, pval=TRUE) {
  if (pval) {
    res_reg <- res_reg |>
      mutate(latex_reg = sprintf("%.3g (%.3g; [%.3g, %.3g])",
                                 ate, ate_pval, ate_q025, ate_q975))
  } else {
    res_reg <- res_reg |>
      mutate(latex_reg = sprintf("%0.3g (%0.3g, %0.3g)",
                                 ate, ate_q025, ate_q975))
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

make_timept_within_group_latex <- function(descrip_res, ttest_res=NULL, keep_vars=NULL) {
  if (!is.null(ttest_res)) {
    res <- descrip_res |>
      left_join(ttest_res, by=c('group', 'var')) |>
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

# T4 & 6: Primary & Secondary Outcomes at Baseline, 4-weeks, 8-weeks, 12-weeks
base_fname <- file.path(ANALYSIS_OUT_DIR, 
                        '1-Descriptives-Output', 
                        '2b-Outcome-Vars-Summary', 
                        'baseline',
                        'baseline-retained_descriptive_summary.csv')
if (file.exists(base_fname)) {
  base_descrip <- read.csv(base_fname)
  base_descrip_latex <- make_baseline_descrip_latex(base_descrip)
  write.csv(base_descrip_latex, file.path(save_dir, 'outcome-descriptive-baseline.csv'), row.names = FALSE)
}

for (timept in c('mid', 'eot', 'followup')) {
  timept_dir <- file.path(ANALYSIS_OUT_DIR, 
                          '1-Descriptives-Output', 
                          '2b-Outcome-Vars-Summary', 
                          timept)
  timept_fname <- str_c(timept, '-retained_descriptive_summary.csv')
  timept_path <- file.path(timept_dir, timept_fname)
  if (file.exists(timept_path)) {
    descrip <- read.csv(timept_path)
    descrip_latex <- make_timept_descrip_latex(descrip)
    
    save_fname <-str_c('outcome-descriptive-', timept, '.csv')
    write.csv(descrip_latex, file.path(save_dir, save_fname), row.names = FALSE)
  }
}

# T5: Estimated Treatment Effects at 4-weeks, 8-weeks, 12-weeks
res_dir <- file.path(ANALYSIS_OUT_DIR, '2-Effects-Output', '2a-Between-Group-Primary')
for (timept in c('mid', 'eot', 'followup')) {
  reg_fname <- str_c(res_dir, '/', timept, '/regression/res_delta_', timept, '_p30_ols-weighted_ols.csv')
  ttest_fname <- str_c(res_dir, '/', timept, '/ttest/ttest_delta_', timept, '_p30.csv')
  
  if (file.exists(reg_fname) & file.exists(ttest_fname)) {
    res_reg <- read.csv(reg_fname)
    res_ttest <- read.csv(ttest_fname)
    
    res_latex <- make_timept_est_latex(res_reg, res_ttest)
    
    save_fname <- str_c('treatment_effect_primary-', timept, '.csv')
    write.csv(res_latex, file.path(save_dir, save_fname), row.names = FALSE)
  }
}

# T7: Estimated Secondary Outcome Treatment Effects at 4-weeks, 8-weeks, 12-weeks
res_dir <- file.path(ANALYSIS_OUT_DIR, '2-Effects-Output', '2b-Between-Group-Secondary')
for (timept in c('mid', 'eot', 'followup')) {
  outcome_vars_timept <- OUTCOME_VARS_DICT[[timept]]
  secondary_outcome_vars_timept <- outcome_vars_timept[outcome_vars_timept != 'p30']
  
  res_latex_combined <- data.frame()
  for (outcome_var in secondary_outcome_vars_timept) {
    reg_outcome_var_fname <- str_c(res_dir, '/', timept, '/regression/res_delta_', timept, '_', 
                                   outcome_var, '_ols-weighted_ols.csv')
    ttest_outcome_var_fname <- str_c(res_dir, '/', timept, '/ttest/ttest_delta_', timept, '_', 
                                     outcome_var, '.csv')
    
    if (file.exists(reg_outcome_var_fname) & file.exists(ttest_outcome_var_fname)) {
      res_reg <- read.csv(reg_outcome_var_fname) |> 
        mutate(var = outcome_var)
      res_ttest <- read.csv(ttest_outcome_var_fname) |> 
        mutate(var = outcome_var)
      res_latex <- make_timept_est_latex(res_reg, res_ttest, c('var'))
      res_latex_combined <- bind_rows(res_latex_combined, res_latex)
    }
  }
  
  save_fname <- str_c('treatment_effect_secondary-', timept, '.csv')
  write.csv(res_latex_combined, file.path(save_dir, save_fname), row.names = FALSE)
}

# T8: Primary Outcome by Subgroup at 4-weeks, 8-weeks, 12-weeks
subgroup_base_fname <- file.path(ANALYSIS_OUT_DIR, 
                                 '1-Descriptives-Output', 
                                 '2b-Outcome-Vars-Summary', 
                                 'baseline',
                                 'subgroups_baseline-retained_descriptive_summary.csv')
if (file.exists(subgroup_base_fname)) {
  subgroups_base_descrip <- read.csv(subgroup_base_fname) 
  subgroups_base_descrip_latex <- make_baseline_descrip_latex(subgroups_base_descrip, keep_vars = c('subgroup', 'level'))
  write.csv(subgroups_base_descrip_latex, file.path(save_dir, 'subgroups_outcome-descriptive-baseline.csv'), row.names = FALSE)
}

for (timept in c('mid', 'eot', 'followup')) {
  timept_dir <- file.path(ANALYSIS_OUT_DIR, '1-Descriptives-Output', '2b-Outcome-Vars-Summary', timept)
  subgroup_timept_fname <- str_c('subgroups_', timept, '-retained_descriptive_summary.csv')
  subgroup_timept_path <- file.path(timept_dir, subgroup_timept_fname)
  if (file.exists(subgroup_timept_path)) {
    descrip <- read.csv(subgroup_timept_path)
    descrip_latex <- make_timept_descrip_latex(descrip, keep_vars = c('subgroup', 'level'))
    
    save_fname <-str_c('subgroups-outcome-descriptive-', timept, '.csv')
    write.csv(descrip_latex, file.path(save_dir, save_fname), row.names = FALSE)
  }
}

# T9: Estimated Treatment Effects by Subgroup at 8-weeks
res_dir <- file.path(ANALYSIS_OUT_DIR, '2-Effects-Output', '2a-Between-Group-Primary')
for (timept in c('eot')) {
  fname <- str_c(res_dir, '/', timept, '/subgroup_regression/subgroups_res_delta_', timept, '_p30_ols-weighted_ols.csv')
  if (file.exists(fname)) {
    res_reg <- read.csv(fname)
  
    res_latex <- make_timept_est_latex(res_reg, keep_vars = c('subgroup_name'), pval=FALSE)
    save_fname <- str_c('subgroups_treatment_effect_primary-', timept, '.csv')
    write.csv(res_latex, file.path(save_dir, save_fname), row.names = FALSE)
  }
}

# T10: Within Group "Effects" at 4-weeks, 8-weeks, 12-weeks
descriptives_dir <- file.path(ANALYSIS_OUT_DIR, '1-Descriptives-Output', '2b-Outcome-Vars-Summary')
res_dir <- file.path(ANALYSIS_OUT_DIR, '2-Effects-Output', '3-Within-Group')

for (timept in c('mid', 'eot', 'followup')) {
  outcome_vars_timept <- OUTCOME_VARS_DICT[[timept]]
  descrip_fname <- str_c(descriptives_dir, '/', timept, '/', timept, '-retained_descriptive_summary.csv')
  descrip <- read.csv(descrip_fname) |>
    filter(str_starts(var, 'delta'))
  
  ttest_fname <- str_c(res_dir, '/', timept, '/paired_ttest_', timept, '.csv')
  if (file.exists(ttest_fname)) {
    ttest_res <- read.csv(ttest_fname)
    res_latex <- make_timept_within_group_latex(descrip, ttest_res)
  }
  else {
    res_latex <- make_timept_within_group_latex(descrip)
  }
  
  save_fname <- str_c('within_group_effect-', timept, '.csv')
  write.csv(res_latex, file.path(save_dir, save_fname), row.names = FALSE)
}
