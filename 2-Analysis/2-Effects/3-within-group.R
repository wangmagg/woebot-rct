source('0-Config/0-config.R')

# Define directory for saving results
save_dir <- file.path(ANALYSIS_OUT_DIR, '2-Effects-Output', '3-Within-Group')

if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Read input data file
dat_outcome_regression <- read.csv(file.path(DATA_OUT_DIR, 'dat_analysis_outcome-regression.csv'))

# Set discrete variables as factors
dat_outcome_regression <- dat_outcome_regression |>
  set_factors(c('group')) |>
  mutate_at(c("group"), ~relevel(.x, ref=1))

timepts <- c('eot')
group_lbls <- c(1, 2)

for (timept in timepts) {
  
  if (!dir.exists(str_c(save_dir, '/', timept))) {
    dir.create(str_c(save_dir, '/', timept), recursive = TRUE)
  }
  
  outcome_vars_timept <- OUTCOME_VARS_DICT[[timept]]
  
  all_var_res <- data.frame()
  for (var in outcome_vars_timept) {
    dat_outcome_to_fit <- subset_has_outcome(dat_outcome_regression,
                                             str_c('delta', timept, var, sep='_'),
                                             timept)
    for (group_lbl in group_lbls) {
      res <- run_paired_ttest(dat_outcome_to_fit,
                              timept = timept,
                              group_lbl = group_lbl,
                              outcome_var = var,
                              retain_var = str_c('retained', timept, sep='_'),
                              save_dir = str_c(save_dir, '/', timept))
      res <- res |>
        mutate(var = str_c('delta', timept, var, sep='_'),
               group = group_lbl)
      all_var_res <- bind_rows(all_var_res, res)
    }
  }
  write.csv(all_var_res, str_c(save_dir, '/', timept, '/paired_ttest_', timept, '.csv'))
}
