source('0-Config/0-config.R')

# Define directory for saving results
save_dir <- file.path(LATEXIFY_OUT_DIR, 'perprot')

if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# T4 & 6: Primary & Secondary Outcomes at Baseline, 4-weeks, 8-weeks, 12-weeks
base_fname <- file.path(ANALYSIS_OUT_DIR, 
                        '1-Descriptives-Output', 
                        '2b-Outcome-Vars-Summary', 
                        'perprot',
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
                          'perprot',
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
res_dir <- file.path(ANALYSIS_OUT_DIR, 
                     '2-Effects-Output', 
                     '2a-Between-Group-Primary',
                     'perprot')
for (timept in c('mid', 'eot', 'followup')) {
  reg_fname <- str_c(res_dir, '/', timept, '/regression/res_delta_', timept, '_p30_ols.csv')
  ttest_fname <- str_c(res_dir, '/', timept, '/ttest/ttest_delta_', timept, '_p30.csv')
  
  if (file.exists(reg_fname) & file.exists(ttest_fname)) {
    res_reg <- read.csv(reg_fname)
    res_ttest <- read.csv(ttest_fname)
    
    res_latex <- make_timept_est_latex(res_reg, res_ttest) 
    
    save_fname <- str_c('treatment_effect_primary-', timept, '.csv')
    write.csv(res_latex, file.path(save_dir, save_fname), row.names = FALSE)
  }
}


# T10: Within Group "Effects" at 4-weeks, 8-weeks, 12-weeks
descriptives_dir <- file.path(ANALYSIS_OUT_DIR, 
                              '1-Descriptives-Output', 
                              '2b-Outcome-Vars-Summary',
                              'perprot')
res_dir <- file.path(ANALYSIS_OUT_DIR, 
                     '2-Effects-Output', 
                     '3-Within-Group',
                     'perprot')

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
