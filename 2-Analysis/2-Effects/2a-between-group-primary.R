source('0-Config/0-config.R')

analysis_types <- c("itt", "perprot")

for (analysis_type in analysis_types) {
  # Define directory for saving results
  save_dir <- file.path(ANALYSIS_OUT_DIR, '2-Effects-Output', '2a-Between-Group-Primary', analysis_type)
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Read input data file
  dat_outcome_regression <- read.csv(file.path(DATA_OUT_DIR, 'dat_analysis_outcome-regression.csv'))
  if (analysis_type == "perprot") {
    dat_outcome_regression <- dat_outcome_regression |>
      subset_per_protocol()
  }
  
  # Set factors and do column-mean imputation for BSCQ
  dat_outcome_regression <- dat_outcome_regression |>
    set_factors(c("group", "screen",  "retained_eot", "retained_mid", "retained_followup", DISCRETE_FIXED_EFFECT_VARS)) |>
    set_factors(c(ORDERED_DISCRETE_FIXED_EFFECT_VARS, ORDERED_DISCRETE_WEIGHT_VARS), ordered=TRUE) |>
    fill_column_mean('bscq') 
  
  timepts <- c('eot', 'mid', 'followup')
  analyses_to_run <- c('ttest', 'regression', 'subgroup_regression')
  
  # Run analyses across all timepoints
  for (timept in timepts) {
    outcome_var <- str_c('delta_', timept, '_p30')
    retain_var <- str_c('retained_', timept)
    
    for (analysis in analyses_to_run) {
      set.seed(42)
      
      if (analysis == 'ttest') {
        if (!dir.exists(file.path(save_dir, timept, analysis))) {
          dir.create(file.path(save_dir, timept, analysis), recursive = TRUE)
        }
        
        dat_to_fit <- dat_outcome_regression |>
          mutate_at(c("group"), ~relevel(.x, ref=1))
        
        # Run simple t-tests
        run_ttest(dat_to_fit, 
                  outcome_var = outcome_var,
                  retain_var = retain_var,
                  save_dir = file.path(save_dir, timept, analysis))
      } else if (analysis == 'regression') {
        if (!dir.exists(file.path(save_dir, timept, analysis))) {
          dir.create(file.path(save_dir, timept, analysis), recursive = TRUE)
        }
        
        dat_to_fit <- dat_outcome_regression |>
          mutate_at(c("group"), ~relevel(.x, ref=2))
        
        # Run variable selection
        fixed_effect_vars_selected <- get_variables_for_fitting(dat_outcome_regression, 
                                                                fixed_effect_vars = FIXED_EFFECT_VARS, 
                                                                retain_var = retain_var, 
                                                                outcome_var = outcome_var,
                                                                save_dir = file.path(save_dir, timept, analysis))

        # Run regressions
        if (analysis_type == "itt") {
          methods <- c('ols', 'weighted_ols')
          weight_vars_selected <- get_variables_for_weights(dat_outcome_regression, 
                                                            fixed_effect_vars = FIXED_EFFECT_VARS,
                                                            retain_var = retain_var,
                                                            outcome_var = outcome_var,
                                                            save_dir = file.path(save_dir, timept, analysis))
        } else {
          methods <- c('ols')
          weight_vars_selected <- NULL
        }
        run_regressions(dat_to_fit, 
                        methods = methods,
                        fixed_effect_vars = fixed_effect_vars_selected, 
                        weight_vars = weight_vars_selected, 
                        outcome_var = outcome_var,
                        retain_var = retain_var, 
                        bootstrap_reps = 1000, 
                        save_dir = file.path(save_dir, timept, analysis),
                        overwrite = FALSE)
      } else if (analysis == 'subgroup_regression' & timept == 'eot' & analysis_type == "itt") {
        if (!dir.exists(file.path(save_dir, timept, analysis))) {
          dir.create(file.path(save_dir, timept, analysis), recursive = TRUE)
        }
        dat_to_fit <- dat_outcome_regression |>
          mutate_at(c("group"), ~relevel(.x, ref=2))
        
        # Run subgroup regressions 
        fixed_effect_vars_selected <- get_variables_for_fitting(dat_to_fit, 
                                                                fixed_effect_vars = FIXED_EFFECT_VARS, 
                                                                retain_var = retain_var, 
                                                                outcome_var = outcome_var,
                                                                save_dir = file.path(save_dir, timept, analysis))
        weight_vars_selected <- get_variables_for_weights(dat_to_fit, 
                                                          fixed_effect_vars = FIXED_EFFECT_VARS,
                                                          retain_var = retain_var,
                                                          outcome_var = outcome_var,
                                                          save_dir = file.path(save_dir, timept, analysis))
        
        run_subgroup_regressions(dat_to_fit, 
                                 methods = c('ols', 'weighted_ols'),
                                 subgroup_vars = SUBGROUP_VARS,
                                 fixed_effect_vars = fixed_effect_vars_selected, 
                                 weight_vars = weight_vars_selected, 
                                 outcome_var = outcome_var,
                                 retain_var = retain_var, 
                                 bootstrap_reps = 1000, 
                                 save_dir = file.path(save_dir, timept, analysis),
                                 overwrite_combined = TRUE,
                                 overwrite_single = FALSE)
      }
    }
  }
}
