source('0-Config/0-config.R')

analysis_types <- c("itt", "perprot")

for (analysis_type in analysis_types) {
  # Define directory for saving results
  save_dir <- file.path(ANALYSIS_OUT_DIR, '2-Effects-Output', '2b-Between-Group-Secondary', analysis_type)
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Read input data file
  dat_outcome_regression <- read.csv(file.path(DATA_OUT_DIR, 'dat_analysis_outcome-regression.csv'))
  if (analysis_type == "perprot") {
    dat_outcome_regression <- dat_outcome_regression |>
      subset_per_protocol()
  }
  
  # Set discrete variables as factors and do column-mean imputation for BSCQ
  dat_outcome_regression <- dat_outcome_regression |>
    set_factors(c("group", "screen",  "retained_eot", "retained_mid", "retained_followup", DISCRETE_FIXED_EFFECT_VARS)) |>
    set_factors(c(ORDERED_DISCRETE_FIXED_EFFECT_VARS, ORDERED_DISCRETE_WEIGHT_VARS), ordered=TRUE) |>
    fill_column_mean(c('bscq')) |>
    set_delta_vars(c('bscq'), 'eot') |>
    set_delta_vars(c('bscq'), 'mid') |>
    set_delta_vars(c('bscq'), 'followup')
  
  timepts <- c('eot', 'mid', 'followup')
  analyses_to_run <- c('ttest', 'regression')
  
  # Run analyses across all timepoints
  for (timept in timepts) {
    print(timept)
    
    outcome_vars_timept <- OUTCOME_VARS_DICT[[timept]]
    secondary_outcome_vars_timept <- outcome_vars_timept[outcome_vars_timept != 'p30']
    retain_var <- str_c('retained_', timept)
    
    for (analysis in analyses_to_run) {
      set.seed(42)
        
      res_all_outcome_var <- data.frame()
      
      for (outcome_var in secondary_outcome_vars_timept) {
        print(outcome_var)
        delta_outcome_var_for_fit <- str_c('delta', timept, outcome_var, sep='_')
        dat_outcome_to_fit <- subset_has_outcome(dat_outcome_regression, 
                                                 delta_outcome_var_for_fit,
                                                 timept)
        
        if (!dir.exists(file.path(save_dir, timept, analysis))) {
          dir.create(file.path(save_dir, timept, analysis), recursive = TRUE)
        }
        
        if (analysis == 'ttest') {
          # Run simple t-tests
          dat_outcome_ttest <- dat_outcome_to_fit |>
            mutate_at(c("group"), ~relevel(.x, ref=1))
          
          res_outcome_var <- run_ttest(dat_outcome_ttest, 
                                       outcome_var = delta_outcome_var_for_fit,
                                       retain_var = retain_var,
                                       save_dir = file.path(save_dir, timept, analysis))
          res_outcome_var$var <- outcome_var
          res_all_outcome_var <- bind_rows(res_all_outcome_var, res_outcome_var)
          
        } else if (analysis == 'regression') {
            dat_outcome_for_weights_rlvl <- dat_outcome_regression |> 
              mutate_at(c("group"), ~relevel(.x, ref=2))
            dat_outcome_to_fit_rlvl <- dat_outcome_to_fit |> 
              mutate_at(c("group"), ~relevel(.x, ref=2))
            
            var_candidates <- FIXED_EFFECT_VARS[FIXED_EFFECT_VARS != outcome_var]
            
            # Run variable selection
            fixed_effect_vars_selected <- get_variables_for_fitting(dat_outcome_to_fit_rlvl, 
                                                                    fixed_effect_vars = var_candidates, 
                                                                    retain_var = retain_var, 
                                                                    outcome_var = delta_outcome_var_for_fit,
                                                                    save_dir = file.path(save_dir, timept, analysis))
            
            # Run regressions
            if (analysis_type == "itt") {
              methods <- c('ols', 'weighted_ols')
              weight_vars_selected <- get_variables_for_weights(dat_outcome_for_weights_rlvl, 
                                                                fixed_effect_vars = var_candidates,
                                                                retain_var = retain_var,
                                                                outcome_var = outcome_var,
                                                                save_dir = file.path(save_dir, timept, analysis))
            } else {
              methods <- c('ols')
              weight_vars_selected <- NULL
            }
            
            res_outcome_var <- run_regressions(dat_outcome_to_fit_rlvl, 
                                               methods = methods,
                                               fixed_effect_vars = fixed_effect_vars_selected, 
                                               weight_vars = weight_vars_selected, 
                                               retain_var = retain_var, 
                                               outcome_var = delta_outcome_var_for_fit,
                                               bootstrap_reps = 1000, 
                                               save_dir = file.path(save_dir, timept, analysis),
                                               overwrite = FALSE)
            res_outcome_var$var <- outcome_var
            res_all_outcome_var <- bind_rows(res_all_outcome_var, res_outcome_var)
        }
      }
      
      # Adjust p-values
      if (analysis == 'ttest') {
        res_all_outcome_var$p.value_adj <- p.adjust(res_all_outcome_var$p.value, method='BH')
        save_fname <- sprintf("res_combined.csv")
        
      }
      if (analysis == 'regression') {
        res_all_outcome_var$beta_pval_adj <- p.adjust(res_all_outcome_var$beta_pval, method='BH')
        res_all_outcome_var$dim_pval_adj <- p.adjust(res_all_outcome_var$dim_pval, method='BH')
        save_fname <- sprintf("res_combined_%s.csv", paste(methods, collapse='-'))
      }
      
      # Save results
      write.csv(res_all_outcome_var, file.path(save_dir, timept, analysis, save_fname), row.names=FALSE)
    }
  }
}
