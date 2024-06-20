source('0-Config/0-config.R')

# Define directory for saving results
save_dir <- file.path(ANALYSIS_OUT_DIR, '2-Effects-Output', '1-Assumptions')

if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Read input data file
dat_outcome_regression <- read_csv(file.path(DATA_OUT_DIR, 'dat_analysis_outcome-regression.csv'), show_col_types = FALSE)

# Set discrete variables as factors
dat_outcome_regression <- dat_outcome_regression |>
  set_factors(c("group", "screen",  "retained_eot", "retained_mid", "retained_followup", DISCRETE_FIXED_EFFECT_VARS)) |>
  set_factors(ORDERED_DISCRETE_FIXED_EFFECT_VARS, ordered=TRUE) |>
  fill_column_mean('bscq')

timepts <- c('eot', 'mid', 'followup')

# Make visualizations for each timepoint and each outcome variable to check linear regression assumptions hold
for (timept in timepts) {
  outcome_vars_timept <- OUTCOME_VARS_DICT[[timept]]
  retain_var <- str_c('retained_', timept)
  
  for (outcome_var in outcome_vars_timept) {
    set.seed(42)
    
    if (!dir.exists(file.path(save_dir, timept, outcome_var))) {
      dir.create(file.path(save_dir, timept, outcome_var), recursive = TRUE)
    }
    
    delta_outcome_var_for_fit <- str_c('delta', timept, outcome_var, sep='_')
    dat_outcome_to_fit <- subset_has_outcome(dat_outcome_regression,
                                             delta_outcome_var_for_fit, 
                                             timept)

    var_candidates <- FIXED_EFFECT_VARS[FIXED_EFFECT_VARS != outcome_var]
    fixed_effect_vars_selected <- get_variables_for_fitting(dat_outcome_to_fit, 
                                                            fixed_effect_vars = var_candidates, 
                                                            retain_var = retain_var, 
                                                            outcome_var = delta_outcome_var_for_fit,
                                                            save_dir = file.path(save_dir, timept))
    
    dat_outcome_to_fit <- dat_outcome_to_fit |>
      select(all_of(fixed_effect_vars_selected), !!sym(delta_outcome_var_for_fit)) |>
      mutate(across(c(where(is.numeric), -!!sym(delta_outcome_var_for_fit)), scale)) |>
      select(where(~n_distinct(.) > 1))
    
    formula <- as.formula(paste0(delta_outcome_var_for_fit, " ~ ."))
    mdl_lm <- lm(formula, data = dat_outcome_to_fit)
    mdl_lm_df <- data.frame('y_hat' = mdl_lm$fitted.values,
                            'residuals' = mdl_lm$residuals)
    vars_df <- dat_outcome_to_fit |>
      select(all_of(fixed_effect_vars_selected))
    mdl_lm_df <- bind_cols(mdl_lm_df, vars_df)
    
    # Make QQ plot
    p_qq <- ggplot(mdl_lm_df, aes(sample = residuals)) +
      geom_qq() +
      geom_qq_line() +
      labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles') +
      theme_bw(base_size = 14)

    # Plot residuals against fitted values
    p_resid_vs_fitted <- ggplot(mdl_lm_df, aes(x = y_hat, y = residuals)) +
      geom_point() +
      labs(x = 'Fitted Values', y = 'Residuals') +
      theme_bw(base_size = 14)

    mdl_lm_df_long_factor <- mdl_lm_df |>
      select(where(is.factor), residuals) 
      
    p_resid_vs_factor_vars <- NULL
    if (ncol(mdl_lm_df_long_factor) > 1) {
      mdl_lm_df_long_factor <- mdl_lm_df_long_factor |>
        mutate(across(where(is.factor), as.character)) |>
        pivot_longer(cols = -residuals, 
                     names_to = 'var', 
                     values_to = 'value')
      p_resid_vs_factor_vars <- ggplot(mdl_lm_df_long_factor, aes(x = value, y = residuals)) +
        geom_point() +
        facet_wrap(~var, scales = 'free') +
        labs(x = 'Value', y = 'Residuals') +
        theme_bw(base_size = 14)
    }
   
    mdl_lm_df_numeric <- mdl_lm_df |>
      select(where(is.numeric), residuals, -y_hat)
    
    p_resid_vs_numeric_vars <- NULL
    if (ncol(mdl_lm_df_numeric) > 1) {
      mdl_lm_df_long_numeric <- mdl_lm_df_numeric |>
        pivot_longer(cols = -residuals, 
                     names_to = 'var', 
                     values_to = 'value')
      
      p_resid_vs_numeric_vars <- ggplot(mdl_lm_df_long_numeric, aes(x = value, y = residuals)) +
        geom_point() +
        facet_wrap(~var, scales = 'free') +
        labs(x = 'Value', y = 'Residuals') +
        theme_bw(base_size = 14)
    }
    
    # Arrange plots
    if (is.null(p_resid_vs_factor_vars) & is.null(p_resid_vs_numeric_vars)) {
      p_arranged <- ggarrange(p_qq, p_resid_vs_fitted, ncol = 2, labels=c("A", "B"))
    } else if (is.null(p_resid_vs_factor_vars)) {
      p_arranged <- ggarrange(p_qq, p_resid_vs_fitted, p_resid_vs_numeric_vars, ncol = 2, nrow = 2, 
                              labels=c("A", "B", "D"), widths = c(1, 1, 3))

    } else if (is.null(p_resid_vs_numeric_vars)) {
      p_arranged <- ggarrange(p_qq, p_resid_vs_fitted, p_resid_vs_factor_vars, ncol = 2, nrow = 2, 
                              labels=c("A", "B", "C"), widths = c(1, 1, 3))
    } else {
      p_arranged <- ggarrange(p_qq, p_resid_vs_fitted, p_resid_vs_factor_vars, p_resid_vs_numeric_vars, ncol = 2, nrow = 2, 
                              labels=c("A", "B", "C", "D"), widths = c(1, 1, 3, 3))
    }
    
    p_arranged <- annotate_figure(p_arranged, top = text_grob(paste(outcome_var, timept), 
                                                face = 'bold', size = 14))
    
    # Save plots
    ggsave(file.path(save_dir, timept, str_c(outcome_var, 'assumption_checks.png', sep='_')), 
           p_arranged,
           width=12, height=12)
  }
}