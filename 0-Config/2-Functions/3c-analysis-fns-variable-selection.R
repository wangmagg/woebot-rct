### Functions for variable selection


#' Select variables to include in linear regression
#' Runs a stepwise variable selection procedure, keeping variables based on AIC info criterion
#' 
#' @param dat Data
#' @param fixed_effect_vars List of variable names to include as dependent variables
#' @param retain_var Retention status variable name 
#' @param outcome_var Outcome variable name
#' @param save_dir Directory to save selected variables to
#' @param seed Random seed
#' @returns Selected variables
get_variables_for_fitting <- function(dat, fixed_effect_vars, retain_var, outcome_var, save_dir, seed=42) {
  set.seed(seed)
  
  # Get retained individuals, 
  # keep dependent and independent variable columns,
  # scale independent variables,
  # drop columns that have only one distinct value
  dat <- dat |>
    filter(!!sym(retain_var) == 1) |>
    select(all_of(fixed_effect_vars), !!sym(outcome_var)) |>
    mutate(across(c(where(is.numeric), -!!sym(outcome_var)), scale)) |>
    select(where(~n_distinct(.) > 1))
  
  # Run stepwise variable selection
  formula <- as.formula(paste0(outcome_var, " ~ ."))
  linreg_mdl <- lm(formula, data=dat)
  step_linreg_mdl <- stepAIC(linreg_mdl, direction="both", trace=FALSE)
  selected_vars <- attr(terms(step_linreg_mdl), "term.labels")
  
  # Make sure the group assignment variable is kept
  if (!('group' %in% selected_vars)) {
    selected_vars <- c('group', selected_vars)
  }
  
  # Save results
  if (!is.null(save_dir)) {
    save_fname <- str_c(retain_var, outcome_var, "selected_fixed_effect_vars.csv", sep='_')
    write.csv(selected_vars, file.path(save_dir, save_fname))
  }
  
  return(selected_vars)
}

#' Select variables to include when fitting retention weights
#' Only keeps variables that are correlated with retention
#' 
#' @param dat Data
#' @param fixed_effect_vars List of variable names to include as dependent variables
#' @param retain_var Retention status variable name 
#' @param outcome_var Outcome variable name
#' @param save_dir Directory to save selected variables to
#' @param seed Random seed
#' @returns Selected variables

get_variables_for_weights <- function(dat, fixed_effect_vars, retain_var, outcome_var, save_dir, seed=42) {
  set.seed(seed)
  
  # Keep dependent and independent variable columns,
  # scale independent variables,
  # drop columns that have only one distinct value
  dat <- dat |>
    select(all_of(fixed_effect_vars), !!sym(retain_var)) |>
    mutate(across(c(where(is.numeric), -!!sym(retain_var)), scale)) |>
    select(where(~n_distinct(.) > 1))
  
  # Get names of continuous and categorical variables
  continuous_vars <- dat |>
    select(where(is.numeric)) |>
    names()
  categorical_vars <- dat |>
    select(where(is.factor), -!!sym(retain_var)) |>
    names()
  
  # Run ANOVAs to test for correlation between continuous variables
  # and retention status
  aov_df <- data.frame()
  for (var in continuous_vars) {
    dat_var <- dat |>
      select(!!sym(var), !!sym(retain_var))
    formula <- as.formula(paste0(var, " ~ ", retain_var))
    aov_mdl <- aov(formula, data=dat_var)
    aov_df <- bind_rows(aov_df, 
                        data.frame(var=var, 
                                   p=summary(aov_mdl)[[1]][["Pr(>F)"]][1]))
  }
  # Keep continuous variables with significant correlation
  continuous_vars_keep <- aov_df |>
    filter(p < 0.05) |>
    pull(var)
  
  # Run Chi-squared tests to test for correlation between categorical variables
  # and retention status
  fisher_df <- data.frame()
  for (var in categorical_vars) {
    fisher_res <- fisher.test(dat[[var]], dat[[retain_var]], simulate.p.value=TRUE)
    fisher_df <- bind_rows(fisher_df, 
                           data.frame(var=var, 
                                      p=fisher_res$p.value))
  }
  # Keep categorical variables with significant correlation
  categorical_vars_keep <- fisher_df |>
    filter(p < 0.05) |>
    pull(var)
  
  # Combine continuous and categorical variables to keep
  selected_vars <- c(continuous_vars_keep, categorical_vars_keep)
  
  # Make sure the group assignment variable is kept
  if (!('group' %in% selected_vars)) {
    selected_vars <- c('group', selected_vars)
  }
  
  # Save results
  if (!is.null(save_dir)) {
    save_fname <- str_c(retain_var, outcome_var, "selected_weight_vars.csv", sep='_')
    write.csv(selected_vars, file.path(save_dir, save_fname))
  }

  return (selected_vars)
}