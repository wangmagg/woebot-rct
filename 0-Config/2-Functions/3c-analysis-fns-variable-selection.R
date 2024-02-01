get_variables_for_fitting <- function(dat, fixed_effect_vars, retain_var, outcome_var, save_dir, seed=42) {
  set.seed(seed)
  dat <- dat |>
    filter(!!sym(retain_var) == 1) |>
    select(all_of(fixed_effect_vars), !!sym(outcome_var)) |>
    mutate(across(c(where(is.numeric), -!!sym(outcome_var)), scale)) |>
    select(where(~n_distinct(.) > 1))
  
  formula <- as.formula(paste0(outcome_var, " ~ ."))
  linreg_mdl <- lm(formula, data=dat)
  step_linreg_mdl <- stepAIC(linreg_mdl, direction="both", trace=FALSE)
  selected_vars <- attr(terms(step_linreg_mdl), "term.labels")
  
  if (!('group' %in% selected_vars)) {
    selected_vars <- c('group', selected_vars)
  }
  
  if (!is.null(save_dir)) {
    save_fname <- str_c(retain_var, outcome_var, "selected_fixed_effect_vars.csv", sep='_')
    write.csv(selected_vars, file.path(save_dir, save_fname))
  }
  
  return(selected_vars)
}

get_variables_for_weights <- function(dat, fixed_effect_vars, retain_var, outcome_var, save_dir) {
  dat <- dat |>
    select(all_of(fixed_effect_vars), !!sym(retain_var)) |>
    mutate(across(c(where(is.numeric), -!!sym(retain_var)), scale)) |>
    select(where(~n_distinct(.) > 1))
  
  continuous_vars <- dat |>
    select(where(is.numeric)) |>
    names()
  categorical_vars <- dat |>
    select(where(is.factor), -!!sym(retain_var)) |>
    names()
  
  # run ANOVAs to test for correlation with retention
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
  continuous_vars_keep <- aov_df |>
    filter(p < 0.05) |>
    pull(var)
  
  # run chi-squared tests to test for correlation with retention
  fisher_df <- data.frame()
  for (var in categorical_vars) {
    fisher_res <- fisher.test(dat[[var]], dat[[retain_var]], simulate.p.value=TRUE)
    fisher_df <- bind_rows(fisher_df, 
                           data.frame(var=var, 
                                      p=fisher_res$p.value))
  }
  categorical_vars_keep <- fisher_df |>
    filter(p < 0.05) |>
    pull(var)
  
  # combine variables to keep
  selected_vars <- c(continuous_vars_keep, categorical_vars_keep)
  
  if (!('group' %in% selected_vars)) {
    selected_vars <- c('group', selected_vars)
  }
  
  if (!is.null(save_dir)) {
    save_fname <- str_c(retain_var, outcome_var, "selected_weight_vars.csv", sep='_')
    write.csv(selected_vars, file.path(save_dir, save_fname))
  }

  return (selected_vars)
}