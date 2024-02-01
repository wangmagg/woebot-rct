fit_retention_weights_lm <- function(dat, retain_var, outcome_var) {
  participant_ids <- dat$participant_id
  
  dat_to_fit <- dat |>
    select(-participant_id, -!!sym(outcome_var)) |>
    select(where(~n_distinct(.) > 1)) |>
    mutate_if(is.numeric, scale)
    
  y <- dat |>
    pull(!!sym(retain_var))
  
  formula <- as.formula(paste0(retain_var, " ~ ."))
  
  weight_mdl <- glm(formula, data=dat_to_fit, family="binomial")
  probs <- predict(weight_mdl, newdata=dat_to_fit, type="response")
  
  probs_q_95 <- quantile(probs, 0.95)
  probs_q_05 <- quantile(probs, 0.05)

  probs[probs > probs_q_95] <- probs_q_95
  probs[probs < probs_q_05] <- probs_q_05

  weights <- rep(0, length(probs))
  weights[y == 1] <- mean(y == 1) / probs[y == 1]
  weights[y == 0] <- mean(y == 0) / probs[y == 0]
  
  weights_df <- data.frame(participant_id = participant_ids, 
                           weight = weights) |>
    distinct(participant_id, .keep_all=TRUE)
}

run_lm <- function(dat, fixed_effect_vars, retain_var, outcome_var) {
  dat <- dat |>
    filter(!!sym(retain_var) == 1) |>
    select(all_of(fixed_effect_vars), !!sym(outcome_var)) |>
    mutate(across(c(where(is.numeric), -!!sym(outcome_var)), scale)) |>
    select(where(~n_distinct(.) > 1))
  
  formula <- as.formula(paste0(outcome_var, " ~ ."))
  
  linreg_mdl <- lm(formula, data = dat)
  
  delta_use_hat_trt <- linreg_mdl$fitted.values[dat$group == 1]
  delta_use_hat_ctrl <- linreg_mdl$fitted.values[dat$group == 2]
  
  # X_trt <- dat |> filter(group == 1)
  # X_ctrl <- dat |> filter(group == 2)
  # 
  # warning_pred_trt <- FALSE
  # delta_use_hat_trt <- withCallingHandlers(
  #   predict(linreg_mdl, X_trt),
  #   warning = function(w) {
  #     warning_pred_trt <<- TRUE
  #   }
  # )
  # 
  # warning_pred_ctrl <- FALSE
  # delta_use_hat_ctrl <- withCallingHandlers(
  #   predict(linreg_mdl, X_ctrl),
  #   warning = function(w) {
  #     warning_pred_ctrl <<- TRUE
  #   }
  # )

  ate <- mean(delta_use_hat_trt) - mean(delta_use_hat_ctrl)
  beta <- coef(linreg_mdl)['group1']
  
  list('mdl' = linreg_mdl,
       'ate' = ate, 
       'beta' = beta)
}

bootstrap_run <- function(dat, fixed_effect_vars, retain_var, outcome_var, n_iters, seed=42) {
  set.seed(seed)
  
  obs <- run_lm(dat, fixed_effect_vars, retain_var, outcome_var)
  bs_res <- pbsapply(1:n_iters,
                     function(i) {
                       dat_iter <- dat |> 
                         slice_sample(prop=1, by=c(group, retain_var), replace=TRUE)
                       bs_iter <- run_lm(dat_iter, fixed_effect_vars, retain_var, outcome_var)
                     }, simplify=FALSE)
  bs_res <- as.data.frame(do.call(rbind, bs_res))
  
  pooled_mean <- dat |>
    pull(!!sym(outcome_var)) |>
    mean()
  dat_h0 <- dat |>
    group_by(group) |>
    mutate(across({{outcome_var}}, function(.x) {.x - mean(.x) + pooled_mean})) |>
    ungroup()
  
  bs_res_h0 <- pbsapply(1:n_iters,
                        function(i) {
                          dat_h0_iter <- dat_h0 |> 
                            slice_sample(prop=1, by=c(group, retain_var), replace=TRUE)
                          ate_bs_h0_iter <- run_lm(dat_h0_iter, fixed_effect_vars, retain_var, outcome_var)
                        }, simplify=FALSE)
  bs_res_h0 <- as.data.frame(do.call(rbind, bs_res_h0))
  
  list('ate' = obs$ate,
       'ate_q025' = quantile(unlist(bs_res$ate), 0.025),
       'ate_q975' = quantile(unlist(bs_res$ate), 0.975),
       'ate_pval' = (1 + sum(abs(unlist(bs_res_h0$ate)) >= abs(obs$ate))) / (n_iters + 1),
       'beta' = obs$beta,
       'beta_q025' = quantile(unlist(bs_res$beta),0.025),
       'beta_q975' = quantile(unlist(bs_res$beta),0.975),
       'beta_pval' = (1 + sum(abs(unlist(bs_res_h0$beta)) >= abs(obs$beta))) / (n_iters + 1),
       'n_warnings' = sum(unlist(bs_res$warning)))
}

run_weighted_lm <- function(dat, fixed_effect_vars, retain_var, outcome_var, weight_vars) {
  dat_for_weights <- dat |>
    select(all_of(weight_vars), !!sym(retain_var), !!sym(outcome_var), participant_id)
  weights <- fit_retention_weights_lm(dat_for_weights, retain_var, outcome_var)
  
  dat_for_reg <- dat |>
    filter(!!sym(retain_var) == 1) |>
    select(all_of(fixed_effect_vars), !!sym(outcome_var), participant_id)
  
  dat_for_reg_weighted <- dat_for_reg |>
    left_join(weights, by=c('participant_id')) |>
    select(-participant_id) |>
    mutate(across(c(where(is.numeric), -!!sym(outcome_var), -weight), scale)) |>
    select(where(~n_distinct(.) > 1), weight)
  
  formula <- as.formula(paste0(outcome_var, " ~ . - weight"))
  
  weighted_linreg_mdl <- lm(formula, data = dat_for_reg_weighted, weights=dat_for_reg_weighted$weight)
  
  dat_for_reg_weighted_trt <- dat_for_reg_weighted |>
    filter(group == 1)
  dat_for_reg_weighted_ctrl <- dat_for_reg_weighted |>
    filter(group == 2)
  
  warning_pred_trt <- FALSE
  weighted_delta_use_hat_trt <- withCallingHandlers(
    predict(weighted_linreg_mdl, 
            newdata = dat_for_reg_weighted_trt, 
            weights = dat_for_reg_weighted_trt$weight),
    warning = function(w) warning_pred_trt <<- TRUE
  )
     
  warning_pred_ctrl <- FALSE
  weighted_delta_use_hat_ctrl <- withCallingHandlers(
    predict(weighted_linreg_mdl, 
            newdata = dat_for_reg_weighted_ctrl, 
            weights = dat_for_reg_weighted_ctrl$weight),
    warning = function(w) warning_pred_ctrl <<- TRUE
  )
  
  weighted_ate <- mean(weighted_delta_use_hat_trt) - mean(weighted_delta_use_hat_ctrl)
  weighted_beta <- coef(weighted_linreg_mdl)['group1']
  
  list('mdl' = weighted_linreg_mdl,
       'ate' = weighted_ate, 
       'beta' = weighted_beta, 
       'warning' = warning_pred_trt + warning_pred_ctrl)
}

bootstrap_run_weighted <- function(dat, fixed_effect_vars, retain_var, outcome_var, weight_vars, n_iters, seed=42) {
  set.seed(seed)
  
  obs <- run_weighted_lm(dat, fixed_effect_vars, retain_var, outcome_var, weight_vars)
  bs_res <- pbsapply(1:n_iters,
                     function(i) {
                       dat_bs_iter <- dat |>
                         group_by(group, !!sym(retain_var)) |>
                         slice_sample(prop=1, replace=TRUE) |>
                         ungroup()
                       bs_iter <- run_weighted_lm(dat_bs_iter, fixed_effect_vars, retain_var, outcome_var, weight_vars)
                     }, simplify=FALSE)
  bs_res <- as.data.frame(do.call(rbind, bs_res))
  
  pooled_mean <- dat |>
    pull(!!sym(outcome_var)) |>
    mean()
  dat_h0 <- dat |>
    group_by(group) |>
    mutate(across({{outcome_var}}, function(.x) {.x - mean(.x) + pooled_mean})) |>
    ungroup()
  
  bs_res_h0 <- pbsapply(1:n_iters,
                        function(i) {
                          dat_h0_iter <- dat_h0 |> 
                            group_by(group, !!sym(retain_var)) |>
                            slice_sample(prop=1, replace=TRUE) |>
                            ungroup()
                          bs_h0_iter <- run_weighted_lm(dat_h0_iter, fixed_effect_vars, retain_var, outcome_var, weight_vars)
                        }, simplify=FALSE)
  bs_res_h0 <- as.data.frame(do.call(rbind, bs_res_h0))
  
  list('ate' = obs$ate,
       'ate_q025' = quantile(unlist(bs_res$ate), 0.025),
       'ate_q975' = quantile(unlist(bs_res$ate), 0.975),
       'ate_pval' = (1 + sum(abs(unlist(bs_res_h0$ate)) >= abs(obs$ate))) / (n_iters + 1),
       'beta' = obs$beta,
       'beta_q025' = quantile(unlist(bs_res$beta),0.025, na.rm=TRUE),
       'beta_q975' = quantile(unlist(bs_res$beta),0.975, na.rm=TRUE),
       'beta_pval' = (1 + sum(abs(unlist(bs_res_h0$beta)) >= abs(obs$beta), na.rm=TRUE)) / (n_iters + 1),
       'n_warnings' = sum(unlist(bs_res$warning)))
}

run_intxn_lm <- function(dat, fixed_effect_vars, retain_var, outcome_var) {
  dat <- dat |>
    filter(!!sym(retain_var) == 1) |>
    select(all_of(fixed_effect_vars), !!sym(outcome_var), subgroup, participant_id) |>
    mutate(across(c(where(is.numeric), -!!sym(outcome_var), -participant_id), scale)) |>
    select(where(~n_distinct(.) > 1))
  
  formula <- as.formula(paste0(outcome_var, " ~ . + subgroup + subgroup:group"))
  linreg_mdl <- lm(formula, data = dat)
  
  pred <- predict(linreg_mdl)

  ates <- dat |> 
    mutate(pred = pred) |>
    select(group, subgroup, pred) |>
    pivot_wider(names_from=group, 
                values_from=pred,
                names_prefix='mean_pred_grp_',
                values_fn=mean) |>
    mutate(ate = mean_pred_grp_1 - mean_pred_grp_2) |>
    select(subgroup, ate)
}

run_intxn_weighted_lm <- function(dat, fixed_effect_vars, retain_var, outcome_var, weight_vars) {
  dat_for_weights <- dat |>
    select(all_of(weight_vars), !!sym(retain_var), !!sym(outcome_var), subgroup, participant_id)
  weights <- fit_retention_weights_lm(dat_for_weights, retain_var, outcome_var)
  
  dat_for_reg <- dat |>
    filter(!!sym(retain_var) == 1) |>
    select(all_of(fixed_effect_vars), !!sym(outcome_var), subgroup, participant_id)
  
  dat_for_reg_weighted <- dat_for_reg |>
    left_join(weights, by=c('participant_id')) |>
    select(-participant_id) |>
    mutate(across(c(where(is.numeric), -!!sym(outcome_var), -weight), scale)) |>
    select(where(~n_distinct(.) > 1))
  
  formula <- as.formula(paste0(outcome_var, " ~ . - weight + subgroup + subgroup:group"))
  linreg_mdl <- lm(formula, data=dat_for_reg_weighted, weights=dat_for_reg_weighted$weight)
  
  ates <- dat_for_reg_weighted |>
    mutate(pred = predict(linreg_mdl, weights=weight)) |>
    select(group, subgroup, pred) |>
    pivot_wider(names_from=group, 
                values_from=pred,
                names_prefix='mean_pred_grp_',
                values_fn=mean) |>
    mutate(ate = mean_pred_grp_1 - mean_pred_grp_2) |>
    select(subgroup, ate)
  
  return (ates)
}

bootstrap_run_intxn <- function(dat, fixed_effect_vars, retain_var, outcome_var, n_iters, seed=42) {
  set.seed(seed)
  
  ate_obs <-  run_intxn_lm(dat, fixed_effect_vars, retain_var, outcome_var)
  
  ate_bs <- pbsapply(1:n_iters,
                     function(i) {
                       dat_bs_iter <- dat |>
                         group_by(group, subgroup) |>
                         slice_sample(prop=1, replace=TRUE) |>
                         ungroup()
                       ate_bs_iter <- run_intxn_lm(dat_bs_iter, fixed_effect_vars, retain_var, outcome_var) |>
                         mutate(iter = i)
                     }, simplify=FALSE)
  ate_bs <- do.call(bind_rows, ate_bs)
  
  res <- ate_bs |>
    group_by(subgroup) |>
    summarize(ate_q025 = quantile(ate, 0.025),
              ate_q975 = quantile(ate, 0.975)) |>
    ungroup()
  
  res <- ate_obs |> inner_join(res, by='subgroup') 
}

bootstrap_run_intxn_weighted <- function(dat, fixed_effect_vars, retain_var, outcome_var, weight_vars, n_iters, seed=42) {
  set.seed(seed)
  
  ate_obs <- run_intxn_weighted_lm(dat, fixed_effect_vars, retain_var, outcome_var, weight_vars)
  
  ate_bs <- pbsapply(1:n_iters,
                     function(i) {
                       dat_bs_iter <- dat |>
                         group_by(group, subgroup) |>
                         slice_sample(prop=1, replace=TRUE) |>
                         ungroup()
                       ate_bs_iter <- run_intxn_weighted_lm(dat_bs_iter, fixed_effect_vars, retain_var, outcome_var, weight_vars) |>
                         mutate(iter = i)
                     }, simplify=FALSE)
  ate_bs <- do.call(bind_rows, ate_bs)
  
  res <- ate_bs |>
    group_by(subgroup) |>
    summarize(ate_q025 = quantile(ate, 0.025),
              ate_q975 = quantile(ate, 0.975)) |>
    ungroup()
  
  res <- ate_obs |>
    inner_join(res, by='subgroup') 
}

run_regressions <- function(dat, 
                            methods,
                            fixed_effect_vars, 
                            weight_vars, 
                            outcome_var,
                            retain_var, 
                            bootstrap_reps, 
                            save_dir,
                            overwrite = FALSE) {
  
  # fit linear model with all data (bootstrapped)
  save_fname <- sprintf("res_%s_%s.csv", outcome_var, paste(methods, collapse='-'))
  
  if (!overwrite & file.exists(file.path(save_dir, save_fname))) {
    print(sprintf("Results file %s already exists, skipping...", save_fname))
    return (NULL)
  }
  
  res <- data.frame()
  
  if ('ols' %in% methods) {
    print("Fitting linear model with all data...")
    ols_res <- bootstrap_run(dat, fixed_effect_vars, retain_var, outcome_var, bootstrap_reps)
    ols_res$method <- 'ols'
    res <- bind_rows(res, ols_res)
  }
 
  if ('weighted_ols' %in% methods) {
    print("Fitting weighted linear model with all data...")
    weighted_ols_res <- bootstrap_run_weighted(dat, fixed_effect_vars, retain_var, outcome_var, weight_vars, bootstrap_reps)
    weighted_ols_res$method <- 'weighted_ols'
    res <- bind_rows(res, weighted_ols_res)
  }
  
  write.csv(res, file.path(save_dir, save_fname), row.names=FALSE)
}

run_single_subgroup_regressions <- function(dat, 
                                            methods,
                                            subgroup_var,
                                            fixed_effect_vars, 
                                            weight_vars,
                                            outcome_var,
                                            retain_var, 
                                            bootstrap_reps,
                                            save_dir,
                                            overwrite) {
  
  save_fname <- sprintf("subgroups_res_%s_%s_%s.csv", subgroup_var, outcome_var, paste(methods, collapse='-'))
  
  if (file.exists(file.path(save_dir, save_fname)) & !overwrite) {
    print(sprintf("Results file %s already exists, loading...", save_fname))
    res <- read.csv(file.path(save_dir, save_fname))
    return (res)
  }
  
  # Revert DAST scores for subgroup regression
  if (subgroup_var == 'dast') {
    # dat <- dat |>
    #   revert_dast(c('dast'))
    dat <- dat |>
      select(-dast) |>
      set_composite_sums(c('dast')) 
  }
  
  # Get subgroup data
  get_subgroups <- get_subgrouping_fn(subgroup_var)
  subgroups <- get_subgroups(dat)
  dat_sg <- dat |>
    inner_join(subgroups, by=c('participant_id'))
  
  
  res <- data.frame()
  
  if ('ols' %in% methods) {
    print("Fitting linear model with all data...")
    ols_res <- bootstrap_run_intxn(dat_sg, fixed_effect_vars, retain_var, outcome_var, bootstrap_reps)
    ols_res <- ols_res |>
      mutate(method = 'ols')
    res <- bind_rows(res, ols_res)
  }
  
  if ('weighted_ols' %in% methods) {
    print("Fitting weighted linear model with all data...")
    weighted_ols_res <- bootstrap_run_intxn_weighted(dat_sg, fixed_effect_vars, retain_var, outcome_var, weight_vars,
                                                     bootstrap_reps)
    weighted_ols_res <- weighted_ols_res |>
      mutate(method = 'weighted_ols')
    res <- bind_rows(res, weighted_ols_res)
  }
  
  res <- res |> arrange(method, subgroup)
  write.csv(res, file.path(save_dir, save_fname), row.names=FALSE)
  
  return (res)
}

run_subgroup_regressions <- function(dat, 
                                     methods,
                                     subgroup_vars,
                                     fixed_effect_vars, 
                                     weight_vars,
                                     outcome_var,
                                     retain_var, 
                                     bootstrap_reps,
                                     save_dir, 
                                     overwrite_combined,
                                     overwrite_single) {
  
  save_fname <- sprintf("subgroups_res_%s_%s.csv", outcome_var, paste(methods, collapse='-'))
  
  if (!overwrite_combined & file.exists(file.path(save_dir, save_fname))) {
    print(sprintf("Results file %s already exists, skipping...", save_fname))
    return (NULL)
  }

  subgroup_res_combined <- data.frame()
  for (subgroup_var in subgroup_vars) {
    print(subgroup_var)
    subgroup_res <- run_single_subgroup_regressions(dat, 
                                                    methods,
                                                    subgroup_var,
                                                    fixed_effect_vars,
                                                    weight_vars,
                                                    outcome_var,
                                                    retain_var,
                                                    bootstrap_reps,
                                                    save_dir,
                                                    overwrite_single)
    subgroup_res <- subgroup_res |>
      mutate(subgroup_var = subgroup_var) |>
      mutate_at(vars(subgroup), as.character) |>
      unite("subgroup_name", subgroup_var, subgroup, sep="_")
    
    subgroup_res_combined <- bind_rows(subgroup_res_combined, subgroup_res)
  }
  write.csv(subgroup_res_combined, file.path(save_dir, save_fname), row.names=FALSE)
}
