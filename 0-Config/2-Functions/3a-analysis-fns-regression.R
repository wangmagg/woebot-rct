fit_retention_weights_lm <- function(dat, retain_var, outcome_var) {
  participant_ids <- dat$participant_id
  
  dat_to_fit <- dat |>
    select(-participant_id, -!!sym(outcome_var)) |>
    select(where(~n_distinct(.) > 1), retain_var) |>
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


run_lm <- function(dat, boot_idx, fixed_effect_vars, retain_var, outcome_var) {
  dat <- dat[boot_idx, ]
  
  dat <- dat |>
    filter(!!sym(retain_var) == 1) |>
    select(all_of(fixed_effect_vars), !!sym(outcome_var)) |>
    mutate(across(c(where(is.numeric), -!!sym(outcome_var)), scale)) |>
    select(where(~n_distinct(.) > 1))
  
  formula <- as.formula(paste0(outcome_var, " ~ ."))
  linreg_mdl <- lm(formula, data = dat)
  beta <- coef(linreg_mdl)['group1']
  
  delta_use_hat_trt <- linreg_mdl$fitted.values[dat$group == 1]
  delta_use_hat_ctrl <- linreg_mdl$fitted.values[dat$group == 2]
  dim <- mean(delta_use_hat_trt) - mean(delta_use_hat_ctrl)
  
  c(beta, dim)
}

run_weighted_lm <- function(dat, boot_idx, fixed_effect_vars, retain_var, outcome_var, weight_vars) {
  dat <- dat[boot_idx, ]
  
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
  weighted_beta <- coef(weighted_linreg_mdl)['group1']
  
  dat_for_reg_weighted_trt <- dat_for_reg_weighted |> filter(group == 1)
  dat_for_reg_weighted_ctrl <- dat_for_reg_weighted |> filter(group == 2)
  
  weighted_delta_use_hat_trt <- predict(weighted_linreg_mdl,
                                        newdata = dat_for_reg_weighted_trt,
                                        weights = dat_for_reg_weighted_trt$weight)
  
  weighted_delta_use_hat_ctrl <- predict(weighted_linreg_mdl,
                                         newdata = dat_for_reg_weighted_ctrl,
                                         weights = dat_for_reg_weighted_ctrl$weight)
  
  weighted_dim <- mean(weighted_delta_use_hat_trt) - mean(weighted_delta_use_hat_ctrl)
  
  c(weighted_beta, weighted_dim)
}

bootstrap_run <- function(lm_fn, dat, fixed_effect_vars, retain_var, outcome_var, n_iters, seed=42, weight_vars=NULL) {
  set.seed(seed)
  
  if (!is.null(weight_vars)) {
    stat_boot <- boot(data=dat, statistic=lm_fn, R=n_iters, 
                      fixed_effect_vars=fixed_effect_vars, retain_var=retain_var, outcome_var=outcome_var, weight_vars=weight_vars)
  } else {
    stat_boot <- boot(data=dat, statistic=lm_fn, R=n_iters, 
                      fixed_effect_vars=fixed_effect_vars, retain_var=retain_var, outcome_var=outcome_var)
  }
  
  ci_beta <- boot.ci(stat_boot, conf=0.95, type='bca', index=1)
  ci_dim <- boot.ci(stat_boot, conf=0.95, type='bca', index=2)

  list('beta' = stat_boot$t0[1],
       'beta_q025' = ci_beta$bca[4],
       'beta_q975' = ci_beta$bca[5],
       'dim' = stat_boot$t0[2],
       'dim_q025' = ci_dim$bca[4],
       'dim_q975' = ci_dim$bca[5])
}

permutation_test <- function(stat_obs, lm_fn, dat, fixed_effect_vars, retain_var, outcome_var, n_iters, seed=42, ...) {
  set.seed(seed)

  stat_shuff <- pbsapply(1:n_iters,
                       function(i) {
                         dat <- dat |>
                           mutate(group = sample(group, n(), replace=FALSE))
                         stat_iter <- lm_fn(dat, 1:nrow(dat), fixed_effect_vars, retain_var, outcome_var, ...)
                       })
  
  pval <- rowMeans(abs(unlist(stat_shuff)) >= abs(unlist(stat_obs)))
  list('beta_pval' = pval[1],
       'dim_pval' = pval[2])
}

.get_dim_from_subgroup_mdl <- function(dat, linreg_mdl, ...) {
  pred <- predict(linreg_mdl, ...)
  
  dat |> 
    mutate(pred = pred) |>
    select(group, subgroup, pred) |>
    pivot_wider(names_from=group, 
                values_from=pred,
                names_prefix='mean_pred_grp_',
                values_fn=mean) |>
    mutate(ate = mean_pred_grp_1 - mean_pred_grp_2) |>
    select(subgroup, ate)
}

.get_beta_from_subgroup_mdl <- function(dat, linreg_mdl) {
  subgroup_lvls <- replace_na(levels(dat$subgroup), 'NA')
  
  subgroup_nonref_lvls <- subgroup_lvls[2:length(subgroup_lvls)]
  subgroup_nonref_names <- str_c('subgroup', subgroup_nonref_lvls)
  group_subgroup_intxn_names <- str_c('group1', subgroup_nonref_names, sep=':')
  
  betas <- coef(linreg_mdl)[c('group1', group_subgroup_intxn_names)]
  beta_intxns <- c(NA, betas[2:length(betas)])
  betas[2:length(betas)] <- betas[2:length(betas)] + betas[1]
  
  c(betas, beta_intxns)
}

run_intxn_lm <- function(dat, boot_idx, fixed_effect_vars, retain_var, outcome_var, ret_type='beta') {
  dat <- dat[boot_idx, ]
  
  dat <- dat |>
    filter(!!sym(retain_var) == 1) |>
    select(all_of(fixed_effect_vars), !!sym(outcome_var), subgroup, participant_id) |>
    mutate(across(c(where(is.numeric), -!!sym(outcome_var), -participant_id), scale)) |>
    select(where(~n_distinct(.) > 1))
  
  formula <- as.formula(paste0(outcome_var, " ~ . + group + subgroup + subgroup:group"))
  linreg_mdl <- lm(formula, data = dat)

  if (ret_type == 'beta') {
    .get_beta_from_subgroup_mdl(dat, linreg_mdl)
  } else if (ret_type == 'dim') {
    .get_dim_from_subgroup_mdl(dat, linreg_mdl)
  }
}

run_intxn_weighted_lm <- function(dat, boot_idx, fixed_effect_vars, retain_var, outcome_var, weight_vars, ret_type='beta') {
  dat <- dat[boot_idx, ]
  
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
    select(where(~n_distinct(.) > 1), weight)
  
  formula <- as.formula(paste0(outcome_var, " ~ . - weight + group + subgroup + subgroup:group"))
  linreg_mdl <- lm(formula, data=dat_for_reg_weighted, weights=dat_for_reg_weighted$weight)
  
  if (ret_type == 'beta') {
    .get_beta_from_subgroup_mdl(dat, linreg_mdl)
  } else if (ret_type == 'dim') {
    .get_dim_from_subgroup_mdl(dat, linreg_mdl)
  }
}

bootstrap_run_intxn <- function(lm_fn, dat, fixed_effect_vars, retain_var, outcome_var, n_iters, seed=42, weight_vars=NULL) {
  set.seed(seed)
  
  if (!is.null(weight_vars)) {
    stat_boot <- boot(data=dat, statistic=lm_fn, R=n_iters, 
                      fixed_effect_vars=fixed_effect_vars, retain_var=retain_var, outcome_var=outcome_var, weight_vars=weight_vars)
  } else {
    stat_boot <- boot(data=dat, statistic=lm_fn, R=n_iters, 
                      fixed_effect_vars=fixed_effect_vars, retain_var=retain_var, outcome_var=outcome_var)
  }
  
  subgroup_lvls <- replace_na(levels(dat$subgroup), 'NA')
  n_subgroup_lvls <- length(subgroup_lvls)
  
  beta <- data.frame(subgroup = subgroup_lvls,
                     beta = stat_boot$t0[1:n_subgroup_lvls])
  beta_intxn <- data.frame(subgroup = subgroup_lvls,
                           beta_intxn = stat_boot$t0[(n_subgroup_lvls + 1): length(stat_boot$t0)])
  
  all_beta_ci <- data.frame()
  all_beta_intxn_ci <- data.frame()
  for (i in 1:n_subgroup_lvls) {
    ci <- boot.ci(stat_boot, conf=0.95, type='bca', index=i)
    ci <- list(subgroup = subgroup_lvls[i],
               beta_q025 = ci$bca[4],
               beta_q975 = ci$bca[5])
    all_beta_ci <- bind_rows(all_beta_ci, ci)
    
    intxn_idx <- i + n_subgroup_lvls
    if (is.na(stat_boot$t0[intxn_idx])) {
      intxn_ci <- list(subgroup = subgroup_lvls[i],
                       beta_intxn_q025 = NA,
                       beta_intxn_q975 = NA)
    } else {
      intxn_ci <- boot.ci(stat_boot, conf=0.95, type='bca', index=intxn_idx)
      intxn_ci <- list(subgroup = subgroup_lvls[i],
                       beta_intxn_q025 = intxn_ci$bca[4],
                       beta_intxn_q975 = intxn_ci$bca[5])
    }
    all_beta_intxn_ci <- bind_rows(all_beta_intxn_ci, intxn_ci)
  }
  
  beta |> 
    left_join(all_beta_ci, by='subgroup') |>
    left_join(beta_intxn, by='subgroup') |>
    left_join(all_beta_intxn_ci, by='subgroup')
}

permutation_test_subgroup <- function(stat_obs, lm_fn, dat, fixed_effect_vars, retain_var, outcome_var, n_iters, seed=42, ...) {
  set.seed(seed)

  res_shuff <- pbsapply(1:n_iters,
                        function(i) {
                          dat <- dat |>
                            group_by(subgroup) |>
                            mutate(group = sample(group, size=n(), replace=FALSE)) |>
                            ungroup()
                          res_iter <- lm_fn(dat, 1:nrow(dat), fixed_effect_vars, retain_var, outcome_var, ...)
                        })
  
  subgroup_lvls <- replace_na(levels(dat$subgroup), 'NA')
  n_subgroup_lvls <- length(subgroup_lvls)
  
  beta_shuff <- res_shuff[1:n_subgroup_lvls, ]
  beta_obs <- stat_obs |> pull(beta)
  beta_intxn_shuff <- res_shuff[(n_subgroup_lvls + 1): (2*n_subgroup_lvls), ]
  beta_intxn_obs <- stat_obs |> pull(beta_intxn)
  
  beta_pvals <- rowMeans(abs(beta_shuff) >= abs(beta_obs))
  beta_intxn_pvals <- rowMeans(abs(beta_intxn_shuff) >= abs(beta_intxn_obs))
  
  data.frame(subgroup = subgroup_lvls,
             beta_pval = beta_pvals,
             beta_intxn_pval = beta_intxn_pvals)
}

run_regressions <- function(dat, 
                            methods,
                            fixed_effect_vars, 
                            weight_vars, 
                            outcome_var,
                            retain_var, 
                            bootstrap_reps, 
                            save_dir,
                            overwrite = FALSE,
                            seed = 42) {
  
  # fit linear model with all data (bootstrapped)
  save_fname <- sprintf("res_%s_%s.csv", outcome_var, paste(methods, collapse='-'))
  
  if (!overwrite & file.exists(file.path(save_dir, save_fname))) {
    print(sprintf("Results file %s already exists, skipping...", save_fname))
    res <- read.csv(file.path(save_dir, save_fname))
    return (res)
  }
  
  res <- data.frame()
  
  if ('ols' %in% methods) {
    print("Fitting linear model with all data...")
    ols_res <- bootstrap_run(run_lm, dat, fixed_effect_vars, retain_var, outcome_var, bootstrap_reps, seed)
    ols_pval <- permutation_test(ols_res[c('beta', 'dim')], run_lm, dat, 
                                 fixed_effect_vars, retain_var, outcome_var, 
                                 bootstrap_reps, seed)
    ols_res <- bind_cols(ols_res, ols_pval)
    ols_res$method <- 'ols'
    res <- bind_rows(res, ols_res)
  }
 
  if ('weighted_ols' %in% methods) {
    print("Fitting weighted linear model with all data...")
    weighted_ols_res <- bootstrap_run(run_weighted_lm, dat, 
                                      fixed_effect_vars, retain_var, outcome_var, 
                                      bootstrap_reps, seed, weight_vars)
    weighted_ols_pval <- permutation_test(weighted_ols_res[c('beta', 'dim')], run_weighted_lm, dat, 
                                          fixed_effect_vars, retain_var, outcome_var, 
                                          bootstrap_reps, seed, weight_vars)
    weighted_ols_res <- bind_cols(weighted_ols_res, weighted_ols_pval)
    weighted_ols_res$method <- 'weighted_ols'
    res <- bind_rows(res, weighted_ols_res)
  }
  
  write.csv(res, file.path(save_dir, save_fname), row.names=FALSE)
  
  return(res)
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
                                            overwrite = FALSE,
                                            seed = 42) {
  
  save_fname <- sprintf("subgroups_res_%s_%s_%s.csv", subgroup_var, outcome_var, paste(methods, collapse='-'))
  
  if (file.exists(file.path(save_dir, save_fname)) & !overwrite) {
    print(sprintf("Results file %s already exists, loading...", save_fname))
    res <- read.csv(file.path(save_dir, save_fname))
    return (res)
  }
  
  # Revert DAST scores for subgroup regression
  if (subgroup_var == 'dast') {
    dat <- dat |>
      select(-dast) |>
      set_composite_sums(c('dast')) 
  }
  
  # Get subgroup data
  get_subgroups <- get_subgrouping_fn(subgroup_var)
  subgroups <- get_subgroups(dat) |>
    filter(!is.na(subgroup))
  dat_sg <- dat |>
    inner_join(subgroups, by=c('participant_id'))
  
  
  res <- data.frame()
  
  if ('ols' %in% methods) {
    print("Fitting linear model with all data...")
    ols_res <- bootstrap_run_intxn(run_intxn_lm, dat_sg, 
                                   fixed_effect_vars, retain_var, outcome_var, 
                                   bootstrap_reps, seed) 
    ols_pval <- permutation_test_subgroup(ols_res, run_intxn_lm, dat_sg,
                                          fixed_effect_vars, retain_var, outcome_var, 
                                          bootstrap_reps, seed)
    ols_res <- ols_res |>
      left_join(ols_pval, by='subgroup') |>
      mutate(method = 'ols')

    res <- bind_rows(res, ols_res) 
  }
  
  if ('weighted_ols' %in% methods) {
    print("Fitting weighted linear model with all data...")
    weighted_ols_res <- bootstrap_run_intxn(run_intxn_weighted_lm, dat_sg, 
                                            fixed_effect_vars, retain_var, outcome_var,
                                            bootstrap_reps, seed, weight_vars)
    weighted_ols_pval <- permutation_test_subgroup(weighted_ols_res, run_intxn_weighted_lm, dat_sg,
                                                   fixed_effect_vars, retain_var, outcome_var, 
                                                   bootstrap_reps, seed, weight_vars)
    weighted_ols_res <- weighted_ols_res |>
      left_join(weighted_ols_pval, by='subgroup') |>
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
                                     overwrite_single,
                                     seed = 42) {
  
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
                                                    overwrite_single,
                                                    seed)
    subgroup_res <- subgroup_res |>
      mutate(subgroup_var = subgroup_var) |>
      mutate_at(vars(subgroup), as.character) |>
      unite("subgroup_name", subgroup_var, subgroup, sep="_")
    
    subgroup_res_combined <- bind_rows(subgroup_res_combined, subgroup_res)
  }
  subgroup_res_combined$beta_adj_pval <- p.adjust(subgroup_res_combined$beta_pval, method='BH')
  subgroup_res_combined$beta_intxn_adj_pval <- p.adjust(subgroup_res_combined$beta_intxn_pval, method='BH')
  
  write.csv(subgroup_res_combined, file.path(save_dir, save_fname), row.names=FALSE)
  
  return (subgroup_res_combined)
}
