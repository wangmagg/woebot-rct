run_shapiro_test <- function(dat, group_lbl, outcome_var, retain_var, save_dir) {
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  save_fname <- sprintf('shapiro_group-%s_%s.csv', group_lbl, outcome_var)
  dat <- dat |> 
    filter(group == group_lbl) |>
    filter(!!sym(retain_var) == 1) 
  shapiro_res <- shapiro.test(dat[[outcome_var]])
  shapiro_res |>
    broom::tidy() |>
    write_csv(file.path(save_dir, save_fname))
}

run_ttest <- function(dat, outcome_var, retain_var, save_dir) {
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  save_fname <- str_c('ttest_', outcome_var, '.csv', sep='')
  dat <- dat |> filter(!!sym(retain_var) == 1)
  
  formula <- as.formula(paste0(outcome_var, " ~ group"))
  ttest_res <- t.test(formula, dat)
  ttest_res |>
    broom::tidy() |>
    write_csv(file.path(save_dir, save_fname))
  
  return (ttest_res)
}

run_paired_ttest <- function(dat, group_lbl, timept, outcome_var, retain_var, save_dir) {
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  save_fname <- sprintf('paired_ttest_group-%s_delta_%s.csv', group_lbl, str_c(timept, outcome_var, sep='_')) 
  dat <- dat |> 
    filter(group == group_lbl) |>
    filter(!!sym(retain_var) == 1)
  outcome_t1 <- dat |> pull(outcome_var)
  outcome_t2 <- dat |> pull(str_c(timept, outcome_var, sep='_'))
  
  ttest_res <- t.test(outcome_t2, outcome_t1, paired=TRUE) |>
    broom::tidy()
  write.csv(ttest_res, file.path(save_dir, save_fname), row.names = FALSE)
  
  return (ttest_res)
}