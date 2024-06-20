source('0-Config/0-config.R')

save_dir <- file.path(ANALYSIS_OUT_DIR, '4-BivarCorr-Output')
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Read input data file
dat_descriptive <- read.csv(file.path(DATA_OUT_DIR, 'dat_analysis_descriptive.csv'))

# Set factors and do column-mean imputation for BSCQ
dat_descriptive <- dat_descriptive |> 
  fill_column_mean(c('bscq')) |>
  set_delta_vars(c('bscq'), 'eot') |>
  set_delta_vars(c('bscq'), 'mid') |>
  set_delta_vars(c('bscq'), 'followup')

timepts <- c('eot', 'mid', 'followup')
vars <- c('p30', 'bscq', 'phq', 'gad', 'sps')

# Get pairwise Pearson correlations between variables 
for (timept in timepts) {
  retain_var <- str_c('retained_', timept)
  delta_vars_timept <- str_c('delta', timept, vars, sep='_')
  
  dat_to_analyze <- dat_descriptive |>
    filter(!!sym(retain_var) == 1) |>
    select(any_of(delta_vars_timept))
  
  pair_comps <- combn(colnames(dat_to_analyze), 2)
  bivar_cor <- apply(pair_comps,
                     2,
                     function(pair) {
                       dat_to_analyze_complete <- dat_to_analyze |>
                         select(all_of(pair)) |>
                         drop_na()
                       cor.test(dat_to_analyze_complete[,pair[1]], 
                                dat_to_analyze_complete[,pair[2]]) |>
                         broom::tidy() |>
                         mutate(var_1 = pair[1],
                                var_2 = pair[2]) |>
                         select(var_1, var_2, estimate, p.value)
                     })
  bivar_cor <- do.call(bind_rows, bivar_cor)
  write.csv(bivar_cor, file.path(save_dir, sprintf('bivar_cor_%s.csv', timept)))
}
