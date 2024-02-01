source('0-Config/0-config.R')

# Define directory for saving results
save_dir <- file.path(ANALYSIS_OUT_DIR, '1-Descriptives-Output', '1b-Missingness')
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Read input data file
dat_rand_base <- read_csv(file.path(DATA_OUT_DIR,'dat_analysis_missingness.csv'), show_col_types=FALSE)

timepts <- c('eot', 'mid', 'followup')

# Count number prefer not to answer (99) and number NA at other timepts
for (timept in timepts) {
    # counts for baseline
    count_99 <- dat_rand_base |>
      select(-starts_with(c('eot', 'mid', 'followup'))) |>
      group_by(group, !!sym(str_c('retained', timept, sep='_'))) |>
      summarize_at(vars(any_of(NON_COMPOSITE_VARS),  
                        starts_with(SUMMED_COMPOSITE_VARS),
                        starts_with(P30_VARS)), 
                   ~sum(.x == 99, na.rm=TRUE)) |>
      pivot_longer(-c(group, !!sym(str_c('retained', timept, sep='_'))),
                   names_to="variable",
                   values_to="num_99") |>
      arrange(variable)
    write.csv(count_99, file.path(save_dir, sprintf('baseline_retained-%s_count_99.csv', timept)), row.names=F)
    
    count_na <- dat_rand_base |>
      select(-starts_with(c('eot', 'mid', 'followup'))) |>
      group_by(group, !!sym(str_c('retained', timept, sep='_'))) |>
      summarize_at(vars(any_of(NON_COMPOSITE_VARS),
                        starts_with(SUMMED_COMPOSITE_VARS),
                        starts_with(AVERAGED_COMPOSITE_VARS),
                        starts_with(P30_VARS)), 
                   ~sum(is.na(.x))) |>
      pivot_longer(-c(group, !!sym(str_c('retained', timept, sep='_'))),
                   names_to="variable",
                   values_to="num_na") |>
      arrange(variable)
    write.csv(count_na, file.path(save_dir, sprintf('baseline_retained-%s_count_na.csv', timept)), row.names=F)

    # counts for timept
    count_99 <- dat_rand_base |>
      group_by(group, !!sym(str_c('retained', timept, sep='_'))) |>
      summarize_at(vars(any_of(NON_COMPOSITE_VARS) & contains(timept),  
                        starts_with(SUMMED_COMPOSITE_VARS) & contains(timept),
                        starts_with(P30_VARS) & contains(timept)), 
                   ~sum(.x == 99, na.rm=TRUE)) |>
      pivot_longer(-c(group, !!sym(str_c('retained', timept, sep='_'))),
                   names_to="variable",
                   values_to="num_99") |>
      arrange(variable)
    write.csv(count_99, file.path(save_dir, str_c(timept, 'count_99.csv', sep='_')), row.names=F)
    
    count_na <- dat_rand_base |>
      group_by(group, !!sym(str_c('retained', timept, sep='_'))) |>
      summarize_at(vars(any_of(NON_COMPOSITE_VARS) & contains(timept),  
                        starts_with(SUMMED_COMPOSITE_VARS) & contains(timept),
                        starts_with(AVERAGED_COMPOSITE_VARS) & contains(timept),
                        starts_with(P30_VARS) & contains(timept)), 
                   ~sum(is.na(.x))) |>
      pivot_longer(-c(group, !!sym(str_c('retained', timept, sep='_'))),
                   names_to="variable",
                   values_to="num_na") |>
      arrange(variable)
    write.csv(count_na, file.path(save_dir, str_c(timept, 'count_na.csv', sep='_')), row.names=F)

}
