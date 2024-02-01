source('0-Config/0-config.R')

# Define directory for saving results
save_dir <- file.path(ANALYSIS_OUT_DIR, '1-Descriptives-Output', '2-Tobacco-Balance-Table')
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Read input data file
dat_balance_table <- read_csv(file.path(DATA_OUT_DIR, 'dat_analysis_balance-table.csv'), show_col_types = FALSE)

# Count participants who listed tobacco as primary
dat_tobac_cnts <- dat_balance_table |>
  group_by(group) |>
  summarize(n_tobac_primary = sum(psub_1 == 11,  na.rm=TRUE),
            p_tobac_primary = n_tobac_primary / n(),
            n_tobac_secondary = sum(psub_2 == 11,  na.rm=TRUE),
            p_tobac_secondary = n_tobac_secondary / n(),
            n_tobac_tertiary = sum(psub_3 == 11, na.rm=TRUE),
            p_tobac_tertiary = n_tobac_tertiary / n(),
            n_tobac_any = sum(psub_1 == 11 | psub_2 == 11 | psub_3 == 11,  na.rm=TRUE),
            p_tobac_any = n_tobac_any / n(),
            n_tobac_primary_only = sum(psub_1 == 11 & psub_2 != 11 & psub_3 != 11,  na.rm=TRUE),
            p_tobac_primary_only = n_tobac_primary_only / n(),
            n_tobac_primary_or_secondary = sum(psub_1 == 11 | psub_2 == 11 & psub_3 != 11,  na.rm=TRUE),
            p_tobac_primary_or_secondary = n_tobac_primary_or_secondary / n())
write.csv(dat_tobac_cnts, file.path(save_dir, 'cnts.csv'))

# Descriptives of people who listed tobacco as primary and/or secondary 
dat_tobac_grp <- dat_balance_table |>
  mutate(tobac_grp = case_when(
    psub_1 == 11 | psub_2 == 1 ~ 0,
    TRUE ~ 1
  ))

get_balance(dat_tobac_grp, 
            'tobac_grp', 
            c('group', 'sex', 'age', 'sipad', 'phq', 'gad', 'bscq', 'dast', 'crave'), 
            save_prefix='tobacco',
            save_dir = save_dir)

csq_grp1_items <- str_c('eot_csq_grp1', c(1, 2, 3, 4, 5, 6, 7, 8), sep='_')
csq_grp2_items <- str_c('eot_csq_grp2', c(1, 2, 3, 4, 5, 6, 7, 8), sep='_')

get_descriptive_summary(dat_tobac_grp |> filter(retained_eot == 1, group == 1), 
                        c('eot_csq_grp1', csq_grp1_items),
                        grouping_var = 'tobac_grp', 
                        save_prefix = 'csq_grp1_eot-retained',
                        save_dir = save_dir)
get_descriptive_summary(dat_tobac_grp |> filter(retained_eot == 1, group == 2), 
                        c('eot_csq_grp2', csq_grp2_items),
                        grouping_var = 'tobac_grp', 
                        save_prefix = 'csq_grp2_eot-retained',
                        save_dir = save_dir)


