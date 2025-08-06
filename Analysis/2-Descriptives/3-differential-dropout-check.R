source('Analysis/config.R')

# Load helper functions
for (file in list.files('Analysis/0-Functions', full.names=TRUE)) {
  source(file)
}

save_dir <- file.path(ANALYSIS_OUT_DIR, '2-Descriptives-Output', '3-Differential-Dropout-Check')

if (!dir.exists(save_dir)) {
  dir.create(save_dir)
}

# Get all randomized participants and label them by whether they were removed/withdrew
dat_rand <- read_csv(file=file.path(DATA_OUT_DIR, 'dat_analysis_rand.csv'), show_col_types=F) |>
  set_therapy_status() |>
  set_pst_p30() |>
  reverse_code_sps_items() |>
  reverse_code_urpi_items() |>
  fill_99_with_na(c(P30_VARS, SUMMED_COMPOSITE_VARS, MULTIPLIED_COMPOSITE_VARS, AVERAGED_COMPOSITE_VARS)) |>
  set_composite_sums(P30_VARS, exclude_substr = c('tob'), drop_items=FALSE, na_rm=TRUE) |>
  set_composite_sums(PST_P30_VARS, drop_items=FALSE, na_rm=TRUE) |>
  set_composite_sums(SUMMED_COMPOSITE_VARS, drop_items = FALSE) |>
  set_eot_csq () |>
  set_composite_means(AVERAGED_COMPOSITE_VARS, drop_items = FALSE) |>
  set_composite_products(MULTIPLIED_COMPOSITE_VARS, drop_items=FALSE) |>
  mutate(init_removed = (!is.na(withdraw) | participant_id == 58) & participant_id != 180,
         p30_zero = rowSums(!is.na(pick(starts_with('p30') & !contains('tob')))) == 0 |
                    rowSums(pick(starts_with('p30') & !contains('tob')), na.rm=TRUE) == 0,
         removed = init_removed | p30_zero)

# Perform z-test to check for differential dropout
z_test <- dat_rand |>
  group_by(group) |>
  summarize(n_remove = sum(removed),
            p_remove = n_remove / n(),
            n = n()) |>
  pivot_wider(names_from = group,
              values_from = c(n , n_remove, p_remove)) |>
  mutate(p_remove = (p_remove_1 * n_1 + p_remove_2 * n_2) / (n_1 + n_2),
         z_num = p_remove_1 - p_remove_2,
         z_den = sqrt(p_remove * (1-p_remove) * (1 / (n_1 + n_2))),
         z_stat = z_num / z_den,
         p_val = 2 * pnorm(abs(z_stat), lower.tail=FALSE))
write.csv(z_test, file.path(save_dir, 'z_test.csv'))

# Check SMD's before and after participant removal
var_names <- c('group', 'age', 'sex', 'gender', 'orient', 'eth', 'race',
               'marital', 'educ', 'empl', 'disab', 'insur',
               'ther', 'trt', 'med', 'psych_med',
               'mh', 'phq', 'gad', 'sps', 'pst_sub',
               'dast', 'sipad', 'cageaid', 'bscq', 'crave', 
               'taa_1', 'taa_2', 'taa_3', 'taa_4', 
               'qds', 'p30_per_sub', 'p30')

dat_rand_smd <- get_smd(dat_rand, c('group'), 
                        var_names, save_prefix='before_removal', save_dir) |>
  mutate(lbl='bef')
dat_rand_ret_smd <- get_smd(dat_rand |> filter(removed == 0), c('group'), 
                            var_names, save_prefix='after_removal', save_dir) |>
  mutate(lbl='after')

dat_smd <- bind_rows(dat_rand_smd, dat_rand_ret_smd) |>
  pivot_wider(names_from=lbl,
              values_from=smd) |>
  mutate(bef_u01 = abs(bef) <= 0.1,
         after_u01 = abs(after) <= 0.1,
         bef_u01_after_o01 = bef_u01 & !after_u01,
         bef_u02 = abs(bef) <= 0.2,
         after_u02 = abs(after) <= 0.2,
         bef_u02_after_o02 = bef_u02 & !after_u02,
         inc_by_01 = abs(after) - abs(bef) >= 0.1)

dat_smd |> filter(bef_u01_after_o01)
dat_smd |> filter(bef_u02_after_o02)
dat_smd |> filter(inc_by_01)



  
