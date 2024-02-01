source('0-Config/0-config.R')

dat_combined_raw_q <- load_data_raw(rcc_rand_path, 
                                      rcc_baseline_path, 
                                      rcc_withdraw_path, 
                                      rcc_mid_path, 
                                      rcc_eot_path, 
                                      rcc_followup_path,
                                      id_rm_wd_path,
                                      screen_type = 'q',
                                      screen_path = q_screen_path)

dat_combined_raw_rcc <- load_data_raw(rcc_rand_path, 
                                      rcc_baseline_path, 
                                      rcc_withdraw_path, 
                                      rcc_mid_path, 
                                      rcc_eot_path, 
                                      rcc_followup_path,
                                      id_rm_wd_path,
                                      screen_type = 'rcc',
                                      screen_path = rcc_screen_path)


dat_analysis <- load_data_analysis(rcc_rand_path, 
                                   rcc_baseline_path, 
                                   rcc_withdraw_path, 
                                   rcc_mid_path, 
                                   rcc_eot_path, 
                                   rcc_followup_path,
                                   id_rm_wd_path,
                                   q_screen_path, 
                                   rcc_screen_path,
                                   var_name_mapping_path) 

# Randomized and has baseline
dat_rand_base <- dat_analysis |>
  subset_randomized() |>
  subset_has_baseline() |>
  subset_has_p30()

# Randomized, retained, has baseline
dat_rand_ret_base <- dat_analysis |>
  subset_randomized() |>
  subset_has_baseline() |>
  subset_retained() |>
  subset_has_p30()

# Construct data for missingness analysis
dat_missingness <- dat_rand_base |>
  collapse_multi() |>
  set_composite_sums(P30_VARS, exclude_substr = c('tob'), drop_items=FALSE, na_rm=TRUE) |>
  set_composite_sums(SUMMED_COMPOSITE_VARS, drop_items = FALSE) |>
  set_composite_means(AVERAGED_COMPOSITE_VARS, drop_items = FALSE) |>
  set_retention_status(c('eot', 'mid', 'followup')) 

# Construct data for balance table
dat_balance_table <- dat_rand_ret_base |>
  set_therapy_status() |>
  reverse_code_sps_items() |>
  fill_99_with_na(c(P30_VARS, SUMMED_COMPOSITE_VARS, MULTIPLIED_COMPOSITE_VARS, AVERAGED_COMPOSITE_VARS)) |>
  set_composite_sums(SUMMED_COMPOSITE_VARS, drop_items = FALSE) |>
  set_composite_sums(P30_VARS, exclude_substr = c('tob'), drop_items=FALSE, na_rm=TRUE) |>
  set_composite_means(AVERAGED_COMPOSITE_VARS, drop_items = FALSE) |>
  set_composite_products(MULTIPLIED_COMPOSITE_VARS, drop_items=FALSE) |>
  set_retention_status(c('eot', 'mid', 'followup')) 

# Construct data for descriptive analyses
dat_descriptive <- dat_rand_base |>
  set_therapy_status() |>
  collapse_multi() |>
  reverse_code_sps_items() |>
  fill_99_with_na(c(P30_VARS, SUMMED_COMPOSITE_VARS, MULTIPLIED_COMPOSITE_VARS)) |>
  fill_na_with_mean(SUMMED_COMPOSITE_VARS) |>
  set_composite_sums(SUMMED_COMPOSITE_VARS, drop_items=FALSE) |>
  set_composite_sums(P30_VARS, exclude_substr = c('tob'), drop_items=FALSE, na_rm=TRUE) |>
  fill_na_with_0(P30_VARS) |> # if '{timept}_p30' is missing, assume it is 0
  set_composite_means(AVERAGED_COMPOSITE_VARS, drop_items=FALSE) |>
  set_composite_products(MULTIPLIED_COMPOSITE_VARS, drop_items=FALSE) |>
  set_delta_vars(EOT_OUTCOME_VARS, 'eot') |>
  set_delta_vars(MID_OUTCOME_VARS, 'mid') |>
  set_delta_vars(FOLLOWUP_OUTCOME_VARS, 'followup') |>
  set_retention_status(c('eot', 'mid', 'followup')) 
  
# Construct data for primary and secondary outcome regression
dat_outcome_regression <- dat_rand_base |>
  set_therapy_status() |>
  collapse_multi() |>
  reverse_code_sps_items() |>
  fill_99_with_na(c(P30_VARS, SUMMED_COMPOSITE_VARS, MULTIPLIED_COMPOSITE_VARS)) |>
  fill_na_with_mean(SUMMED_COMPOSITE_VARS) |>
  set_composite_sums(SUMMED_COMPOSITE_VARS, drop_items=FALSE) |>
  set_composite_sums(P30_VARS, exclude_substr = c('tob'), drop_items=FALSE, na_rm=TRUE) |>
  fill_na_with_0(P30_VARS) |> # if '{timept}_p30' is missing, assume it is 0
  set_composite_means(AVERAGED_COMPOSITE_VARS, drop_items=FALSE) |>
  set_composite_products(MULTIPLIED_COMPOSITE_VARS, drop_items=FALSE) |>
  recode_sps_for_branching(c('sps', 'eot_sps')) |>
  recode_dast_for_branching(c('dast', 'eot_dast')) |>
  set_delta_vars(EOT_OUTCOME_VARS, 'eot') |>
  set_delta_vars(MID_OUTCOME_VARS, 'mid') |>
  set_delta_vars(FOLLOWUP_OUTCOME_VARS, 'followup') |>
  set_retention_status(c('eot', 'mid', 'followup')) 
  
# Create data directory if it doesn't exist
if (!dir.exists(DATA_OUT_DIR)) {
  dir.create(DATA_OUT_DIR)
}

# Save data files
write.csv(dat_combined_raw_q, file=file.path(DATA_OUT_DIR, 'dat_combined_raw_qualtrics.csv'), row.names=F)
write.csv(dat_combined_raw_rcc, file=file.path(DATA_OUT_DIR, 'dat_combined_raw_rcc.csv'), row.names=F)
write.csv(dat_analysis, file=file.path(DATA_OUT_DIR, 'dat_analysis.csv'), row.names=F)
write.csv(dat_rand_base, file=file.path(DATA_OUT_DIR, 'dat_analysis_rand-base.csv'), row.names=F)
write.csv(dat_rand_ret_base, file=file.path(DATA_OUT_DIR, 'dat_analysis_rand-ret-base.csv'), row.names=F)

write.csv(dat_missingness, file=file.path(DATA_OUT_DIR, 'dat_analysis_missingness.csv'), row.names=F)
write.csv(dat_balance_table, file=file.path(DATA_OUT_DIR, 'dat_analysis_balance-table.csv'), row.names=F)
write.csv(dat_descriptive, file=file.path(DATA_OUT_DIR, 'dat_analysis_descriptive.csv'), row.names=F)
write.csv(dat_outcome_regression, file=file.path(DATA_OUT_DIR, 'dat_analysis_outcome-regression.csv'), row.names=F)
