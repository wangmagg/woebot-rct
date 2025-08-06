source('Analysis/config.R')

# Load helper functions
for (file in list.files('Analysis/0-Functions', full.names=TRUE)) {
  source(file)
}

# Load analytic sample
dat_analysis <- read_csv('Data/dat_analysis_share.csv')

# Get participants that were randomized
dat_rand <- dat_analysis |> 
  subset_randomized()

# Get participants that were randomized and retained
dat_rand_ret <- dat_analysis |> 
  subset_randomized() |>
  subset_retained()

# Get participants that were randomized and completed baseline survey
dat_rand_base <- dat_rand |>
  subset_has_baseline() |>
  subset_has_p30()

# Get participants that were randomized, had baseline, did not withdraw, and had non-zero substance use
dat_rand_ret_base <- dat_analysis |>
  subset_randomized() |>
  subset_has_baseline() |>
  subset_retained() |>
  subset_has_p30()

# Get participants that were randomized and had baseline, but withdrew or had zero substance use
dat_rand_not_ret_base <- dat_analysis |>
  subset_randomized() |>
  subset_has_baseline() |>
  subset_not_retained_no_p30() 

# Construct data for missingness analysis
dat_missingness <- dat_rand_base |>
  collapse_multi() |>
  set_composite_sums(P30_VARS, exclude_substr = c('tob'), drop_items=FALSE, na_rm=TRUE) |>
  set_composite_sums(SUMMED_COMPOSITE_VARS, drop_items = FALSE) |>
  set_composite_means(AVERAGED_COMPOSITE_VARS, drop_items = FALSE) |>
  set_retention_status(c('mid', 'eot','followup')) 

# Construct data for balance table
dat_balance_table <- dat_rand_ret_base |>
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
  set_retention_status(c('mid', 'eot', 'followup'))

# Construct data for withdrawn participant balance table
dat_not_ret_balance_table <- dat_rand_not_ret_base |>
  set_therapy_status() |>
  reverse_code_sps_items() |>
  reverse_code_urpi_items() |>
  fill_99_with_na(FILL_99_WITH_NA_VARS) |>
  set_composite_sums(P30_VARS, exclude_substr = c('tob'), drop_items=FALSE, na_rm=TRUE) |>
  set_composite_sums(SUMMED_COMPOSITE_VARS, drop_items = FALSE) |>
  set_eot_csq () |>
  set_composite_means(AVERAGED_COMPOSITE_VARS, drop_items = FALSE) |>
  set_composite_products(MULTIPLIED_COMPOSITE_VARS, drop_items=FALSE)
  

# Construct data for descriptive analyses
dat_descriptive <- dat_rand_base |>
  set_therapy_status() |>
  set_pst_p30() |>
  collapse_multi() |>
  reverse_code_sps_items() |>
  reverse_code_urpi_items() |>
  fill_99_with_na(FILL_99_WITH_NA_VARS) |>
  fill_na_with_mean(SUMMED_COMPOSITE_VARS) |>
  set_composite_sums(P30_VARS, exclude_substr = c('tob'), drop_items=FALSE, na_rm=TRUE) |>
  fill_na_with_0(P30_VARS) |> # if '{timept}_p30' is NA, assume it is 0
  set_composite_sums(PST_P30_VARS, drop_items=FALSE, na_rm=TRUE) |>
  fill_na_with_0(PST_P30_VARS) |> # if '{timept}_p30' is NA, assume it is 0
  set_composite_sums(SUMMED_COMPOSITE_VARS, drop_items=FALSE) |>
  set_eot_csq () |>
  set_composite_means(AVERAGED_COMPOSITE_VARS, drop_items=FALSE) |>
  set_composite_products(MULTIPLIED_COMPOSITE_VARS, drop_items=FALSE) |>
  set_delta_vars(MID_OUTCOME_VARS, 'mid') |>
  set_delta_vars(EOT_OUTCOME_VARS, 'eot') |>
  set_delta_vars(FOLLOWUP_OUTCOME_VARS, 'followup') |>
  set_retention_status(c('mid', 'eot', 'followup')) 
  
# Construct data for primary and secondary outcome regression
dat_outcome_regression <- dat_rand_base |>
  set_therapy_status() |>
  set_pst_p30() |>
  collapse_multi() |>
  reverse_code_sps_items() |>
  reverse_code_urpi_items() |>
  fill_99_with_na(FILL_99_WITH_NA_VARS) |>
  set_composite_sums(P30_VARS, exclude_substr = c('tob'), drop_items=FALSE, na_rm=TRUE) |>
  fill_na_with_0(P30_VARS) |> # if '{timept}_p30' is missing, assume it is 0
  set_composite_sums(PST_P30_VARS, drop_items=FALSE, na_rm=TRUE) |>
  fill_na_with_0(PST_P30_VARS) |> # if '{timept}_p30' is missing, assume it is 0
  fill_na_with_mean(SUMMED_COMPOSITE_VARS) |> # impute missing items with mean of non-missing items
  set_composite_sums(SUMMED_COMPOSITE_VARS, drop_items=FALSE) |>
  set_eot_csq() |>
  set_composite_means(AVERAGED_COMPOSITE_VARS, drop_items=FALSE) |>
  set_composite_products(MULTIPLIED_COMPOSITE_VARS, drop_items=FALSE) |>
  recode_sps_for_branching(c('sps', 'eot_sps')) |>
  recode_dast_for_branching(c('dast', 'eot_dast')) |>
  set_delta_vars(MID_OUTCOME_VARS, 'mid') |>
  set_delta_vars(EOT_OUTCOME_VARS, 'eot') |>
  set_delta_vars(FOLLOWUP_OUTCOME_VARS, 'followup') |>
  set_retention_status(c('mid', 'eot', 'followup')) 
  
# Create data directory if it doesn't exist
if (!dir.exists(DATA_OUT_DIR)) {
  dir.create(DATA_OUT_DIR, recursive = TRUE)
}

# Save all data files
write.csv(dat_rand, file=file.path(DATA_OUT_DIR, 'dat_analysis_rand.csv'), row.names=F)
write.csv(dat_rand_ret, file=file.path(DATA_OUT_DIR, 'dat_analysis_rand-ret.csv'), row.names=F)
write.csv(dat_rand_base, file=file.path(DATA_OUT_DIR, 'dat_analysis_rand-base.csv'), row.names=F)
write.csv(dat_rand_ret_base, file=file.path(DATA_OUT_DIR, 'dat_analysis_rand-ret-base.csv'), row.names=F)
write.csv(dat_missingness, file=file.path(DATA_OUT_DIR, 'dat_analysis_missingness.csv'), row.names=F)
write.csv(dat_balance_table, file=file.path(DATA_OUT_DIR, 'dat_analysis_balance-table.csv'), row.names=F)
write.csv(dat_descriptive, file=file.path(DATA_OUT_DIR, 'dat_analysis_descriptive.csv'), row.names=F)
write.csv(dat_outcome_regression, file=file.path(DATA_OUT_DIR, 'dat_analysis_outcome-regression.csv'), row.names=F)
