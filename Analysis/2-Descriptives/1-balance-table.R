source('Analysis/config.R')

# Load helper functions
for (file in list.files('Analysis/0-Functions', full.names=TRUE)) {
  source(file)
}

analysis_types <- c("itt", "perprot")

for (analysis_type in analysis_types) {
  # Define directory for saving results
  save_dir <- file.path(ANALYSIS_OUT_DIR, '2-Descriptives-Output', '1-Balance-Tables', analysis_type)
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Read input data file
  dat_balance_table <- read_csv(file.path(DATA_OUT_DIR, 'dat_analysis_balance-table.csv'), show_col_types=FALSE)
  if (analysis_type == "perprot") {
    dat_balance_table <- dat_balance_table |>
      subset_per_protocol()
  }
  
  var_names <- c('group', 'age', 'sex', 'gender', 'orient', 'eth', 'race',
                 'marital', 'educ', 'empl', 'disab', 'insur',
                 'ther', 'trt', 'med', 'psych_med',
                 'mh', 'phq', 'gad', 'sps', 'pst_sub',
                 'dast', 'sipad', 'cageaid', 'bscq', 'crave', 
                 'taa_1', 'taa_2', 'taa_3', 'taa_4', 
                 'qds', 'p30_per_sub', 'p30')
  
  get_balance(dat_balance_table, c('group'), 
              var_names, save_prefix='grouped', save_dir, get_smd=TRUE)
  get_balance(dat_balance_table, c(), 
              var_names, save_prefix='all', save_dir, get_smd=FALSE)
}
