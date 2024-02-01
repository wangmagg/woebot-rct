source('0-Config/0-config.R')

# Define directory for saving results
save_dir <- file.path(ANALYSIS_OUT_DIR, '1-Descriptives-Output', '1a-Balance-Tables')
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Read input data file
dat_balance_table <- read_csv(file.path(DATA_OUT_DIR, 'dat_analysis_balance-table.csv'), show_col_types=FALSE)

var_names <- c('group', 'age', 'sex', 'gender', 'orient', 'eth', 'race',
               'marital', 'educ', 'empl', 'disab', 'insur',
               'ther', 'trt', 'med', 'psych_med',
               'mh', 'phq', 'gad', 'sps', 'pst_sub', 'pri_sub', 'sec_sub', 'ter_sub',
               'dast', 'sipad', 'cageaid', 'bscq', 'crave', 'taa', 'qds', 'p30_per_sub', 'p30', 'csq')

get_balance(dat_balance_table, c('group'), 
            var_names, save_prefix='grouped', save_dir)
get_balance(dat_balance_table, c(), 
            var_names, save_prefix='all', save_dir)
