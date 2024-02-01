# Define paths
rcc_exported_data_dir <- '/Users/maggiewang/Box/RCC Exported Data Final'
rcc_dict_dir <- '/Users/maggiewang/Box/RCC Data Dictionaries and Codebooks'

# Path to randomization info
rcc_rand_path <- file.path(rcc_exported_data_dir, 'Raw data Randomization and Withdrawals', 'CSV_Randomization_2023-08-18-003613529.csv')

# Path to withdrawal and removal info 
rcc_withdraw_path <- file.path(rcc_exported_data_dir, 'Raw data Randomization and Withdrawals', 'CSV_Withdrawn_Removed_2023-08-18-003350119.csv')

# Paths to raw baseline and eot survey data
rcc_baseline_path <- file.path(rcc_exported_data_dir, 'Raw Survey CSV data from RCC', 'CSV_Survey_1_2023-08-18-000319338.csv')
rcc_mid_path <- file.path(rcc_exported_data_dir, 'Raw Survey CSV data from RCC', 'CSV_Survey_2_Data_2023-08-18-000232857.csv')
rcc_eot_path <- file.path(rcc_exported_data_dir, 'Raw Survey CSV data from RCC', 'CSV_Survey_3_Data_2023-08-18-000123027.csv')
rcc_followup_path <- file.path(rcc_exported_data_dir, 'Raw Survey CSV data from RCC', 'CSV_Survey_4_Data_2023-08-17-235823355.csv')

# Path to data on PDF engagement in control group
rcc_pdf_path <- file.path(rcc_exported_data_dir, 'Raw data Group Info', 'CSV_Weekly_PDFs_2023-08-18-002923237.csv')

# Path to file with screening/subject ids and removal/withdrawal info
id_rm_wd_path <- file.path(rcc_dict_dir, 'WoebotPhase2_IDs_Removals.xlsx')


# Paths to screening data
q_screen_dir <- '/Users/maggiewang/Box/Screening Data/Raw Qualtrics File'
q_screen_path <- file.path(q_screen_dir, 'Woebot+Phase+2+Screening+Questionnaire_September+25,+2023_13.15.csv')
rcc_screen_path <- file.path(rcc_exported_data_dir, 'Raw Screening Data', 'CSV_All_data_2023-08-17-150005064.csv')

# Path to variable naming
var_name_mapping_path <- '0-Config/0-Files/var_name_mapping.xlsx'