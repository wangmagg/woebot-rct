source('0-Config/0-config.R')

# Define directory for saving results
save_dir <- file.path(ANALYSIS_OUT_DIR, '1-Descriptives-Output', '1c-Outliers')
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Read input file
dat_rand_base <- read_csv(file.path(DATA_OUT_DIR,'dat_analysis_descriptive.csv'), show_col_types=FALSE)

outliers <- dat |> 
  filter(p30 > 90) |>
  select(participant_id, starts_with('p30'))

write.csv(outliers, file.path(save_dir, 'outliers.csv'), row.names=FALSE)