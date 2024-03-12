source('0-Config/0-config.R')

# Define directory for saving results
save_dir <- file.path(ANALYSIS_OUT_DIR, '3-Engagement-Output', '2-Control-PDF')

if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Read input data files
dat_rand_ret_base <- read_csv(file.path(DATA_OUT_DIR, 'dat_analysis_rand-ret-base.csv'), show_col_types=FALSE)

# summarize weekly engagement in control group
dat_pdf_summary <- dat_rand_ret_base |> 
  mutate(actual = rowSums(across(w1_complete:w8_complete), na.rm=TRUE)) |>
  select(participant_id, actual)

# check for consistency with self-report
dat_pdf_self_rep_actual <- dat_rand_ret_base |>
  subset_has_eot() |>
  filter(group == 2) |>
  left_join(dat_pdf_summary, by = c('participant_id')) |>
  rename(self_report = eot_pdf_cnt) |>
  mutate(discrep = actual - self_report) |>
  select(participant_id, actual, self_report, discrep)

# Plot self-report and actual engagement distributions
dat_pdf_self_rep_actual_long <- dat_pdf_self_rep_actual |>
  pivot_longer(cols=c(actual, self_report, discrep), 
               names_to='measure', 
               values_to='cnt')

dat_pdf_summary_by_measure_and_cnt <- dat_pdf_self_rep_actual_long |>
  group_by(measure, cnt) |>
  summarize(n = n(), .groups='keep') |>
  ungroup(cnt) |>
  mutate(p = n / sum(n) * 100) |>
  ungroup()
write.csv(dat_pdf_summary_by_measure_and_cnt, 
          file.path(save_dir, 'summary_by_measure_and_cnt.csv'), row.names=FALSE)

dat_pdf_summary_by_measure <- dat_pdf_self_rep_actual_long |>
  group_by(measure) |>
  summarize(mean_cnt = mean(cnt, na.rm=TRUE), 
            sd_cnt = sd(cnt, na.rm=TRUE),
            median_cnt = median(cnt, na.rm=TRUE),
            n_atleast_4 = sum(cnt >= 4, na.rm=TRUE),
            p_atleast_4 = n_atleast_4 / n() * 100, .groups='drop')
write.csv(dat_pdf_summary_by_measure, 
          file.path(save_dir, 'summary_by_measure.csv'), row.names=FALSE)
