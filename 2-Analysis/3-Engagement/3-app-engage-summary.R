source('0-Config/0-config.R')

# Define directory for saving results
save_dir <- file.path(ANALYSIS_OUT_DIR, '3-Engagement-Output', '3-App-Engage')

if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Read input data files
dat_rand_ret_base <- read_csv(file.path(DATA_OUT_DIR, 'dat_analysis_rand-ret-base.csv'), show_col_types=FALSE) |>
  filter(group == 1)

dat_engage_long <- dat_rand_ret_base |>
  pivot_longer(starts_with("days_active_w"),
               names_prefix = "days_active_w",
               names_to="week_num",
               values_to="days_active") |>
  mutate(is_active = days_active > 0) |>
  select(participant_id, emailaddress, week_num, days_active, is_active)
 
n_participants <- length(unique(dat_engage_long$participant_id))

weeks_active_summary <- dat_engage_long |>
   group_by(participant_id) |>
   summarize(n_weeks_active = sum(is_active)) |>
   group_by(n_weeks_active) |>
   summarize(n = n(),
             p = n / n_participants)
write.csv(weeks_active_summary, 
          file.path(save_dir, 'summary_weeks_active_by_n_weeks.csv'), row.names=FALSE)
weeks_active_summary_agg_weeks <- dat_engage_long |>
  group_by(participant_id) |>
  summarize(n_weeks_active = sum(is_active)) |>
  summarize(mean_n_weeks_active = mean(n_weeks_active, na.rm=TRUE),
            sd_n_weeks_active = sd(n_weeks_active, na.rm=TRUE),
            med_n_weeks_active = median(n_weeks_active, na.rm=TRUE))
write.csv(weeks_active_summary_agg_weeks, 
          file.path(save_dir, 'summary_weeks_active.csv'), row.names=FALSE)
 
days_active_summary <- dat_engage_long |>
   group_by(week_num) |>
   summarize(n_active = sum(is_active, na.rm=TRUE),
             p_active = n_active / n(),
             mean_days_active = mean(days_active, na.rm=TRUE),
             sd_days_active = sd(days_active, na.rm=TRUE),
             median_days_active = median(days_active, na.rm=TRUE))

write.csv(days_active_summary, 
          file.path(save_dir, 'summary_days_active.csv'), row.names=FALSE)

app_engage_dat <- read_excel(app_engage_path, sheet='sud2 raw data') |>
  mutate(emailaddress = tolower(emailaddress))
in_engage_only <- app_engage_dat |>
  anti_join(dat_rand_ret_base, by=c("emailaddress")) |>
  select(emailaddress)
in_survey_only <- dat_rand_ret_base |>
  anti_join(app_engage_dat, by=c("emailaddress")) |>
  select(emailaddress)

ggplot(dat_engage_long, mapping=aes(x=week_num, y=days_active)) +
  geom_boxplot()
