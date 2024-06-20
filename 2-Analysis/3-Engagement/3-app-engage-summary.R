source('0-Config/0-config.R')

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Define directory for saving results
save_dir <- file.path(ANALYSIS_OUT_DIR, '3-Engagement-Output', '3-App-Engage')

if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Read input data files
dat_rand_ret_base <- read_csv(file.path(DATA_OUT_DIR, 'dat_analysis_rand-ret-base.csv'), show_col_types=FALSE) |>
  filter(group == 1) |>
  set_retention_status(c('mid', 'eot', 'followup'))
dat_rand_base <- read_csv(file.path(DATA_OUT_DIR, 'dat_analysis_rand-base.csv'), show_col_types=FALSE) |>
  filter(group == 1) |>
  set_retention_status(c('mid', 'eot', 'followup'))

# Get engagement dataframes
metric_cols <- c('days_active',
                 'user_messages_sent',
                 'stories_started',
                 'stories_completed',
                 'tools_started',
                 'tools_completed',
                 'stories_rated_positive',
                 'stories_rated_negative',
                 'tools_rated_positive',
                 'tools_rated_negative',
                 'tools_rated_neutral',
                 'moods_logged')

dat_engage_long <- NULL
for (col in metric_cols) {
  any_col <- str_c('any_', col)
  dat_engage_long_col <- dat_rand_ret_base |>
    pivot_longer(starts_with(col),
                 names_prefix = str_c(col, '_w'),
                 names_to = "week_num",
                 values_to = col) |>
    mutate(!!sym(any_col) := ifelse(!is.na(!!sym(col)), !!sym(col) > 0, NA)) |>
    select(participant_id, emailaddress, week_num, !!sym(col), !!sym(any_col))
  if (is.null(dat_engage_long)) {
    dat_engage_long <- dat_engage_long_col
  } else {
    dat_engage_long <- dat_engage_long |>
      left_join(dat_engage_long_col, by=c("participant_id", "emailaddress", "week_num"))
  }
}

dat_engage_long <- dat_engage_long |>
  mutate(frac_stories_rated_positive = stories_rated_positive / stories_completed,
         any_frac_stories_rated_positive = any_stories_completed,
         frac_stories_rated_negative = stories_rated_negative / stories_completed,
         any_frac_stories_rated_negative = any_stories_completed,
         frac_tools_rated_positive = tools_rated_positive / tools_completed,
         any_frac_tools_rated_positive = any_tools_completed,
         frac_tools_rated_negative = tools_rated_negative / tools_completed,
         any_frac_tools_rated_negative = any_tools_completed,
         frac_tools_rated_neutral = tools_rated_neutral / tools_completed,
         any_frac_tools_rated_neutral = any_tools_completed) 

frac_ratings_cols <- c('frac_stories_rated_positive',
                       'frac_stories_rated_negative',
                       'frac_tools_rated_positive',
                       'frac_tools_rated_negative',
                       'frac_tools_rated_neutral')

metric_cols <- c(metric_cols, frac_ratings_cols)
  
# Summarize number of participants with certain number of weeks active
n_participants <- length(unique(dat_engage_long$participant_id))
weeks_active_summary <- dat_engage_long |>
   group_by(participant_id) |>
   summarize(n_weeks_active = sum(any_days_active)) |>
   group_by(n_weeks_active) |>
   summarize(n = n(),
             p = n / n_participants)
write.csv(weeks_active_summary, 
          file.path(save_dir, 'summary_weeks_active_by_n_weeks.csv'), row.names=FALSE)

weeks_active_summary_agg_weeks <- dat_engage_long |>
  group_by(participant_id) |>
  summarize(n_weeks_active = sum(any_days_active)) |>
  summarize(n_four_or_more_weeks_active = sum(n_weeks_active >= 4, na.rm=TRUE),
            p_four_or_more_weeks_active = n_four_or_more_weeks_active / n_participants,
            mean_n_weeks_active = mean(n_weeks_active, na.rm=TRUE),
            sd_n_weeks_active = sd(n_weeks_active, na.rm=TRUE),
            med_n_weeks_active = median(n_weeks_active, na.rm=TRUE),
            mode_n_weeks_active = getmode(n_weeks_active),
            min_n_weeks_active = min(n_weeks_active, na.rm=TRUE),
            max_n_weeks_active = max(n_weeks_active, na.rm=TRUE))
write.csv(weeks_active_summary_agg_weeks, 
          file.path(save_dir, 'summary_weeks_active_agg.csv'), row.names=FALSE)
 
# Summarize metrics per week 
any_metric_cols <- str_c('any', metric_cols, sep='_')
any_metric_summary_per_week <- dat_engage_long |>
  group_by(week_num) |>
  summarize(across(all_of(any_metric_cols),
                   .fns = list(n=~sum(., na.rm=TRUE),
                               p=~sum(., na.rm=TRUE) / n())))
write.csv(any_metric_summary_per_week, 
          file.path(save_dir, 'summary_any_per_week.csv'), row.names=FALSE)

metric_summary_per_week <- NULL
for (col in metric_cols) {
  any_col <- str_c('any_', col)
  
  col_summ <- dat_engage_long |>
    filter(!!sym(any_col)) |>
    group_by(week_num) |>
    summarize(across(all_of(col),
                     .fns = list(mean=~mean(.x, na.rm=TRUE), 
                                 sd=~sd(.x, na.rm=TRUE),
                                 median=~median(.x, na.rm=TRUE),
                                 mode=~getmode(.x),
                                 min=~min(.x, na.rm=TRUE),
                                 max=~max(.x, na.rm=TRUE)),
                     .names = '{.col}_{.fn}'))
  if (is.null(metric_summary_per_week)) {
    metric_summary_per_week <- col_summ
  } else {
    metric_summary_per_week <- metric_summary_per_week |>
      left_join(col_summ, by="week_num")
  }
}
write.csv(metric_summary_per_week,
          file.path(save_dir, 'summary_per_week.csv'), row.names=FALSE)

# Summarize overall metrics across weeks
dat_engage_long_any_metric_agg <- dat_engage_long |>
  group_by(participant_id) |>
  summarize(across(all_of(any_metric_cols),
                   .fns = list(sum=~sum(.) > 0),
                   .names = '{.col}'))
dat_engage_long_metric_agg <- dat_engage_long |>
  group_by(participant_id) |>
  summarize(across(all_of(metric_cols),
                   .fns = list(sum=~sum(., na.rm=TRUE)),
                   .names = '{.col}')) |>
  mutate(frac_stories_rated_positive = stories_rated_positive / stories_completed,
         frac_stories_rated_negative = stories_rated_negative / stories_completed,
         frac_tools_rated_positive = tools_rated_positive / tools_completed,
         frac_tools_rated_negative = tools_rated_negative / tools_completed,
         frac_tools_rated_neutral = tools_rated_neutral / tools_completed) 

dat_engage_long_agg <- dat_engage_long_any_metric_agg |>
  left_join(dat_engage_long_metric_agg, by=c("participant_id"))

any_metric_summary <- dat_engage_long_agg |>
  summarize(across(all_of(any_metric_cols),
                   .fns = list(n=~sum(., na.rm=TRUE),
                               p=~sum(., na.rm=TRUE) / n()),
                   .names = '{.fn}_{.col}')) 
write.csv(any_metric_summary, file.path(save_dir, 'summary_any.csv'), row.names=FALSE)

metric_summary <- c()
for (col in metric_cols) {
  any_col <- str_c('any_', col)
  col_summ <- dat_engage_long_agg |>
    filter(!!sym(any_col)) |>
    summarize(across(all_of(col),
                     .fns = list(mean=~mean(.x, na.rm=TRUE), 
                                 sd=~sd(.x, na.rm=TRUE),
                                 median=~median(.x, na.rm=TRUE),
                                 mode=~getmode(.x),
                                 min=~min(.x, na.rm=TRUE),
                                 max=~max(.x, na.rm=TRUE)),
                     .names = '{.fn}')) |>
    mutate(var=col)
  metric_summary <- bind_rows(metric_summary, col_summ)
}

write.csv(metric_summary, file.path(save_dir, 'summary.csv'), row.names=FALSE)

# Summarize percentage positive/negative/neutral ratings
rating_summary <- dat_engage_long_agg |>
  summarize(stories_completed = sum(stories_completed, na.rm=TRUE),
            stories_rated_positive = sum(stories_rated_positive, na.rm=TRUE),
            stories_rated_negative = sum(stories_rated_negative, na.rm=TRUE),
            tools_completed = sum(tools_completed, na.rm=TRUE),
            tools_rated_positive = sum(tools_rated_positive, na.rm=TRUE),
            tools_rated_negative = sum(tools_rated_negative, na.rm=TRUE),
            tools_rated_neutral = sum(tools_rated_neutral, na.rm=TRUE)) |>
  mutate(p_stories_positive = stories_rated_positive / stories_completed,
         p_stories_negative = stories_rated_negative / stories_completed,
         p_tools_positive = tools_rated_positive / tools_completed,
         p_tools_negative = tools_rated_negative / tools_completed,
         p_tools_neutral = tools_rated_neutral / tools_completed)

write.csv(rating_summary, file.path(save_dir, 'ratings.csv'), row.names=FALSE)

# Examine inconsistencies in survey and engagement data
app_engage_dat <- read_excel(app_engage_path, sheet='sud2 raw data') |>
  mutate(emailaddress = tolower(emailaddress))
ret_in_engage_only <- app_engage_dat |>
  anti_join(dat_rand_ret_base, by=c("emailaddress")) |>
  select(emailaddress)
write.csv(ret_in_engage_only, file.path(save_dir, 'ret_in_engage_only.csv'))

ret_in_survey_only <- dat_rand_ret_base |>
  anti_join(app_engage_dat, by=c("emailaddress")) |>
  select(participant_id, emailaddress, retained_eot, retained_mid, retained_followup)
write.csv(ret_in_survey_only, file.path(save_dir, 'ret_in_survey_only.csv'))

in_engage_only <- app_engage_dat |>
  anti_join(dat_rand_base, by=c("emailaddress")) |>
  select(emailaddress)
write.csv(in_engage_only, file.path(save_dir, 'in_engage_only.csv'))

in_survey_only <- dat_rand_base |>
  anti_join(app_engage_dat, by=c("emailaddress")) |>
  select(participant_id,emailaddress, retained_eot, retained_mid, retained_followup)
write.csv(ret_in_engage_only, file.path(save_dir, 'in_survey_only.csv'))

