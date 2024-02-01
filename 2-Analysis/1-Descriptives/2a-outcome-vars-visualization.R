source('0-Config/0-config.R')

save_dir <- file.path(ANALYSIS_OUT_DIR, '1-Descriptives-Output', '2a-Outcome-Vars-Visualization')

if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

dat_analysis_descriptive <- read_csv(file.path(DATA_OUT_DIR, 'dat_analysis_descriptive.csv'), show_col_types=FALSE)

dat_plot <- dat_analysis_descriptive |>
  filter(retained_eot == 1) |>
  mutate_at(c('group', 'screen'), as.factor) |>
  mutate(group = if_else(group == 1, "W-SUDS", "Psychoed"),
         screen = if_else(screen == 1, "rcc", "qualtrics"))

dat_plot_wide <- dat_plot |>
  pivot_longer(starts_with("eot_p30_"), 
               names_to = "substance",
               values_to = "days",
               names_prefix="eot_p30_") |>
  filter(!is.na(days))

# boxplots
ggplot(dat_plot_wide, aes(x=substance, y=days, fill=group)) +
  geom_boxplot() +
  ylab("Days of Use (8-wks)") +
  facet_grid(rows = vars(group))
ggsave(file.path(save_dir, 'boxplot_days_eot_by_group.png'), width=5, height=5)

# histograms
ggplot(dat_plot, aes(x=p30, fill=group, color=group)) + 
  geom_histogram(alpha=0.4, position="identity", binwidth=2) + 
  xlab("Substance Use Occasions (Baseline)") +
  facet_grid(rows = vars(group)) +
  theme_classic()
ggsave(file.path(save_dir, 'hist_days_baseline_by_group.png'), width=5, height=5)

ggplot(dat_plot, aes(x=eot_p30, fill=group, color=group)) + 
  geom_histogram(alpha=0.4, position="identity", binwidth=2) + 
  xlab("Substance Use Occasions (8-wks)") +
  facet_grid(rows = vars(group)) +
  theme_classic()
ggsave(file.path(save_dir, 'hist_days_eot_by_group.png'), width=5, height=5)

ggplot(dat_plot, aes(x=delta_eot_p30, fill=group, color=group)) + 
  geom_histogram(alpha=0.4, position="identity", binwidth=2) + 
  xlab("Change in Substance Use Occasions (8-wks - Baseline)") +
  facet_grid(rows = vars(group)) +
  theme_classic()
ggsave(file.path(save_dir, 'hist_change_by_group.png'), width=5, height=5)

# connected scatter
dat_plot_long <- dat_plot |>
  select(participant_id, group, screen, p30, eot_p30) |>
  pivot_longer(-c(participant_id, group, screen),
               names_to = 'timepoint',
               values_to = 'days') |>
  mutate(timepoint =
           case_when(timepoint == 'p30' ~ 'baseline',
                     .default = 'eot'))

ggplot(dat_plot_long, aes(x=timepoint, y=days, color=group, group=participant_id)) +
  geom_point(alpha=0.5) +
  geom_line(alpha=0.5) +
  ylab('Substance Use Occasions') +
  facet_grid(cols=vars(group)) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
ggsave(file.path(save_dir, 'line_change_by_group.png'), width=5, height=5)

# correlation plot
dat_plot_zero_fill_p30 <- dat_plot |>
  mutate(across(starts_with('p30'), ~replace_na(.x, 0)))
p30_correlations <- cor(dat_plot_zero_fill_p30 |> select(all_of(starts_with('p30_'))))
pdf(file.path(save_dir, 'corrplot_p30.pdf'), width=5, height=5)
p_corr <- corrplot(p30_correlations, method='number', type='lower')
dev.off()