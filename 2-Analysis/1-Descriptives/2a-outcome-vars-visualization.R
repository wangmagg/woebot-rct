source('0-Config/0-config.R')

analysis_types <- c("itt", "perprot")

for (analysis in analysis_types) {
  save_dir <- file.path(ANALYSIS_OUT_DIR, '1-Descriptives-Output', '2a-Outcome-Vars-Visualization', analysis)
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  dat_analysis_descriptive <- read_csv(file.path(DATA_OUT_DIR, 'dat_analysis_descriptive.csv'), show_col_types=FALSE)
  if (analysis_type == "perprot") {
    dat_analysis_descriptive <- dat_analysis_descriptive |>
      subset_per_protocol()
  }
  
  for (use_var in c('p30', 'pst_p30')) {
    dat_plot <- dat_analysis_descriptive |>
      filter(retained_eot == 1) |>
      mutate_at(c('group', 'screen'), as.factor) |>
      mutate(group = if_else(group == 1, "W-SUDS", "Psychoed"),
             screen = if_else(screen == 1, "rcc", "qualtrics"))
    
    # histograms
    ggplot(dat_plot, aes(x=!!ensym(use_var), fill=group, color=group)) + 
      geom_histogram(alpha=0.4, position="identity", binwidth=2) + 
      xlab("Substance Use Occasions (Baseline)") +
      facet_grid(rows = vars(group)) +
      theme_classic()
    fname <- sprintf('hist_%s_baseline_by_group.png', use_var)
    ggsave(file.path(save_dir, fname), width=5, height=5)
    
    eot_use_var <- str_c('eot_', use_var)
    ggplot(dat_plot, aes(x=!!ensym(eot_use_var), fill=group, color=group)) + 
      geom_histogram(alpha=0.4, position="identity", binwidth=2) + 
      xlab("Substance Use Occasions (8-wks)") +
      facet_grid(rows = vars(group)) +
      theme_classic()
    fname <- sprintf('hist_%s_eot_by_group.png', use_var)
    ggsave(file.path(save_dir, fname), width=5, height=5)
    
    delta_eot_use_var <- str_c('delta_eot_', use_var)
    ggplot(dat_plot, aes(x=!!ensym(delta_eot_use_var), fill=group, color=group)) + 
      geom_histogram(alpha=0.4, position="identity", binwidth=2) + 
      xlab("Change in Substance Use Occasions (8-wks - Baseline)") +
      facet_grid(rows = vars(group)) +
      theme_classic()
    fname <- sprintf('hist_change_%s_by_group.png', use_var)
    ggsave(file.path(save_dir, fname), width=5, height=5)
    
    # connected scatter
    dat_plot_long <- dat_plot |>
      select(participant_id, group, screen, 
             !!sym(use_var), !!sym(eot_use_var)) |>
      pivot_longer(-c(participant_id, group, screen),
                   names_to = 'timepoint',
                   values_to = 'days') |>
      mutate(timepoint =
               case_when(timepoint == use_var ~ 'baseline',
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
    fname <- sprintf('line_%s_by_group.png', use_var)
    ggsave(file.path(save_dir, fname), width=5, height=5)
    
    if (use_var == 'p30')  {
      dat_plot_wide <- dat_plot |>
        pivot_longer(starts_with(str_c("eot_", use_var, "_")),
                     names_to = "substance",
                     values_to = "days",
                     names_prefix=str_c("eot_", use_var, "_")) |>
        filter(!is.na(days))
      
      # boxplots
      ggplot(dat_plot_wide, aes(x=substance, y=days, fill=group)) +
        geom_boxplot() +
        ylab("Days of Use (8-wks)") +
        facet_grid(rows = vars(group))
      fname <- sprintf('boxplot_%s_eot_by_group.png', use_var)
      ggsave(file.path(save_dir, fname), width=5, height=5)
      
      # correlation plot
      dat_plot_zero_fill_p30 <- dat_plot |>
        mutate(across(starts_with(use_var), ~replace_na(.x, 0)))
      p30_correlations <- cor(dat_plot_zero_fill_p30 |> select(all_of(starts_with(str_c(use_var, '_')))))
      
      fname <- sprintf('corrplot_%s.pdf', use_var)
      pdf(file.path(save_dir, fname), width=5, height=5)
      p_corr <- corrplot(p30_correlations, method='number', type='lower')
      dev.off()
    }
  }
}


