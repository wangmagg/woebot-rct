source('0-Config/0-config.R')

analysis_types <- c("itt", "perprot")

for (analysis_type in analysis_types) {
  # Define directory for saving results
  save_dir <- file.path(ANALYSIS_OUT_DIR, '3-Engagement-Output', '1-Acc-Fea', analysis_type)
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Read input data file
  dat_outcome_regression <- read.csv(file.path(DATA_OUT_DIR, 'dat_analysis_outcome-regression.csv'))
  dat_descriptive <- read.csv(file.path(DATA_OUT_DIR, 'dat_analysis_descriptive.csv'))
  
  if (analysis_type == "perprot") {
    dat_outcome_regression <- dat_outcome_regression |>
      subset_per_protocol()
    dat_descriptive <- dat_descriptive |>
      subset_per_protocol()
  }
  
  # Get descriptive summaries
  dat_descriptive_retained <- dat_descriptive |> filter(retained_eot == 1)
  get_descriptive_summary(dat_descriptive_retained |> filter(group == 1),
                          vars = c('eot_csq_grp1', str_c('eot_csq_grp1', 1:8, sep='_')),
                          save_prefix = 'csq_grp1_eot-retained',
                          save_dir = save_dir)
  get_descriptive_summary(dat_descriptive_retained |> filter(group == 2),
                          vars = c('eot_csq_grp2', str_c('eot_csq_grp2', 1:8, sep='_')),
                          save_prefix = 'csq_grp2_eot-retained',
                          save_dir = save_dir)
  get_descriptive_summary(dat_descriptive_retained |> filter(group == 1),
                          vars = c('eot_waisr_g', 'eot_waisr_t', 'eot_waisr_b'),
                          save_prefix = 'eot_waisr_eot-retained',
                          save_dir = save_dir)
  get_descriptive_summary(dat_descriptive_retained,
                          vars = c('eot_urpi_a', 'eot_urpi_f'),
                          save_prefix = 'eot_urpi_eot-retained',
                          save_dir = save_dir)
  
  # Get correlations in treated group (W-SUDS)
  dat <- dat_outcome_regression |>
    filter(retained_eot == 1) |>
    mutate(eot_csq = 
             case_when(
               group == 1 ~ eot_csq_grp1,
               group == 2 ~ eot_csq_grp2),
           group = as.factor(group)) |>
    mutate_at(c("group"), ~relevel(.x, ref=2)) |>
    select(-eot_csq_grp1, -eot_csq_grp2)
  
  dat_grp1 <- dat |> filter(group == 1)
  grp1_vars <- c('eot_csq', 
                 'eot_urpi_a', 'eot_urpi_f', 
                 'eot_waisr_g', 'eot_waisr_t', 'eot_waisr_b')
  
  grp1_vars_cor <- data.frame()
  for (grp1_var in grp1_vars) {
    dat_grp1_var_complete <- dat_grp1 |> 
      drop_na({{grp1_var}})
    grp1_var_cor <- cor.test(dat_grp1_var_complete$delta_eot_p30, dat_grp1_var_complete[[grp1_var]],
                             method = 'kendall')
    grp1_var_cor_tidy <- broom::tidy(grp1_var_cor) |> mutate(var = grp1_var)
    grp1_vars_cor <- bind_rows(grp1_vars_cor, grp1_var_cor_tidy) 
  }
  grp1_vars_cor <- grp1_vars_cor |> mutate(group = 1)
  
  # Get correlations in control group (Psychoeducation)
  dat_grp2 <- dat |> filter(group == 2)
  grp2_vars <- c('eot_csq', 'eot_urpi_a', 'eot_urpi_f')
  
  grp2_vars_cor <- data.frame()
  for (grp2_var in grp2_vars) {
    dat_grp2_var_complete <- dat_grp2 |> 
      drop_na({{grp2_var}})
    grp2_var_cor <- cor.test(dat_grp2_var_complete$delta_eot_p30, dat_grp2_var_complete[[grp2_var]],
                             method="kendall")
    grp2_var_cor_tidy <- broom::tidy(grp2_var_cor) |> mutate(var = grp2_var)
    grp2_vars_cor <- bind_rows(grp2_vars_cor, grp2_var_cor_tidy) 
  }
  grp2_vars_cor <- grp2_vars_cor |> mutate(group = 2)
  
  vars_cor <- bind_rows(grp1_vars_cor, grp2_vars_cor)
  write.csv(vars_cor, file.path(save_dir, 'vars_cor.csv'))
  
  # Make scatterplots of feasibility/acceptability scores versus change in substance use occasions
  dat_subset <- dat |> 
    select(group, delta_eot_p30, all_of(grp1_vars), all_of(grp2_vars))
  dat_subset_long <- dat_subset |> 
    pivot_longer(cols = -c(delta_eot_p30, group), names_to = 'var_name', values_to = 'value')
  
  csq.labs <- c("CSQ")
  names(csq.labs) <- c("eot_csq")
  groups.labs <- c("W-SUDS", "Psychoeducation")
  names(groups.labs) <- c("1", "2")
  dat_subset_long_csq <- dat_subset_long |> 
    filter(str_detect(var_name, 'csq')) |>
    drop_na(value)
  ggplot(dat_subset_long_csq, aes(x=delta_eot_p30, y=value, color=group)) + 
    geom_point() + 
    geom_smooth(method="lm", formula='y~x') +
    facet_grid(cols=vars(var_name),
               rows=vars(group),
               labeller = labeller(var_name=csq.labs,
                                   group=groups.labs)) + 
    xlab('Change in Substance Use Occasions (8-wk - Baseline)') +
    ylab('Score') +
    scale_color_discrete(name = 'Group',
                         labels = c('1'='W-SUDS', '2'='Psychoed')) +
    theme_bw()
  ggsave(file.path(save_dir, 'csq_sub-use-occ_scatter.png'), width=5, height=5)
  
  urpi.labs <- c("URP-I Acceptability", "URP-I Feasibility")
  names(urpi.labs) <- c("eot_urpi_a", "eot_urpi_f")
  dat_subset_long_urpi <- dat_subset_long |> 
    filter(str_detect(var_name, 'urpi')) |>
    drop_na(value)
  ggplot(dat_subset_long_urpi, aes(x=delta_eot_p30, y=value, color=group)) + 
    geom_point() + 
    geom_smooth(method="lm", formula='y~x') +
    facet_grid(cols=vars(var_name),
               rows=vars(group),
               labeller = labeller(var_name=urpi.labs,
                                   group=groups.labs)) + 
    xlab('Change in Substance Use Occasions (8-wk - Baseline)') +
    ylab('Score') +
    scale_color_discrete(name = 'Group',
                         labels = c('1'='W-SUDS', '2'='Psychoed')) +
    theme_bw()
  ggsave(file.path(save_dir, 'urpi_sub-use-occ_scatter.png'), width=5, height=5)
  
  
  waisr.labs <- c("WAI-SR Goals", "WAI-SR Tasks", "WAI-SR Bond")
  names(waisr.labs) <- c("eot_waisr_g", "eot_waisr_t", "eot_waisr_b")
  dat_subset_long_waisr <- dat_subset_long |> 
    filter(str_detect(var_name, 'waisr'), group==1) |>
    drop_na(value)
  ggplot(dat_subset_long_waisr, aes(x=delta_eot_p30, y=value, color=group)) + 
    geom_point() + 
    geom_smooth(method="lm", formula='y~x') +
    facet_grid(cols=vars(var_name),
               rows=vars(group),
               labeller = labeller(var_name=waisr.labs,
                                   group=groups.labs)) + 
    xlab('Change in Substance Use Occasions (8-wk - Baseline)') +
    ylab('Score') +
    scale_color_discrete(name = 'Group',
                         labels = c('1'='W-SUDS', '2'='Psychoed')) +
    theme_bw()
  ggsave(file.path(save_dir, 'waisr_sub-use-occ_scatter.png'), width=5, height=5)
}
