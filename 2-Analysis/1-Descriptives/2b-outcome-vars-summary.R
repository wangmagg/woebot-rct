source('0-Config/0-config.R')

analysis_types <- c("itt", "perprot")

for (analysis_type in analysis_types) {
  # Define directory for saving results
  save_dir <- file.path(ANALYSIS_OUT_DIR, '1-Descriptives-Output', '2b-Outcome-Vars-Summary', analysis_type)
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  # Read input data file
  dat <- read_csv(file.path(DATA_OUT_DIR, 'dat_analysis_descriptive.csv'), show_col_types=FALSE)
  if (analysis_type == "perprot") {
    dat <- dat |>
      subset_per_protocol()
  }
  
  # Get descriptive summaries for baseline
  # NOTE: all scales have within-scale mean imputation
  if (!dir.exists(file.path(save_dir, 'baseline'))) {
    dir.create(file.path(save_dir, 'baseline'), recursive = TRUE)
  }
  get_descriptive_summary(dat,
                          vars = BASELINE_OUTCOME_VARS,
                          save_prefix = 'baseline',
                          save_dir = file.path(save_dir, 'baseline'))
  
  get_descriptive_summary(dat |> subset_retained(),
                          vars = BASELINE_OUTCOME_VARS,
                          save_prefix = 'baseline-retained',
                          save_dir = file.path(save_dir, 'baseline'))
  
  if (!dir.exists(file.path(save_dir, 'baseline', 'subgroups'))) {
    dir.create(file.path(save_dir, 'baseline', 'subgroups'), recursive = TRUE)
  }
  
  subgroup_summary_combined <- data.frame()
  for (subgroup_var in SUBGROUP_VARS) {
    subgroup_fn <- get_subgrouping_fn(subgroup_var)
    subgroups <- dat |> 
      subset_retained() |>
      subgroup_fn()
    dat_with_subgroups <- dat |> subset_retained() |> inner_join(subgroups, by = 'participant_id')
    subgroup_summary <- get_descriptive_summary(dat_with_subgroups,
                                                vars = c('p30'),
                                                save_prefix = str_c('subgroup-', subgroup_var, '_baseline-retained'),
                                                save_dir = file.path(save_dir, 'baseline', 'subgroups'),
                                                grouping_var = c('group', 'subgroup')) |> 
      rename(level = subgroup) |>
      mutate(subgroup = subgroup_var)
    subgroup_summary_combined <- bind_rows(subgroup_summary_combined, subgroup_summary)
  }
  save_fname <- str_c('subgroups_baseline-retained_descriptive_summary.csv')
  write.csv(subgroup_summary_combined, file.path(save_dir, 'baseline', save_fname), row.names=FALSE)
  
  # Get descriptive summaries for baseline (complete at 4, 8, 12), 4-weeks, 8-weeks, 12-weeks
  # NOTE: all scales have within-scale mean imputation
  timepts <- c('eot', 'mid', 'followup')
  all_delta_var_summaries <- data.frame()
  
  for (timept in timepts) {
    if (!dir.exists(file.path(save_dir, timept))) {
      dir.create(file.path(save_dir, timept), recursive = TRUE)
    }
    if (!dir.exists(file.path(save_dir, timept, 'subgroups'))) {
      dir.create(file.path(save_dir, timept, 'subgroups'), recursive = TRUE)
    }
    
    
    retain_var <- str_c('retained', timept, sep='_')
    outcome_vars <- str_c(timept, OUTCOME_VARS_DICT[[timept]], sep='_')
    dat_retained <- dat |> filter(!!sym(retain_var) == 1)
    
    get_descriptive_summary(dat_retained,
                            vars = BASELINE_OUTCOME_VARS,
                            save_prefix = str_c('baseline', '-retained-complete-at-', timept),
                            save_dir = file.path(save_dir, 'baseline'))
    delta_var_summary <- get_descriptive_summary(dat_retained,
                                                 vars = c(BASELINE_OUTCOME_VARS, outcome_vars, str_c('delta', outcome_vars, sep='_')),
                                                 cohens_d_vars = c(str_c('delta', outcome_vars, sep='_')),
                                                 save_prefix = str_c(timept, '-retained'),
                                                 save_dir = file.path(save_dir, timept))
    delta_var_summary <- delta_var_summary |> 
      mutate(timept = timept)
    all_delta_var_summaries <- bind_rows(all_delta_var_summaries, delta_var_summary)
    
    subgroup_summary_combined <- data.frame()
    for (subgroup_var in SUBGROUP_VARS) {
      subgroup_fn <- get_subgrouping_fn(subgroup_var)
      subgroups <- dat_retained |> 
        subgroup_fn() 
      dat_with_subgroups <- dat_retained |>
        inner_join(subgroups, by = 'participant_id')
      
      subgroup_summary <- get_descriptive_summary(dat_with_subgroups,
                                                  vars = c(str_c(timept, 'p30', sep='_'), str_c('delta', timept, 'p30', sep='_')),
                                                  save_prefix = str_c('subgroup-', subgroup_var, '_', timept, '-retained'),
                                                  save_dir = file.path(save_dir, timept, 'subgroups'),
                                                  grouping_var = c('group', 'subgroup')) |> 
        rename(level = subgroup) |>
        mutate(subgroup = subgroup_var)
      subgroup_summary_combined <- bind_rows(subgroup_summary_combined, subgroup_summary)
    }
    save_fname <- str_c('subgroups_', timept, '-retained_descriptive_summary.csv')
    write.csv(subgroup_summary_combined, file.path(save_dir, timept, save_fname), row.names=FALSE)
    
    if (timept == 'eot') {
      if (!dir.exists(file.path(save_dir, timept, 'scales_acc_fea'))) {
        dir.create(file.path(save_dir, timept, 'scales_acc_fea'), recursive = TRUE)
      }
      
      get_descriptive_summary(dat_retained |> filter(group == 1),
                              vars = c('eot_csq_grp1', str_c('eot_csq_grp1', 1:8, sep='_')),
                              save_prefix = 'csq_grp1_eot-retained',
                              save_dir = file.path(save_dir, timept, 'scales_acc_fea'))
      get_descriptive_summary(dat_retained |> filter(group == 2),
                              vars = c('eot_csq_grp2', str_c('eot_csq_grp2', 1:8, sep='_')),
                              save_prefix = 'csq_grp2_eot-retained',
                              save_dir = file.path(save_dir, timept, 'scales_acc_fea'))
      get_descriptive_summary(dat_retained |> filter(group == 1),
                              vars = c('eot_waisr_g', 'eot_waisr_t', 'eot_waisr_b'),
                              save_prefix = 'eot_waisr_eot-retained',
                              save_dir = file.path(save_dir, timept, 'scales_acc_fea'))
      get_descriptive_summary(dat_retained,
                              vars = c('eot_urpi_a', 'eot_urpi_f'),
                              save_prefix = 'eot_urpi_eot-retained',
                              save_dir = file.path(save_dir, timept, 'scales_acc_fea'))
      get_substance_use_summary(dat_retained,
                                p30_vars_regex = '^p30',
                                save_dir = file.path(save_dir, timept))
    }
  }
}
