get_descriptive_summary <- function(dat, 
                                    vars,
                                    save_prefix,
                                    save_dir, 
                                    cohens_d_vars=NULL,
                                    by_group=TRUE,
                                    grouping_var=c('group')) {

  if (by_group) {
    dat <- dat |>
      group_by(across(all_of(grouping_var)))
  }
  all_summaries <- data.frame()
  for (var in vars) {
    summary <- dat |>
      summarize(n = sum(!is.na(!!sym(var))),
                mean = mean(!!sym(var), na.rm=TRUE),
                sd = sd(!!sym(var), na.rm=TRUE), 
                min = min(!!sym(var), na.rm=TRUE),
                max = max(!!sym(var), na.rm=TRUE), .groups='drop') |>
      mutate(var = var)
    if (!is.null(cohens_d_vars) & var %in% cohens_d_vars) {
      summary <- summary |>
        mutate(cohen_d = mean / sd)
    }
    all_summaries <- bind_rows(all_summaries, summary)
  }
  
  all_summaries <- all_summaries |>
    arrange(across(all_of(rev(grouping_var))))
  
  write.csv(all_summaries, file.path(save_dir, str_c(save_prefix, 'descriptive_summary.csv', sep='_')), row.names=F)
  
  return (all_summaries)
}

get_substance_use_summary <- function(dat, p30_vars_regex, save_dir) {
  sub_use_summary <- dat |> 
    group_by(psub_1) |>
    summarize(across(matches(p30_vars_regex), \(x) mean(x, na.rm=TRUE), .names = '{col}_mean'), 
              across(matches(p30_vars_regex) & !contains('_mean'), \(x) sd(x, na.rm=TRUE), .names = '{col}_sd'))
  
  write.csv(sub_use_summary, file.path(save_dir, 'sub_use_summary.csv'), row.names=F)
}
