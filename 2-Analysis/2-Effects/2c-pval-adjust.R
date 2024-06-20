source('0-Config/0-config.R')

analysis_types <- c("itt", "perprot")

for (analysis_type in analysis_types) {
  # Define directory for saving results
  save_dir <- file.path(ANALYSIS_OUT_DIR, '2-Effects-Output', '2c-Pval-Adjust', analysis_type)
  
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  res_dir_pri <- file.path(ANALYSIS_OUT_DIR, 
                       '2-Effects-Output', 
                       '2a-Between-Group-Primary',
                       analysis_type)
  
  res_dir_sec <- file.path(ANALYSIS_OUT_DIR, 
                           '2-Effects-Output', 
                           '2b-Between-Group-Secondary',
                           analysis_type)
  
  all_res_reg <- data.frame()
  all_res_ttest <- data.frame()
  
  # Load primary and secondary outcome regression and t-test results files
  for (timept in c('mid', 'eot', 'followup')) {
    if (analysis_type == "perprot") {
      reg_pri_fname <- str_c(res_dir_pri, '/', timept, '/regression/res_delta_', timept, '_p30_ols.csv')
      reg_sec_fname <- str_c(res_dir_sec, '/', timept, '/regression/res_combined_ols.csv')
    } else {
      reg_pri_fname <- str_c(res_dir_pri, '/', timept, '/regression/res_delta_', timept, '_p30_ols-weighted_ols.csv')
      reg_sec_fname <- str_c(res_dir_sec, '/', timept, '/regression/res_combined_ols-weighted_ols.csv')
    }
    ttest_pri_fname <- str_c(res_dir_pri, '/', timept, '/ttest/ttest_delta_', timept, '_p30.csv')
    ttest_sec_fname <- str_c(res_dir_sec, '/', timept, '/ttest/res_combined.csv')
    
    if (file.exists(reg_pri_fname)) {
      res_reg_pri <- read.csv(reg_pri_fname) |>
        mutate(timept = timept,
               var = 'p30')
      all_res_reg <- bind_rows(all_res_reg, res_reg_pri)
    } 
    if (file.exists(ttest_pri_fname)) {
      res_ttest_pri <- read.csv(ttest_pri_fname) |>
        mutate(timept = timept,
               var = 'p30')
      all_res_ttest <- bind_rows(all_res_ttest, res_ttest_pri)
    }
    
    if (file.exists(reg_sec_fname)) {
      res_reg_sec <- read.csv(reg_sec_fname) |>
        mutate(timept = timept)
      all_res_reg <- bind_rows(all_res_reg, res_reg_sec)
    } 
    if (file.exists(ttest_sec_fname)) {
      res_ttest_sec <- read.csv(ttest_sec_fname) |>
        mutate(timept = timept)
      all_res_ttest <- bind_rows(all_res_ttest, res_ttest_sec)
    }
  }
  
  # Apply p-value adjustment
  all_res_reg <- all_res_reg |>
    filter(!(timept == "eot" & var == "p30")) |>
    group_by(method) |>
    mutate(beta_pval_adj = p.adjust(beta_pval, method="BH"),
           dim_pval_adj = p.adjust(dim_pval, method="BH")) |>
    arrange(method, timept)
  
  all_res_ttest <- all_res_ttest |>
    filter(!(timept == "eot" & var == "p30")) |>
    mutate(p.value_adj = p.adjust(p.value, method="BH")) |>
    arrange(method, timept)
  
  write.csv(all_res_reg, file.path(save_dir, 'regression_adj_p.csv'))
  write.csv(all_res_ttest, file.path(save_dir, 'ttest_adj_p.csv'))
}