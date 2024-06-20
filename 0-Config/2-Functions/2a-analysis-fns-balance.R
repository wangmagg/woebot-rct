### Functions for constructing balance table

#' Helper function for getting balance info for continuous variables
#' 
#' @param dat_input Data
#' @param var_name Name of variable to get balance info for
#' @param grouping_var Name of variable that defines groups
#' @param mod_sev_thresh If not NULL, threshold defining moderate/severe value of the variable
#' @returns Dataframe with balance info
.get_contin_var_bal_df <- function(dat_input, var_name, grouping_var, mod_sev_thresh=NULL) {
  if (length(var_name) == 1) {
    if (!is.null(mod_sev_thresh)) {
      dat_input |>
        group_by(across(all_of(grouping_var))) |>
        summarize(n = sum(!is.na(!!sym(var_name))),
                  p = n / n(),
                  mean = mean(!!sym(var_name), na.rm=TRUE), 
                  sd = sd(!!sym(var_name), na.rm=TRUE),
                  min = min(!!sym(var_name), na.rm=TRUE),
                  max = max(!!sym(var_name), na.rm=TRUE),
                  n_modsev = sum(!!sym(var_name) >= mod_sev_thresh, na.rm=TRUE),
                  p_modsev = n_modsev / n())
    } 
    dat_input |>
      group_by(across(all_of(grouping_var))) |>
      summarize(n = sum(!is.na(!!sym(var_name))),
                p = n / n(),
                mean = mean(!!sym(var_name), na.rm=TRUE), 
                sd = sd(!!sym(var_name), na.rm=TRUE),
                min = min(!!sym(var_name), na.rm=TRUE),
                max = max(!!sym(var_name), na.rm=TRUE)) |>
      ungroup()
  } else {
    dat_input |>
      group_by(across(all_of(grouping_var))) |>
      summarize(across(all_of(var_name),
                       .fns = list(n = ~sum(!is.na(.x)),
                                   p = ~sum(!is.na(.x)) / n(),
                                   mean = ~mean(.x, na.rm=TRUE), 
                                   sd = ~sd(.x, na.rm=TRUE),
                                   min = ~min(.x, na.rm=TRUE),
                                   max = ~max(.x, na.rm=TRUE)),
                       .names = '{.fn}_{.col}'))
  }
}

#' Helper function for getting balance info for categorical variables
#' 
#' @param dat_input Data
#' @param var_name Name of variable to get balance info for
#' @param grouping_var Name of variable that defines groups
#' @returns Dataframe with balance info
.get_factor_var_bal_df <- function(dat_input, var_name, grouping_var) {
  if (length(var_name) == 1) {
    dat_input |> 
      group_by(across(all_of(grouping_var)), !!sym(var_name)) |>
      summarize(n = n(), .groups="drop_last") |>
      mutate(p = n / sum(n))
  } else {
    dat_input |>
      group_by(across(all_of(grouping_var))) |>
      summarize(across(all_of(var_name),
                       .fns = list(n=~sum(.x, na.rm=TRUE), p=~sum(.x, na.rm=TRUE) / n()),
                       .names = '{.fn}_{.col}'))
  }
}

#' Helper function for getting standardized mean difference across groups
#' 
#' @param dat_input Data
#' @param var_name Name of variable to get balance info for
#' @param grouping_var Name of variable that defines groups
#' @returns Dataframe with SMD
.get_smd_df <- function(dat_input, var_name, grouping_var) {
  
  # Drop columns if any of the groups contain all missing values
  keep_cols <- dat_input |>
    group_by(!!sym(grouping_var)) |>
    summarize(across(everything(), ~all(is.na(.)))) |>
    summarize(across(-!!sym(grouping_var), ~sum(.))) |>
    select(where(~. == 0)) |>
    colnames()
  dat_input <- dat_input |> select(!!sym(grouping_var), all_of(keep_cols))
  
  # Compute standardized mean difference
  if (length(var_name) == 1) {
    dat_input |> 
      summarize(smd = smd(!!sym(var_name), g=!!sym(grouping_var), na.rm=TRUE)$estimate) |>
      mutate(var = var_name)
  } else {
    dat_input |>
      select(!!sym(grouping_var), any_of(var_name)) |>
      summarize(across(-!!sym(grouping_var),
                       ~smd(.x, g=!!sym(grouping_var), na.rm=TRUE)$estimate,
                       .names='smd_{.col}')
      ) |>
      pivot_longer(starts_with('smd'),
                   names_prefix='smd_',
                   names_to='var',
                   values_to='smd') |>
      select(var, smd)
  }
}

#' Get number of individuals in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
get_group_balance <- function(dat_input, grouping_var) {
  bal_df <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n = n())
  return (list('bal' = bal_df,
               'smd' = NULL))
}

#' Get age summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_age_balance <- function(dat_input, grouping_var, get_smd) {
  bal_df <- dat_input |> .get_contin_var_bal_df('age', grouping_var)
  
  if (get_smd) {
    smd_df <-  dat_input |> .get_smd_df('age', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  
  return (list('bal' = bal_df))
}

#' Get sex summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_sex_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      sex = case_when(
        sex == 1 ~ 'f',
        sex == 2 ~ 'm',
        sex == 99 ~ 'pna'
      )
    )
  bal_df <- dat_input |> .get_factor_var_bal_df('sex', grouping_var)
  
  if (get_smd) {
    smd_df <-  dat_input |> .get_smd_df('sex', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  
  return (list('bal' = bal_df))
}

#' Get gender summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_gender_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      gender = case_when(
        gender == 1 ~ 'm',
        gender == 2 ~ 'w',
        gender == 4 ~ 'b',
        gender == 3 | gender == 5 | gender == 6 ~ 'e',
        gender == 99 ~ 'pna'
      )
    )
  bal_df <- dat_input |> .get_factor_var_bal_df('gender', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('gender', grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  
  return (list('bal' = bal_df))
}

#' Get sexual orientation summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_orient_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      orient = case_when(
        orient == 1 ~ 'h',
        orient == 2 ~ 'gl',
        orient == 3 ~ 'b',
        orient == 4 ~ 'q',
        orient == 5 ~ 'p',
        orient == 6 ~ 'a',
        orient == 7 ~ 'e',
        orient == 8 ~ 'd',
        orient == 99 ~ 'pna'
      )
    )
  bal_df <- dat_input |> .get_factor_var_bal_df('orient', grouping_var)
  
  if (get_smd) {
    smd_df <-  dat_input |> .get_smd_df('orient', grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  
  return (list('bal' = bal_df))
}

#' Get ethnicity summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_eth_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      eth = case_when(
        eth == 1 ~ 'h',
        eth == 0 ~ 'nh',
        eth == 99 ~ 'pna'
      )
    )
  bal_df <- dat_input |> .get_factor_var_bal_df('eth', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('eth', grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  
  return (list('bal' = bal_df))
}

#' Get race summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_race_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      race = case_when(
        race_1 == 1 ~ 'ai',
        race_2 == 1 ~ 'aa',
        race_3 == 1 ~ 'b',
        race_5 == 1 ~ 'w',
        race_6 == 1 ~ 'o',
        race_mult == 1 ~ 'mult',
        race_99 == 1 ~ 'pna'
      )
    )
  bal_df <- dat_input |> .get_factor_var_bal_df('race', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('race', grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get marital status summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_marital_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      marital = case_when(
        marital == 1 ~ 'mcp',
        marital == 2 | marital == 3 ~ 'dsw',
        marital == 4 ~ 'ns',
        marital == 99 ~ 'pna'
      )
    )
  
  bal_df <- dat_input |> .get_factor_var_bal_df('marital', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('marital', grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  
  return (list('bal' = bal_df))
}

#' Get education summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_educ_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      educ = case_when(
        (educ == 1 | educ == 2 | educ == 3) ~ 'hs',
        educ == 4 ~ 'ct',
        educ == 5 ~ 'c',
        educ == 6 ~ 'g',
        educ == 99 ~ 'pna'
      )
    )
  bal_df <- dat_input |> .get_factor_var_bal_df('educ', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('educ', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get employment status summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_empl_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      empl = case_when(
        empl == 2 | empl == 3 ~ 'e',
        empl == 1 | empl == 4 | empl == 5 | empl == 6 | empl == 7 | empl == 8 ~ 'u',
        empl == 99 ~ 'pna'
      )
    )
  
  bal_df <- dat_input |> .get_factor_var_bal_df('empl', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('empl', grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  
  return (list('bal' = bal_df))
}

#' Get disability status summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_disab_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      disab = case_when(
        disab == 1 ~ 'd',
        disab == 0 ~ 'nd',
        disab == 99 ~ 'pna'
      )
    )
  
  bal_df <- dat_input |> .get_factor_var_bal_df('disab', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('disab', grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  
  return (list('bal' = bal_df))
}

#' Get insurance type summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_insur_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      insur = case_when(
        insur == 1 ~ 'p',
        insur == 2 | insur == 3 ~ 'm',
        insur == 4 ~ 't',
        insur == 5 ~ 'v',
        insur == 6 ~ 'i',
        insur == 7 ~ 'd',
        insur == 99 ~ 'pna'
      )
    )
  
  bal_df <- dat_input |> .get_factor_var_bal_df('insur', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('insur', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  
  return (list('bal' = bal_df))
  
}

#' Get therapy status summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_ther_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      ther_status = case_when(
        ther_status == 0 ~ 'nev',
        ther_status == 1 ~ 'for',
        ther_status == 2 ~ 'cur'
      )
    )
  bal_df <- dat_input |> .get_factor_var_bal_df('ther_status', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('ther_status', grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  
  return (list('bal' = bal_df))
}

#' Get past treatment summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_trt_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      trt_sh = (trt_1 == 1) & !is.na(trt_sub) & (trt_sub > 0),
      trt_op = (trt_2 == 1) & !is.na(trt_sub) & (trt_sub > 0),
      trt_ir = (trt_3 == 1 | trt_4 ==1) & !is.na(trt_sub) & (trt_sub > 0),
      trt_any = !is.na(trt_sub) & (trt_sub > 0),
      trt_pna = trt_99 == 1
    )
  
  var_names <- c('trt_sh', 'trt_op', 'trt_ir', 'trt_any', 'trt_pna')
  bal_df <- dat_input |> .get_factor_var_bal_df(var_names, grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df(var_names, grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get substance use medication summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_med_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(med = as.factor(med))
  bal_df <- dat_input |> .get_factor_var_bal_df('med', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('med', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get psychiatric medication summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_psych_med_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(psych_med = factor(psych_med))
  bal_df <- dat_input |> .get_factor_var_bal_df('psych_med', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('psych_med', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get mental health diagnosis summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_mh_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(mh_mult = rowSums(pick(mh_1, mh_2, mh_3, mh_4, mh_5, mh_6, mh_7, mh_8, mh_9, mh_10, mh_11, mh_12), na.rm=TRUE) > 1,
           mh_other = rowSums(pick(mh_1, mh_2, mh_6, mh_7, mh_8, mh_10), na.rm=TRUE) > 0,
           mh_nh = as.logical(mh_12),
           mh_ud = mh_4 | mh_11,
           mh_bp = as.logical(mh_3),
           mh_ad = as.logical(mh_5),
           mh_sud = as.logical(mh_9),
           mh_pna = as.logical(mh_99))
  
  var_names <- c('mh_nh', 'mh_ud', 'mh_bp', 'mh_ad', 'mh_sud', 'mh_mult', 'mh_other', 'mh_pna')
  bal_df <- dat_input |> .get_factor_var_bal_df(var_names, grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df(var_names, grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get PHQ summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_phq_balance <- function(dat_input, grouping_var, get_smd) {
  bal_df <- dat_input |> .get_contin_var_bal_df('phq', grouping_var, mod_sev_thresh=10)
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('phq', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get GAD summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_gad_balance <- function(dat_input, grouping_var, get_smd) {
  bal_df <- dat_input |> .get_contin_var_bal_df('gad', grouping_var, mod_sev_thresh=10)
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('gad', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get SPS summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_sps_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
      sps = case_when(
        sps >= 6 & sps <= 10 ~ '6-10',
        sps >= 11 & sps <= 15 ~ '11-15',
        sps >= 16 & sps <= 20 ~ '16-20',
        sps >= 21 & sps <= 25 ~ '21-25',
        sps >= 26 & sps <= 30 ~ '26-30'
      )
    )
  bal_df <- dat_input |> .get_factor_var_bal_df('sps', grouping_var)
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('sps', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get primary, secondary, tertiary substance summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_pst_sub_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(
         psub_1_oth = (psub_1 == 2 | psub_1 == 4 | psub_1 == 5 | psub_1 == 6 | psub_1 == 8 | psub_1 == 9 | psub_1 == 12),
         psub_2_oth = (psub_2 == 2 | psub_2 == 4 | psub_2 == 5 | psub_2 == 6 | psub_2 == 8 | psub_2 == 9 | psub_2 == 12),
         psub_3_oth = (psub_3 == 2 | psub_3 == 4 | psub_3 == 5 | psub_3 == 6 | psub_3 == 8 | psub_3 == 9 | psub_3 == 12)
    ) |>
    mutate(
      pst_alc = psub_1 == 1 | psub_2 == 1 | psub_3 == 1,
      pst_can = psub_1 == 7 | psub_2 == 7 | psub_3 == 7,
      pst_sti = psub_1 == 10 | psub_2 == 10 | psub_3 == 10,
      pst_coc = psub_1 == 3 | psub_2 == 3 | psub_3 == 3,
      pst_tob = psub_1 == 11 | psub_2 == 11 | psub_3 == 11,
      pst_oth_any = psub_1_oth | psub_2_oth | psub_3_oth,
    )
  
  var_names <- c('pst_alc', 'pst_can', 'pst_sti', 'pst_coc', 'pst_tob', 'pst_oth_any')
  
  bal_df <- dat_input |> .get_factor_var_bal_df(var_names, grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df(var_names, grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get DAST summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_dast_balance <- function(dat_input, grouping_var, get_smd) {
  bal_df <- dat_input |> .get_contin_var_bal_df('dast', grouping_var, mod_sev_thresh=3)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('dast', grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get SIPAD summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_sipad_balance <- function(dat_input, grouping_var, get_smd) {
  bal_df <- dat_input |> .get_contin_var_bal_df('sipad', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('sipad', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get CAGEAID summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_cageaid_balance <- function(dat_input, grouping_var, get_smd) {
  bal_df <- dat_input |> .get_contin_var_bal_df('cageaid', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('cageaid', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get BSCQ summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_bscq_balance <- function(dat_input, grouping_var, get_smd) {
  bal_df <- dat_input |> .get_contin_var_bal_df('bscq', grouping_var, mod_sev_thresh=10)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('bscq', grouping_var)
    
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Get cravings summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_crave_balance <- function(dat_input, grouping_var, get_smd) {
  bal_df <- dat_input |> .get_contin_var_bal_df('crave', grouping_var, mod_sev_thresh=10)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('crave', grouping_var)

    return (list('bal' = bal_df,
                 'smd' = smd_df))  
  }
  return (list('bal' = bal_df))

}

#' Get past 30day substance use summary in each group,
#' separately for each substance

#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_p30_per_sub_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    mutate(across(starts_with("p30_"),
                  ~case_when(.x == 99 ~ NA,
                             TRUE ~ .x))) 
  
  bal_df <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(across(starts_with("p30_"),
                     list(mean = ~mean(.x, na.rm=TRUE),
                          sd = ~sd(.x, na.rm=TRUE),
                          min = ~min(.x, na.rm=TRUE),
                          max = ~max(.x, na.rm=TRUE),
                          nany = ~sum(.x > 0, na.rm=TRUE),
                          pany = ~sum(.x > 0, na.rm=TRUE) / n()))) |>
    pivot_longer(
      cols = -all_of(grouping_var),
      names_to = c('substance', '.value') ,
      names_sep = '_',
      names_prefix = 'p30_'
    )
  
  var_names <- dat_input |>
    select(starts_with('p30_')) |>
    colnames()
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df(var_names, grouping_var)
    
    return(list('bal' = bal_df,
                'smd' = smd_df))
  }
  return(list('bal' = bal_df))
}

#' Get total past30d substance use summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_p30_balance <- function(dat_input, grouping_var, get_smd) {
  bal_df <- dat_input |> .get_contin_var_bal_df('p30', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('p30', grouping_var)
    return(list('bal' = bal_df,
                'smd' = smd_df))
  }
  return(list('bal' = bal_df))
}

#' Get QDS summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_qds_balance <- function(dat_input, grouping_var, get_smd) {
  dat_input <- dat_input |>
    rename(dapw = qds_1,
           drpd = qds_2)
  
  var_names <- c('dapw', 'drpd', 'heavy')
  bal_df <- dat_input |> .get_contin_var_bal_df(var_names, grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df(var_names, grouping_var)
    return(list('bal' = bal_df,
                'smd' = smd_df))
  }
  return(list('bal' = bal_df))
}

#' Get TAA summary in each group
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param get_smd Boolean, compute SMD between groups if true
get_taa_balance <- function(dat_input, grouping_var, get_smd) {
  bal_df <- dat_input |> .get_contin_var_bal_df('taa', grouping_var)
  
  if (get_smd) {
    smd_df <- dat_input |> .get_smd_df('taa', grouping_var)
    return (list('bal' = bal_df,
                 'smd' = smd_df))
  }
  return (list('bal' = bal_df))
}

#' Wrapper function for getting balance table information across specified variables
#' 
#' @param dat_input Data
#' @param grouping_var Name of variable that defines groups
#' @param var_names List of variables names to get balance table info for
#' @param save_prefix String prefix for name of file where results are saved
#' @param save_dir Directory to save results to
#' @param get_smd Boolean, compute SMD between groups if true
get_balance <- function(dat_input, grouping_var, var_names, save_prefix, save_dir, get_smd=TRUE) {
  if (!file.exists(save_dir)) {
    dir.create(save_dir)
  }
  if (get_smd) {
    all_smd <- data.frame()
  }
  for (var_name in var_names) {
    res <- switch(
      var_name,
      "group" = get_group_balance(dat_input, grouping_var),
      "age" = get_age_balance(dat_input, grouping_var, get_smd),
      "sex" = get_sex_balance(dat_input, grouping_var, get_smd),
      "gender" = get_gender_balance(dat_input, grouping_var, get_smd),
      "orient" = get_orient_balance(dat_input, grouping_var, get_smd),
      "eth" = get_eth_balance(dat_input, grouping_var, get_smd),
      "race" = get_race_balance(dat_input, grouping_var, get_smd),
      "marital" = get_marital_balance(dat_input, grouping_var, get_smd),
      "educ" = get_educ_balance(dat_input, grouping_var, get_smd),
      "empl" = get_empl_balance(dat_input, grouping_var, get_smd),
      "disab" = get_disab_balance(dat_input, grouping_var, get_smd),
      "insur" = get_insur_balance(dat_input, grouping_var, get_smd),
      "ther" = get_ther_balance(dat_input, grouping_var, get_smd),
      "trt" = get_trt_balance(dat_input, grouping_var, get_smd),
      "med" = get_med_balance(dat_input, grouping_var, get_smd),
      "psych_med" = get_psych_med_balance(dat_input, grouping_var, get_smd),
      "mh" = get_mh_balance(dat_input, grouping_var, get_smd),
      "phq" = get_phq_balance(dat_input, grouping_var, get_smd),
      "gad" = get_gad_balance(dat_input, grouping_var, get_smd),
      "sps" = get_sps_balance(dat_input, grouping_var, get_smd),
      "pst_sub" = get_pst_sub_balance(dat_input, grouping_var, get_smd),
      "dast" = get_dast_balance(dat_input, grouping_var, get_smd),
      "sipad" = get_sipad_balance(dat_input, grouping_var, get_smd),
      "cageaid" = get_cageaid_balance(dat_input, grouping_var, get_smd),
      "bscq" = get_bscq_balance(dat_input, grouping_var, get_smd),
      "crave" = get_crave_balance(dat_input, grouping_var, get_smd),
      "p30_per_sub" = get_p30_per_sub_balance(dat_input, grouping_var, get_smd),
      "p30" = get_p30_balance(dat_input, grouping_var, get_smd),
      "qds" = get_qds_balance(dat_input, grouping_var, get_smd),
      "taa" = get_taa_balance(dat_input, grouping_var, get_smd)
    )
    write.csv(res$bal, file.path(save_dir, str_c(save_prefix, var_name, 'balance.csv', sep='_')), row.names=FALSE)
    if (get_smd) {
      all_smd <- bind_rows(all_smd, res$smd)
    }
    
  }
  if (get_smd) {
    write.csv(all_smd, file.path(save_dir, str_c('smd.csv', sep='_')))
  }
}