### Functions for recoding variables

#' Apply reverse coding to SPS items
#' 
#' @param dat Dataframe of survey data
#' @returns Dataframe with reverse coded SPS variable
reverse_code_sps_items <- function(dat) {

  dat <- dat |>
    mutate(sps_1 = case_when(sps_1 != 99 ~ (6 - sps_1), .default=99), 
           sps_3 = case_when(sps_3 != 99 ~ (6 - sps_3), .default=99),
           sps_4 = case_when(sps_4 != 99 ~ (6 - sps_4), .default=99))
}

#' Apply reverse coding to URPI item
#' 
#' @param dat Dataframe of survey data
#' @returns Dataframe with reverse coded URPI variable
reverse_code_urpi_items <- function(dat) {
  dat <- dat |>
    mutate(eot_urpi_f_5 = case_when(eot_urpi_f_5 != 99 ~ (6 - eot_urpi_f_5), .default=99))
}

#' Recode SPS as a categorical variable to handle missingness due to branching logic
#' 
#' @param dat Dataframe of survey data
#' @param sps_vars Column names for SPS variables
#' @returns Dataframe with SPS as a categorical variable (0 corresponds to missing)
recode_sps_for_branching <- function(dat, sps_vars) {
  for (sps_var in sps_vars) {
    dat <- dat |>
      mutate(!!sym(sps_var) := case_when(rowSums(!is.na(pick(starts_with(sps_var)))) == 0 ~ 0,
                                         !!sym(sps_var) >= 6 & !!sym(sps_var) <= 10 ~ 1,
                                         !!sym(sps_var) >= 11 & !! sym(sps_var) <= 15 ~ 2,
                                         !!sym(sps_var) >= 16 & !!sym(sps_var) <= 20 ~ 3,
                                         !!sym(sps_var) >= 21 & !!sym(sps_var) <= 25 ~ 4,
                                         !!sym(sps_var) >= 26 & !!sym(sps_var) <= 30 ~ 5))
  }
  return(dat)
}

#' Recode SPS to handle missingness due to branching logic by replacing NA's with 0
#' 
#' @param dat Dataframe of survey data
#' @param sps_vars Column names for SPS variables
#' @returns Dataframe with recoded SPS
recode_continuous_sps_for_branching <- function(dat, sps_vars) {
  for (sps_var in sps_vars) {
    dat <- dat |>
      mutate(!!sym(sps_var) := case_when(rowSums(!is.na(pick(starts_with(sps_var)))) == 0 ~ 0,
                                         .default=!!sym(sps_var)))
  }
  return(dat)
}

#' Revert SPS back to its non-recoded values (applicable only if continuous recoding was used)
#' 
#' @param dat Dataframe of survey data
#' @param sps_vars Column names for composite SPS variables 
#' @returns Dataframe with SPS reverted back to the values it had before recoding, aka 0 -> NA
revert_continuous_sps <- function(dat, sps_vars) {
  ## Revert SPS to values prior to recoding
  for (sps_var in sps_vars) {
    if (sps_var %in% colnames(dat)) {
      dat <- dat |>
        mutate(!!sym(sps_var) := case_when(!!sym(sps_var) == 0 ~ NA_real_,
                                           .default=!!sym(sps_var)))
    }
  }
  return(dat)
}

#' Recode DAST as a categorical variable to handle missingness due to branching logic
#' 
#' @param dat Dataframe of survey data
#' @param dast_vars Column names for composite DAST variables
#' @returns Dataframe with recoded DAST
recode_dast_for_branching <- function(dat, dast_vars) {
  for (dast_var in dast_vars) {
    dat <- dat |>
      mutate(!!sym(dast_var) := case_when(rowSums(!is.na(pick(starts_with(dast_var)))) == 0 ~ 0,
                                          !!sym(dast_var) == 0 ~ 1,
                                          !!sym(dast_var) >= 1 & !!sym(dast_var) <= 2 ~ 2,
                                          !!sym(dast_var) >= 3 & !!sym(dast_var) <= 5 ~ 3,
                                          !!sym(dast_var) >= 6 & !!sym(dast_var) <= 8 ~ 4,
                                          !!sym(dast_var) >= 9 & !!sym(dast_var) <= 10 ~ 5))
  }
  return(dat)
}

#' Recode DAST to handle missingness due to branching logic by replacing NA's with 0
#' and by adding 10 to non-missing values to distinguish a true 0 from an imputed 0
#' 
#' @param dat Dataframe of survey data
#' @param dast_vars Column names for composite DAST variables
#' @returns Dataframe with recoded DAST
recode_continuous_dast_for_branching <- function(dat, dast_vars) {
  for (dast_var in dast_vars) {
    dat <- dat |>
      mutate(!!sym(dast_var) := case_when(rowSums(!is.na(pick(starts_with(dast_var)))) == 0 ~ 0,
                                         .default=!!sym(dast_var) + 10))
  }
  return(dat)
}

#' Revert DAST back to its non-recoded values (applicable only if continuous recoding was used)
#' 
#' @param dat Dataframe of survey data
#' @param dast_vars Column names for composite DAST variables
#' @returns Dataframe with DAST reverted back to the values it had before recoding
revert_continuous_dast <- function(dat, dast_vars) {
  for (dast_var in dast_vars) {
    if (dast_var %in% colnames(dat)) {
      dat <- dat |>
        mutate(!!sym(dast_var) := case_when(!!sym(dast_var) == 0 ~ NA_real_,
                                            .default=!!sym(dast_var - 10)))
    }
  }
  return(dat)
}

#' Construct therapy status variable
#' trt_6 == 1 : no past 30d participation in forms of mental health OR substance use treatment
#' ther == 1: ever been seen by therapist for mental health or substance use
#' 
#' @param dat Dataframe of survey data
#' @returns Dataframe with therapy status column
set_therapy_status <- function(dat) {
  ## Define therapy status 
  # 0 - never in therapy
  # 1 - previously in therapy
  # 2 - currently in therapy
  dat <- dat |>
    mutate(ther_status = 
             case_when((ther == 0 & trt_6 == 1) ~ 0,
                       (ther == 1 & trt_6 == 1) ~ 1,
                       (trt_6 != 1 & trt_99 != 1) ~ 2,
                       .default = NA))
}

#' Construct CSQ variable by combining the separate CSQ columns
#' for the two treatment groups
#' 
#' @param dat Dataframe of survey data
#' @returns Dataframe with single CSQ column 
set_eot_csq <- function(dat) {
  ## Set CSQ variable 
  
  # Combine the separate CSQ columns for the two groups
  dat <- dat |>
    mutate(eot_csq = 
             case_when(
               group == 1 ~ eot_csq_grp1,
               group == 2 ~ eot_csq_grp2)) |>
    select(-eot_csq_grp1, -eot_csq_grp2)
  
  for (i in 1:8) {
    csq_var <- str_c("eot_csq", i, sep="_")
    csq_grp1_var <- str_c("eot_csq_grp1", i, sep="_")
    csq_grp2_var <- str_c("eot_csq_grp2", i, sep="_")
    dat <- dat |>
      mutate(!!sym(csq_var) := 
               case_when(
                 group == 1 ~ !!sym(csq_grp1_var),
                 group == 2 ~ !!sym(csq_grp2_var)
               )) |>
      select(-!!sym(csq_grp1_var), -!!sym(csq_grp2_var))
  }
  return (dat)
}

#' Construct past30-day substance use variable for
#' substances reported as primary, secondary, or tertiary
#' problematic substances
#' 
#' @param dat Dataframe of survey data
#' @returns Dataframe with past 30-day use of problematic substance
set_pst_p30 <- function(dat) {
  dat <- dat |>
    mutate(pst_p30_alc = case_when(psub_1 == 1 | psub_2 == 1 | psub_3 == 1 ~ p30_alc,
                                  .default = NA),
           pst_p30_can = case_when(psub_1 == 7 | psub_2 == 7 | psub_3 == 7 ~ p30_can,
                                  .default = NA),
           pst_p30_coc = case_when(psub_1 == 3 | psub_2 == 3 | psub_3 == 3 ~ p30_coc,
                                  .default = NA),
           pst_p30_sti_met = case_when(psub_1 == 10 | psub_2 == 10 | psub_3 == 10 ~ p30_sti + p30_met,
                                      .default = NA),
           pst_p30_inh = case_when(psub_1 == 6 | psub_2 == 6 | psub_3 == 6 ~ p30_inh,
                                   .default = NA),
           pst_p30_sed = case_when(psub_1 == 9 | psub_2 == 9 | psub_3 == 9 ~ p30_sed,
                                  .default = NA),
           pst_p30_hal = case_when(psub_1 == 4 | psub_2 == 4 | psub_3 == 4 ~ p30_hal,
                                  .default = NA),
           pst_p30_sop = case_when(psub_1 == 5 | psub_2 == 5 | psub_3 == 5 ~ p30_sop,
                                  .default = NA),
           pst_p30_pop = case_when(psub_1 == 8 | psub_2 == 8 | psub_3 == 8 ~ p30_pop,
                                  .default = NA)) 
  
  for (timept in c('mid', 'eot', 'followup')) {
    dat <- dat |>
      mutate(!!sym(str_c(timept, '_pst_p30_alc')) :=
               case_when(psub_1 == 1 | psub_2 == 1 | psub_3 == 1 ~ !!sym(str_c(timept, '_p30_alc')),
                         .default = NA),
            !!sym(str_c(timept, '_pst_p30_can')) :=
              case_when(psub_1 == 7 | psub_2 == 7 | psub_3 == 7 ~ !!sym(str_c(timept, '_p30_can')),
                        .default = NA),
            !!sym(str_c(timept, '_pst_p30_coc')) :=
              case_when(psub_1 == 3 | psub_2 == 3 | psub_3 == 3 ~ !!sym(str_c(timept, '_p30_coc')),
                        .default = NA),
            !!sym(str_c(timept, '_pst_p30_sti_met')) :=
              case_when(psub_1 == 10 | psub_2 == 10 | psub_3 == 10 ~ !!sym(str_c(timept, '_p30_sti')) + !!sym(str_c(timept, '_p30_met')),
                        .default = NA),
            !!sym(str_c(timept, '_pst_p30_inh')) :=
              case_when(psub_1 == 6 | psub_2 == 6 | psub_3 == 6 ~ !!sym(str_c(timept, '_p30_inh')),
                        .default = NA),
            !!sym(str_c(timept, '_pst_p30_sed')) :=
              case_when(psub_1 == 9 | psub_2 == 9 | psub_3 == 9 ~ !!sym(str_c(timept, '_p30_sed')),
                        .default = NA),
            !!sym(str_c(timept, '_pst_p30_hal')) :=
              case_when(psub_1 == 4 | psub_2 == 4 | psub_3 == 4 ~ !!sym(str_c(timept, '_p30_hal')),
                        .default = NA),
            !!sym(str_c(timept, '_pst_p30_sop')) :=
              case_when(psub_1 == 5 | psub_2 == 5 | psub_3 == 5 ~ !!sym(str_c(timept, '_p30_sop')),
                        .default = NA),
            !!sym(str_c(timept, '_pst_p30_pop')) :=
              case_when(psub_1 == 8 | psub_2 == 8 | psub_3 == 8 ~ !!sym(str_c(timept, '_p30_pop')),
                        .default = NA))
  }
  return(dat)
}

#' Make variables factor type 
#' 
#' @param dat Dataframe of survey data
#' @param vars List of column names 
#' @param add_na Boolean flag, whether or not to add NA level 
#' @param ordered Boolean flag, whether or not to make the factor ordered
#' @returns Dataframe with specified variables set to factor type
set_factors <- function(dat, vars, add_na=TRUE, ordered=FALSE) {
  dat <- dat |>
    mutate(across(all_of(vars), ~factor(.x, ordered=ordered)))
  
  if (add_na) {
    dat <- dat |>
      mutate_if(~(sum(is.na(.x)) > 0 & is.factor(.x)), addNA)
  }
  
  return(dat)
}

#' Collapse levels of categorical variables -
#' Sexual identity, race, mental health treatment, mental health diagnoses
#' insurance status, employment status, education, disability status
#' 
#' @param dat Dataframe of survey data
#' @returns Dataframe with collapsed categorical variables
collapse_multi <- function(dat) {
  dat <- dat |>
    mutate(
      orient = case_when(
        orient == 1 ~ 'het',
        .default = 'other'
      ),
      race = case_when(
        race_3 == 1 ~ 'black',
        race_5 == 1 ~ 'white',
        .default = 'other'),
      trt = case_when(
        trt_1 == 1 ~ 'self_help',
        trt_2 == 1 ~ 'outpat',
        trt_3 == 1 | trt_4 == 1 ~ 'intense_outpat_resid',
        .default = 'other'
      ),
      mh = case_when(
        mh_12 == 1 ~ 'no_history',
        .default = 'dep_anx_sud_other'
      ),
      insur = case_when(
        insur == 1 ~ 'private',
        insur == 2 | insur == 3 ~ 'medicare',
        .default='other'
      ),
      empl = case_when(
        empl == 2 | empl == 3 ~ 'employed',
        empl == 1 | empl == 4 | empl == 5 | empl == 6 | empl == 7 | empl == 8 ~ 'unemployed',
        .default = 'other'
      ),
      educ = case_when(
        educ == 1 | educ == 2 | educ == 3 | educ == 4 ~ 'hs',
        educ == 5 ~ 'college',
        educ == 6 ~ 'grad',
        .default = 'other'
      ),
      disab = case_when(
        disab == 1 ~ 'yes',
        disab == 0 | disab == 99 ~ 'no_pna'
      )
    ) |>
    select(-starts_with('race_'), -starts_with('trt_'), -starts_with('mh_'))
}

#' Construct composite variables by taking the sum of other variables
#' 
#' @param dat Dataframe of survey data
#' @param var_prefixes List of prefixes for variables to be summed
#' @param drop_items Boolean flag for whether to drop the variables being summed
#' @param exclude_substr String that, if present as a substring in a variable, means that variable
#' is left out of the summation
#' @param na_rm Boolean flag for whether NA should be removed from summation 
#' @returns Dataframe containing composite variables
set_composite_sums <- function(dat, var_prefixes, drop_items=TRUE, exclude_substr=NULL, na_rm=FALSE) {
  for (prefix in var_prefixes) {
    if (!is.null(exclude_substr)) {
      dat <- dat |>
        mutate(!!sym(prefix) := 
                 case_when(
                   rowSums(!is.na(pick(starts_with(prefix) & !contains(exclude_substr)))) == 0 ~ NA,
                   .default = rowSums(pick(starts_with(prefix) & !contains(exclude_substr)), na.rm = na_rm))
        )
    } else {
     dat <- dat |>
       mutate(!!sym(prefix) := 
                case_when(
                  rowSums(!is.na(pick(starts_with(prefix)))) == 0 ~ NA,
                  .default = rowSums(pick(starts_with(prefix)), na.rm = na_rm))
       )
   }
  }
  if (drop_items) {
    var_prefixes_with_underscore <- str_c(var_prefixes, '_')
    dat <- dat |> select(-starts_with(var_prefixes_with_underscore))
  }

  return(dat)
}

#' Construct composite variables by taking the mean of other variables
#' 
#' @param dat Dataframe of survey data
#' @param var_prefixes List of prefixes for variables to be averaged
#' @param drop_items Boolean flag for whether to drop the variables being averaged
#' @returns Dataframe containing composite variables
set_composite_means <- function(dat, var_prefixes, drop_items = TRUE) {
  for (prefix in var_prefixes) {
    dat <- dat |>
      mutate(!!sym(prefix) := 
               case_when(
                 rowSums(pick(starts_with(prefix))) == 0 ~ NA,
                 .default = rowMeans(pick(starts_with(prefix)))))
  }
  
  if (drop_items) {
    var_prefixes_with_underscore <- str_c(var_prefixes, '_')
    dat <- dat |> 
      select(-starts_with(var_prefixes_with_underscore))
  }
  return(dat)
}

#' Construct composite variables by taking the product of other variables
#' 
#' @param dat Dataframe of survey data
#' @param var_prefixes List of prefixes for variables to multiply
#' @param drop_items Boolean flag for whether to drop the variables being multiplied
#' @returns Dataframe containing composite variables
set_composite_products <- function(dat, var_prefixes, drop_items = TRUE) {
  for (prefix in var_prefixes) {
    dat <- dat |>
      rowwise() |>
      mutate (!!sym(prefix) := 
                case_when(
                  sum(pick(starts_with(prefix))) == 0 ~ NA,
                  .default = prod(pick(starts_with(prefix))))) |>
      ungroup()
  }
  
  if (drop_items) {
    var_prefixes_with_underscore <- str_c(var_prefixes, '_')
    dat <- dat |> 
      select(-starts_with(var_prefixes_with_underscore))
  }
  return(dat)
}

#' Construct delta variables by taking the difference between the 
#' value of that variable at a particular timepoint (4-weeks, 8-weeks, 12-weeks)
#' and its baseline value
#' 
#' @param dat Dataframe of survey data
#' @param vars List of variables to construct delta's for
#' @param timepoint_prefix String for the timepoint being used to compute the delta 
#' (mid, eot, followup)
set_delta_vars <- function(dat, vars, timepoint_prefix) {
  for (var in vars) {
      var_with_prefix <- str_c(timepoint_prefix, var, sep='_')
      dat <- dat |>
        mutate(!!sym(str_c('delta', var_with_prefix, sep='_')) := !!sym(var_with_prefix) - !!sym(var))
  }
  return (dat)
}

#' Construct retention status variable 
#' 
#' @param dat Dataframe of survey data
#' @param timepts List of strings for the timepoints at which to determine retention status
#' @returns Dataframe with retention status variables added
set_retention_status <- function(dat, timepts) {
  for (timept in timepts) {
    dat <- dat |> 
      mutate(!!sym(str_c('retained', timept, sep='_')) := 
               as.numeric(!!sym(str_c(timept, 'complete', sep='_')) == 2 & 
                            is.na(withdraw) & participant_id != 58 & 
                            participant_id != 180))
  }
  return (dat)
}
