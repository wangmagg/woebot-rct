## Functions for subsetting rows of the dataframe

#' Subset to only include randomized participants
#' 
#' @param dat Dataframe of survey/screening data
subset_randomized <- function(dat) {
  dat |> 
    filter((Randomization_complete != 0) & (!is.na(group))) |>
    select(-Randomization_complete)
}

#' Subset to only include participants who completed the baseline survey
#' 
#' @param dat Dataframe of survey/screening data
subset_has_baseline <- function(dat) {
  dat |> filter(baseline_complete == 2)
}

#' Subset to only include participants who completed the 4-week survey
#' 
#' @param dat Dataframe of survey/screening data
subset_has_mid <- function(dat) {
  dat |> filter(mid_complete == 2)
}

#' Subset to only include participants who completed the 8-week survey
#' 
#' @param dat Dataframe of survey/screening data
subset_has_eot <- function(dat) {
  dat |> filter(eot_complete == 2)
}

#' Subset to only include participants who completed the 12-week survey
#' 
#' @param dat Dataframe of survey/screening data
subset_has_followup <- function(dat) {
  dat |> filter(followup_complete == 2)
}

#' Subset to only include participants who did not withdraw and were not removed
#' 
#' @param dat Dataframe of survey/screening data
subset_retained <- function(dat) {
  dat |> filter((is.na(withdraw) | participant_id == 180) & participant_id != 58 )
}

#' Subset to only include participants who have at least one non-missing 30-day use
#' 
#' @param dat Dataframe of survey/screening data
subset_has_p30 <- function(dat) {
  dat <- dat |> 
    filter(rowSums(!is.na(pick(starts_with('p30') & !contains('tob')))) != 0,
           rowSums(pick(starts_with('p30') & !contains('tob')), na.rm=TRUE) != 0)
}

#' Subset to only include participants who are not missing the specified outcome variable
#' 
#' @param dat Dataframe of survey/screening data
#' @param outcome_var Outcome variable name (heavy, sps, or dast)
#' @param timept String for timepoint at which variable was measured (eot, mid, followup)
#' NULL corresponds to the baseline variable measurement
subset_has_outcome <- function(dat, outcome_var, timept=NULL) {
  if (str_detect(outcome_var, 'heavy')) {
    dat<- dat |> filter(!is.na(p30_alc))
  } else if (str_detect(outcome_var, 'sps')) {
    sps_timept <- str_c(timept, 'sps', sep='_')
    dat <- dat |>
      select(-sps, -!!sym(sps_timept)) |>
      set_composite_sums(c('sps', sps_timept)) |>
      set_delta_vars(c('sps'), timept)
  } else if (str_detect(outcome_var, 'dast')) {
    dast_timept <- str_c(timept, 'dast', sep='_')
    dat <- dat |>
      select(-dast, -!!sym(dast_timept)) |>
      set_composite_sums(c('dast', dast_timept)) |>
      set_delta_vars(c('dast'), timept)
  } 
  dat <- dat |> drop_na(!!sym(outcome_var))
  return (dat)
}

#' Subset to only include participants who withdrew/were removed and who 
#' who were missing all past 30-day use substance variables
#' 
#' @param dat Dataframe of survey/screening data
subset_not_retained_no_p30 <- function(dat) {
  dat |> 
    filter((!is.na(withdraw) & participant_id != 180) | participant_id == 58 ) |>
    filter(rowSums(!is.na(pick(starts_with('p30') & !contains('tob')))) == 0 |
           rowSums(pick(starts_with('p30') & !contains('tob')), na.rm=TRUE) == 0)
}

#' Subset to only include per-protocol participants (active for at least 4 weeks)
#' 
#' @param dat Dataframe of survey/screening data
subset_per_protocol <- function(data) {
  data |>
    mutate(weeks_active = rowSums(pick(starts_with("days_active_w")) > 0), na.rm=TRUE) |>
    mutate(n_pdfs = rowSums(across(w1_complete:w8_complete), na.rm=TRUE)) |>
    filter(group == 1 & weeks_active >=4 | group == 2 & n_pdfs >=4)
}
