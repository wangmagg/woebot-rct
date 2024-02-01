subset_randomized <- function(dat) {
  dat |> filter((Randomization_complete != 0) & (!is.na(group)))
}

subset_has_baseline <- function(dat) {
  dat |> filter(baseline_complete == 2)
}

subset_has_mid <- function(dat) {
  dat |> filter(mid_complete == 2)
}

subset_has_eot <- function(dat) {
  dat |> filter(eot_complete == 2)
}

subset_has_followup <- function(dat) {
  dat |> filter(followup_complete == 2)
}

subset_retained <- function(dat) {
  dat |> filter((is.na(withdraw) | participant_id == 180) & participant_id != 58 )
}

subset_has_p30 <- function(dat) {
  dat <- dat |> 
    filter(rowSums(!is.na(pick(starts_with('p30') & !contains('tob')))) != 0,
           rowSums(pick(starts_with('p30') & !contains('tob')), na.rm=TRUE) != 0)
}

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