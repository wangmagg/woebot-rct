reverse_code_sps_items <- function(dat) {
  dat <- dat |>
    mutate(sps_1 = case_when(sps_1 != 99 ~ (6 - sps_1), .default=99), 
           sps_3 = case_when(sps_3 != 99 ~ (6 - sps_3), .default=99),
           sps_4 = case_when(sps_4 != 99 ~ (6 - sps_4), .default=99))
}

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

recode_continuous_sps_for_branching <- function(dat, sps_vars) {
  for (sps_var in sps_vars) {
    dat <- dat |>
      mutate(!!sym(sps_var) := case_when(rowSums(!is.na(pick(starts_with(sps_var)))) == 0 ~ 0,
                                         .default=!!sym(sps_var)))
  }
  return(dat)
}

revert_continuous_sps <- function(dat, sps_vars) {
  for (sps_var in sps_vars) {
    if (sps_var %in% colnames(dat)) {
      dat <- dat |>
        mutate(!!sym(sps_var) := case_when(!!sym(sps_var) == 0 ~ NA_real_,
                                           .default=!!sym(sps_var)))
    }
  }
  return(dat)
}

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

recode_continuous_dast_for_branching <- function(dat, dast_vars) {
  for (dast_var in dast_vars) {
    dat <- dat |>
      mutate(!!sym(dast_var) := case_when(rowSums(!is.na(pick(starts_with(dast_var)))) == 0 ~ 0,
                                         .default=!!sym(dast_var) + 10))
  }
  return(dat)
}

revert_continuous_dast <- function(dat, dast_vars) {
  for (dast_var in dast_vars) {
    if (dast_var %in% colnames(dat)) {
      dat <- dat |>
        mutate(!!sym(dast_var) := case_when(!!sym(dast_var) == 0 ~ NA_real_,
                                            .default=!!sym(dast_var)))
    }
  }
  return(dat)
}

set_therapy_status <- function(dat) {
  dat <- dat |>
    mutate(ther_status = 
             case_when((ther == 0 & trt_6 == 1) ~ 0,
                       ther == 1 & trt_6 == 1 ~ 1,
                       trt_6 != 1 & trt_99 != 1 ~ 2,
                       .default = NA))
}

set_factors <- function(dat, vars, add_na=TRUE, ordered=FALSE) {
  dat <- dat |>
    mutate(across(all_of(vars), ~factor(.x, ordered=ordered)))
  
  if (add_na) {
    dat <- dat |>
      mutate_if(~(sum(is.na(.x)) > 0 & is.factor(.x)), addNA)
  }
  
  return(dat)
}

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
      )
    ) |>
    select(-starts_with('race_'), -starts_with('trt_'), -starts_with('mh_'))
}

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

set_delta_vars <- function(dat, vars, timepoint_prefix) {
  for (var in vars) {
      var_with_prefix <- str_c(timepoint_prefix, var, sep='_')
      dat <- dat |>
        mutate(!!sym(str_c('delta', var_with_prefix, sep='_')) := !!sym(var_with_prefix) - !!sym(var))
  }
  return (dat)
}

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
