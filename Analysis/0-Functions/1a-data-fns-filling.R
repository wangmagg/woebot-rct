### Functions for performing data replacement and imputation

#' Replace NA with zeros
#' 
#' @param dat Dataframe of survey data
#' @param var_prefixes List of prefixes for variables to perform filling operation on
#' @returns Dataframe with filled variables
fill_na_with_0 <- function(dat, var_prefixes) {
  for (var in var_prefixes) {
    dat <- dat |>
      mutate(across(starts_with(var), ~if_else(is.na(.x), 0, .x)))
  }
  return (dat)
}

#' Replace 99 with NA
#' 
#' @param dat Dataframe of survey data
#' @param var_prefixes List of prefixes for variables to perform filling operation on
#' @returns Dataframe with filled variables
fill_99_with_na <- function(dat, var_prefixes) {
  for (var in var_prefixes) {
    dat <- dat |>
      mutate(across(starts_with(var), ~case_when(.x == 99 ~ NA, .default=.x)))
  }
  return (dat)
}

#' Within-scale mean imputation (used for scales that contain multiple subitems, e.g. PHQ)
#' 
#' @param dat Dataframe of survey data
#' @param var_prefixes List of prefixes for variables to perform filling operation on
#' @param exclude_susbstr String that, if present in variable, means the variable is excluded
#' when computing the mean
#' @returns Dataframe with filled variables
fill_na_with_mean <- function(dat, var_prefixes, exclude_substr=NULL) {
  for (var in var_prefixes) {
    if (is.null(exclude_substr)) {
      dat <- dat |>
        mutate(across(starts_with(var),
                      ~if_else(is.na(.x), 
                               rowMeans(pick(starts_with(var)
                                             & !contains(cur_column())), na.rm=TRUE), .x)))
    } else {
      dat <- dat |>
        mutate(across(starts_with(var),
                      ~if_else(is.na(.x), 
                               rowMeans(pick(starts_with(var)
                                             & !contains(exclude_substr)
                                             & !contains(cur_column())), na.rm=TRUE), .x)))
    }
  }
  
  return (dat)
}

#' Column-mean imputation
#' 
#' @param dat Dataframe of survey data
#' @param vars List of columns to perform filling operation on
#' @returns Dataframe with filled variables
fill_column_mean <- function(dat, vars) { 
  dat <- dat |>
    mutate(across(all_of(vars), ~as.double(.x))) |>
    mutate(across(all_of(vars), ~replace_na(.x, mean(.x, na.rm=TRUE))))
}
