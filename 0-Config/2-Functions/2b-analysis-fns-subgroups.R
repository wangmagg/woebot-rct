### Functions for defining subgroups

#' Define subgroups by gender (man, woman)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_gender_subgroups <- function(dat) {
  subgroups <- dat |>
    mutate(gender=fct_collapse(gender, "2" = c("2", "4"))) |>
    select(participant_id, gender) |>
    mutate(subgroup = factor(gender), ordered=FALSE) |>
    select(participant_id, subgroup)
}

#' Define subgroups by race (White, Not White)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_race_subgroups <- function(dat) {
  subgroups <- dat |>
    mutate(race = relevel(
      fct_collapse(race, "not_white" = c("black", "other")),
      ref="white"
      )) |>
    select(participant_id, race) |>
    mutate(subgroup = factor(race), ordered=FALSE) |>
    select(participant_id, subgroup)
}

#' Define subgroups by ethnicity (Hispanic, Non-hispanic, Missing)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_eth_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, eth) |>
    mutate(subgroup = factor(eth), ordered=FALSE) |>
    mutate(subgroup = case_when(eth == 99 ~ NA,
                     TRUE ~ subgroup)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by CAGE-AID (2, 3, 4)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_cageaid_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, cageaid) |>
    mutate(subgroup = factor(cageaid, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by therapy status (0, 1, 2)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_ther_status_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, ther_status) |>
    mutate(subgroup = factor(ther_status, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by age (below, above medidan)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_age_subgroups <- function(dat) {
  med_age <- median(dat$age, na.rm=TRUE)
  subgroups <- dat |>
    select(participant_id, age) |>
    mutate(subgroup = factor(as.numeric(age >= med_age), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by tobacco use (no use, at least 1 day of use)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_tobac_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, p30_tob) |>
    mutate(subgroup = factor(p30_tob != 0, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by employment status (employed, unemployed)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_empl_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, empl) |>
    mutate(subgroup = factor(empl, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by education (high school, college and above)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_educ_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, educ) |>
    mutate(subgroup = factor(as.numeric(educ != 'hs'), ordered=FALSE)) |> 
    select(participant_id, subgroup)
}

#' Define subgroups by insurance status (private, not private)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_insur_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, insur) |>
    mutate(subgroup = factor(as.numeric(insur != 'private'), ordered=FALSE)) |> 
    select(participant_id, subgroup)
}

#' Define subgroups by primary problematic substance (alcohol, not alcohol)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_pri_sub_is_alc_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, psub_1) |>
    mutate(subgroup = factor(psub_1 == 1, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by DAST (below 3, 3 or above)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_dast_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, dast) |>
    mutate(subgroup = factor(as.numeric(dast >= 3), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by PHQ (below 10, 10 or above)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_phq_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, phq) |>
    mutate(subgroup = factor(as.numeric(phq >= 10), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by GAD (below 10, 10 or and above)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_gad_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, gad) |>
    mutate(subgroup = factor(as.numeric(gad >= 10), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by BSCQ (below median, above median)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_bscq_subgroups <- function(dat) {
  med_bscq <- median(dat$bscq, na.rm=TRUE)
  subgroups <- dat |>
    select(participant_id, bscq) |>
    mutate(subgroup = factor(as.numeric(bscq >= med_bscq), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by SIPAD (below median, above median)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_sipad_subgroups <- function(dat) {
  med_sipad <- median(dat$sipad, na.rm=TRUE)
  subgroups <- dat |>
    select(participant_id, sipad) |>
    mutate(subgroup = factor(as.numeric(sipad >= med_sipad), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by screening type (REDCap, Qualtrics)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_screen_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, screen) |>
    mutate(subgroup = factor(screen, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by past 30d substance use at baseline (below median, above median)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_baseline_use_subgroups <- function(dat) {
  med_p30 <- median(dat$p30, na.rm=TRUE)
  sugroups <- dat |>
    select(participant_id, p30) |>
    mutate(subgroup = factor(as.numeric(p30 >= med_p30), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by TAA (below median, above median)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_taa_subgroups <- function(dat) {
  med_taa <- median(dat$taa, na.rm=TRUE)
  sugroups <- dat |>
    select(participant_id, taa) |>
    mutate(subgroup = factor(as.numeric(taa >= med_taa), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by QDS, drinks per week (below median, above median)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_qds_subgroups <- function(dat) {
  med_qds <- median(dat$qds, na.rm=TRUE)
  sugroups <- dat |>
    select(participant_id, qds) |>
    mutate(subgroup = factor(as.numeric(qds >= med_qds), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by cravings (less than 2, 2 or above)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_crave_subgroups <- function(dat) {
  sugroups <- dat |>
    select(participant_id, crave) |>
    mutate(subgroup = factor(as.numeric(crave >= 2), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by mental health diagnosis (has had diagnosis, no history of diagnoses)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_dx_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, mh) |>
    mutate(subgroup = factor(as.numeric(mh == 'no_history'), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Define subgroups by CSQ (low, medium high)
#' 
#' @param dat Dataframe of survey data
#' @returns 2-column dataframe with participant ID and subgroup label
get_csq_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, eot_csq) |>
    mutate(subgroup = 
             case_when(
               eot_csq <= 25 ~ 'low',
               eot_csq >= 26 & eot_csq <= 30 ~ 'medium',
               eot_csq >= 31 & eot_csq <= 32 ~ 'high',
               TRUE ~ NA_character_
             )) |>
    mutate(subgroup = factor(subgroup, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

#' Get the subgrouping function corresponding to the specified variable
#' 
#' @param subgroup_var Name of subgroup variable
#' @returns Subgrouping function
get_subgrouping_fn <- function(subgroup_var) {
  switch(
    subgroup_var,
    "gender" = get_gender_subgroups,
    "race" = get_race_subgroups,
    "eth" = get_eth_subgroups,
    "cageaid" = get_cageaid_subgroups,
    "ther_status" = get_ther_status_subgroups,
    "age" = get_age_subgroups,
    "p30_tob" = get_tobac_subgroups,
    "empl" = get_empl_subgroups,
    "educ" = get_educ_subgroups,
    "insur" = get_insur_subgroups,
    "pri_sub_is_alc" = get_pri_sub_is_alc_subgroups,
    "dast" = get_dast_subgroups,
    "phq" = get_phq_subgroups,
    "gad" = get_gad_subgroups,
    "bscq" = get_bscq_subgroups,
    "sipad" = get_sipad_subgroups,
    "screen" = get_screen_subgroups,
    "p30" = get_baseline_use_subgroups,
    "taa" = get_taa_subgroups,
    "qds" = get_qds_subgroups,
    "crave" = get_crave_subgroups,
    "mh" = get_dx_subgroups,
    "csq" = get_csq_subgroups
  )
}

#' Get counts per subgroup 
#' 
#' @param dat Dataframe of survey data
#' @param subgroup_vars List of subgroup variable names
#' @returns Dataframe of subgroup cell counts
get_subgroup_cell_counts <- function(dat, subgroup_vars) {
  subgroup_cnts <- data.frame()
  for (subgroup_var in subgroup_vars) {
    subgrouping_fn <- get_subgrouping_fn(subgroup_var)
    subgroups <- subgrouping_fn(dat) 
    subgroups <- subgroups |>
      filter(!is.na(subgroup)) |>
      group_by(subgroup) |>
      summarise(n = n()) |>
      mutate(subgroup_var = subgroup_var)
    subgroup_counts <- rbind(subgroup_counts, subgroups)
  }
  return (subgroup_cnts)
}