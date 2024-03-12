get_cageaid_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, cageaid) |>
    mutate(subgroup = factor(cageaid, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_ther_status_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, ther_status) |>
    mutate(subgroup = factor(ther_status, ordered=FALSE)) |>
    # mutate(subgroup = addNA(subgroup)) |>
    select(participant_id, subgroup)
}

get_age_subgroups <- function(dat) {
  med_age <- median(dat$age, na.rm=TRUE)
  subgroups <- dat |>
    select(participant_id, age) |>
    mutate(subgroup = factor(as.numeric(age >= med_age), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_tobac_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, p30_tob) |>
    mutate(subgroup = factor(p30_tob != 0, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_empl_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, empl) |>
    mutate(subgroup = factor(empl, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_educ_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, educ) |>
    mutate(subgroup = factor(as.numeric(educ != 'hs'), ordered=FALSE)) |> 
    select(participant_id, subgroup)
}

get_insur_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, insur) |>
    mutate(subgroup = factor(as.numeric(insur != 'private'), ordered=FALSE)) |> 
    select(participant_id, subgroup)
}

get_pri_sub_is_alc_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, psub_1) |>
    mutate(subgroup = factor(psub_1 == 1, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_dast_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, dast) |>
    mutate(subgroup = factor(as.numeric(dast >= 3), ordered=FALSE)) |>
    # mutate(subgroup = addNA(subgroup)) |>
    select(participant_id, subgroup)
}

get_phq_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, phq) |>
    mutate(subgroup = factor(as.numeric(phq >= 10), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_gad_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, gad) |>
    mutate(subgroup = factor(as.numeric(gad >= 10), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_bscq_subgroups <- function(dat) {
  med_bscq <- median(dat$bscq, na.rm=TRUE)
  subgroups <- dat |>
    select(participant_id, bscq) |>
    mutate(subgroup = factor(as.numeric(bscq >= med_bscq), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_sipad_subgroups <- function(dat) {
  med_sipad <- median(dat$sipad, na.rm=TRUE)
  subgroups <- dat |>
    select(participant_id, sipad) |>
    mutate(subgroup = factor(as.numeric(sipad >= med_sipad), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_screen_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, screen) |>
    mutate(subgroup = factor(screen, ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_baseline_use_subgroups <- function(dat) {
  med_p30 <- median(dat$p30, na.rm=TRUE)
  sugroups <- dat |>
    select(participant_id, p30) |>
    mutate(subgroup = factor(as.numeric(p30 >= med_p30), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_taa_subgroups <- function(dat) {
  med_taa <- median(dat$taa, na.rm=TRUE)
  sugroups <- dat |>
    select(participant_id, taa) |>
    mutate(subgroup = factor(as.numeric(taa >= med_taa), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_qds_subgroups <- function(dat) {
  med_qds <- median(dat$qds, na.rm=TRUE)
  sugroups <- dat |>
    select(participant_id, qds) |>
    mutate(subgroup = factor(as.numeric(qds >= med_qds), ordered=FALSE)) |>
    # mutate(subgroup = addNA(subgroup)) |>
    select(participant_id, subgroup)
}

get_crave_subgroups <- function(dat) {
  sugroups <- dat |>
    select(participant_id, crave) |>
    mutate(subgroup = factor(as.numeric(crave >= 2), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

get_dx_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, mh) |>
    mutate(subgroup = factor(as.numeric(mh == 'no_history'), ordered=FALSE)) |>
    select(participant_id, subgroup)
}

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
    # mutate(subgroup = addNA(subgroup)) |>
    select(participant_id, subgroup)
}

get_subgrouping_fn <- function(subgroup_var) {
  if (subgroup_var == 'cageaid') {
    return (get_cageaid_subgroups)
  } else if (subgroup_var == 'ther_status') {
    return (get_ther_status_subgroups)
  } else if (subgroup_var == 'age') {
    return (get_age_subgroups)
  } else if (subgroup_var == 'p30_tob') {
    return (get_tobac_subgroups)
  } else if (subgroup_var == 'empl') {
    return (get_empl_subgroups)
  } else if (subgroup_var == 'educ') {
    return (get_educ_subgroups)
  } else if (subgroup_var == 'insur') {
    return (get_insur_subgroups)
  } else if (subgroup_var == 'pri_sub_is_alc') {
    return (get_pri_sub_is_alc_subgroups)
  } else if (subgroup_var == 'dast') {
    return (get_dast_subgroups)
  } else if (subgroup_var == 'phq') {
    return (get_phq_subgroups)
  } else if (subgroup_var == 'gad') {
    return (get_gad_subgroups) 
  } else if (subgroup_var == 'bscq') {
      return (get_bscq_subgroups)
  } else if (subgroup_var == 'sipad') {
    return (get_sipad_subgroups) 
  } else if (subgroup_var == 'screen') {
    return (get_screen_subgroups)
  } else if (subgroup_var == 'p30') {
    return (get_baseline_use_subgroups) 
  } else if (subgroup_var == 'taa') {
    return (get_taa_subgroups)
  } else if (subgroup_var == 'qds') {
    return (get_qds_subgroups)
  } else if (subgroup_var == 'crave') {
    return (get_crave_subgroups)
   } else if (subgroup_var == 'mh') {
    return (get_dx_subgroups)
   } else if (subgroup_var == 'csq') {
    return (get_csq_subgroups)
   }
   else {
    stop('Invalid subgrouping variable')
  }
}

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