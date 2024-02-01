get_cageaid_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, cageaid) |>
    mutate(subgroup = as.factor(cageaid)) |>
    select(participant_id, subgroup)
}

get_ther_status_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, ther_status) |>
    mutate(subgroup = as.factor(ther_status)) |>
    mutate(subgroup = addNA(subgroup)) |>
    select(participant_id, subgroup)
}

get_age_subgroups <- function(dat) {
  med_age <- median(dat$age, na.rm=TRUE)
  subgroups <- dat |>
    select(participant_id, age) |>
    mutate(subgroup = as.factor(as.numeric(age >= med_age))) |>
    select(participant_id, subgroup)
}

get_tobac_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, p30_tob) |>
    mutate(subgroup = as.factor(p30_tob != 0)) |>
    select(participant_id, subgroup)
}

get_empl_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, empl) |>
    mutate(subgroup = as.factor(empl)) |>
    select(participant_id, subgroup)
}

get_educ_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, educ) |>
    mutate(subgroup = as.factor(as.numeric(educ != 'hs'))) |> 
    select(participant_id, subgroup)
}

get_insur_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, insur) |>
    mutate(subgroup = as.factor(as.numeric(insur != 'private'))) |> 
    select(participant_id, subgroup)
}

get_pri_sub_is_alc_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, psub_1) |>
    mutate(subgroup = as.factor(psub_1 == 1)) |>
    select(participant_id, subgroup)
}

get_dast_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, dast) |>
    mutate(subgroup = as.factor(as.numeric(dast >= 3))) |>
    mutate(subgroup = addNA(subgroup)) |>
    select(participant_id, subgroup)
}

get_phq_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, phq) |>
    mutate(subgroup = as.factor(as.numeric(phq >= 10))) |>
    select(participant_id, subgroup)
}

get_gad_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, gad) |>
    mutate(subgroup = as.factor(as.numeric(gad >= 10))) |>
    select(participant_id, subgroup)
}

get_bscq_subgroups <- function(dat) {
  med_bscq <- median(dat$bscq, na.rm=TRUE)
  subgroups <- dat |>
    select(participant_id, bscq) |>
    mutate(subgroup = as.factor(as.numeric(bscq >= med_bscq))) |>
    select(participant_id, subgroup)
}

get_sipad_subgroups <- function(dat) {
  med_sipad <- median(dat$sipad, na.rm=TRUE)
  subgroups <- dat |>
    select(participant_id, sipad) |>
    mutate(subgroup = as.factor(as.numeric(sipad >= med_sipad))) |>
    select(participant_id, subgroup)
}

get_screen_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, screen) |>
    mutate(subgroup = as.factor(screen)) |>
    select(participant_id, subgroup)
}

get_baseline_use_subgroups <- function(dat) {
  med_p30 <- median(dat$p30, na.rm=TRUE)
  sugroups <- dat |>
    select(participant_id, p30) |>
    mutate(subgroup = as.factor(as.numeric(p30 >= med_p30))) |>
    select(participant_id, subgroup)
}

get_taa_subgroups <- function(dat) {
  med_taa <- median(dat$taa, na.rm=TRUE)
  sugroups <- dat |>
    select(participant_id, taa) |>
    mutate(subgroup = as.factor(as.numeric(taa >= med_taa))) |>
    select(participant_id, subgroup)
}

get_qds_subgroups <- function(dat) {
  med_qds <- median(dat$qds, na.rm=TRUE)
  sugroups <- dat |>
    select(participant_id, qds) |>
    mutate(subgroup = as.factor(as.numeric(qds >= med_qds))) |>
    mutate(subgroup = addNA(subgroup)) |>
    select(participant_id, subgroup)
}

get_crave_subgroups <- function(dat) {
  # med_crave <- median(dat$crave, na.rm=TRUE)
  sugroups <- dat |>
    select(participant_id, crave) |>
    mutate(subgroup = as.factor(as.numeric(crave >= 2))) |>
    select(participant_id, subgroup)
}

get_dx_subgroups <- function(dat) {
  subgroups <- dat |>
    select(participant_id, mh) |>
    mutate(subgroup = as.factor(as.numeric(mh == 'no_history'))) |>
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
      group_by(subgroup) |>
      summarise(n = n()) |>
      mutate(subgroup_var = subgroup_var)
    subgroup_counts <- rbind(subgroup_counts, subgroups)
  }
  return (subgroup_cnts)
}