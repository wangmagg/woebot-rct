get_group_balance <- function(dat_input, grouping_var) {
  group_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n = n())
}

get_age_balance <- function(dat_input, grouping_var) {
  age_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(age_mean = mean(age), 
              age_sd = sd(age),
              age_min = min(age),
              age_max = max(age)) |>
    ungroup()
}

get_sex_balance <- function(dat_input, grouping_var) {
  dat_input |> 
    group_by(across(all_of(grouping_var))) |>
    summarize(n_f = sum(sex == 1), p_f = mean(sex == 1),
              n_m = sum(sex == 2), p_m = mean(sex == 2),
              n_pna = sum(sex == 99, na.rm=TRUE), p_pna = n_pna / n())
}

get_gender_balance <- function(dat_input, grouping_var) {
  gender_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_m = sum(gender == 1, na.rm=TRUE), p_m = n_m / n(),
              n_w = sum(gender == 2, na.rm=TRUE), p_w = n_w / n(),
              n_b = sum(gender == 4, na.rm=TRUE), p_b = n_b / n(),
              n_e = sum(gender == 3 | gender == 5 | gender == 6, na.rm=TRUE), p_e = n_e / n(),
              n_pna = sum(gender == 99, na.rm=TRUE), p_pna = n_pna / n())
}

get_orient_balance <- function(dat_input, grouping_var) {
  orient_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_h = sum(orient == 1, na.rm=TRUE), p_h = n_h / n(),
              n_gl = sum(orient == 2, na.rm=TRUE), p_gl = n_gl / n(),
              n_b = sum(orient == 3, na.rm=TRUE), p_b = n_b / n(),
              n_q = sum(orient == 4, na.rm=TRUE), p_q = n_q / n(),
              n_p = sum(orient == 5, na.rm=TRUE), p_p = n_p / n(),
              n_a = sum(orient == 6, na.rm=TRUE), p_a = n_a / n(),
              n_e = sum(orient == 7, na.rm=TRUE), p_e = n_e / n(),
              n_d = sum(orient == 8, na.rm=TRUE), p_d = n_d / n(),
              n_pna = sum(orient == 99, na.rm=TRUE), p_pna = n_pna / n()
    )
}

get_eth_balance <- function(dat_input, grouping_var) {
  eth_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_h = sum(eth == 1, na.rm=TRUE), p_h = n_h / n(),
              n_nh = sum(eth == 0, na.rm=TRUE), p_nh = n_nh / n(),
              n_pna = sum(eth == 99, na.rm=TRUE), p_pna = n_pna / n())
}

get_race_balance <- function(dat_input, grouping_var) {
  race_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_w = sum(race_5, na.rm=TRUE), p_w = n_w / n(),
              n_b = sum(race_3, na.rm=TRUE), p_b = n_b / n(),
              n_aa = sum(race_2, na.rm=TRUE), p_aa = n_aa / n(),
              n_ai = sum(race_1, na.rm=TRUE), p_ai = n_ai / n(),
              n_mult = sum(race_mult, na.rm=TRUE), p_mult = n_mult / n(),
              n_o = sum(race_6, na.rm=TRUE), p_o = n_o / n(),
              n_pna = sum(race_99, na.rm=TRUE), p_pna = n_pna / n())
}

get_marital_balance <- function(dat_input, grouping_var) {
  marital_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_mcp = sum(marital == 1, na.rm=TRUE), p_mcp = n_mcp / n(),
              n_dsw = sum(marital == 2 | marital == 3, na.rm=TRUE), p_dsw = n_dsw / n(),
              n_ns = sum(marital == 4, na.rm=TRUE), p_ns = n_ns / n(),
              n_pna = sum(marital == 99, na.rm=TRUE), p_pna = n_pna / n())
}

get_educ_balance <- function(dat_input, grouping_var) {
  educ_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_hs = sum(educ == 1 | educ == 2 | educ == 3, na.rm=TRUE), p_hs = n_hs / n(),
              n_ct = sum(educ == 4, na.rm=TRUE), p_ct = n_ct / n(),
              n_c = sum(educ == 5, na.rm=TRUE), p_c = n_c / n(),
              n_g = sum(educ == 6, na.rm=TRUE), p_g = n_g / n(),
              n_pna = sum(educ == 99, na.rm=TRUE), p_pna = n_pna / n())
}

get_empl_balance <- function(dat_input, grouping_var) {
  empl_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_e = sum(empl == 2 | empl == 3, na.rm=TRUE), p_e = n_e / n(),
              n_u = sum(empl == 1 | empl == 4 | empl == 5 | empl == 6 | empl == 7 | empl == 8, na.rm=TRUE), p_u = n_u / n(),
              n_pna = sum(empl == 99, na.rm=TRUE), p_pna = n_pna / n())
}

get_disab_balance <- function(dat_input, grouping_var) {
  dis_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_d = sum(disab == 1, na.rm=TRUE), p_d = n_d / n(),
              n_nd = sum(disab == 0, na.rm=TRUE), p_nd = n_nd / n(),
              n_pna = sum(disab == 99, na.rm=TRUE), p_pna = n_pna / n())
}

get_insur_balance <- function(dat_input, grouping_var) {
  insur_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_p = sum(insur == 1, na.rm=TRUE), p_p = n_p / n(),
              n_m = sum(insur == 2 | insur == 3, na.rm=TRUE), p_m = n_m / n(),
              n_t = sum(insur == 4, na.rm=TRUE), p_t = n_t / n(),
              n_v = sum(insur == 5, na.rm=TRUE), p_v = n_v / n(),
              n_i = sum(insur == 6, na.rm=TRUE), p_i = n_i / n(),
              n_d = sum(insur == 7, na.rm=TRUE), p_d = n_d / n(),
              n_pna = sum(insur == 99, na.rm=TRUE), p_pna = n_pna / n())
}

get_ther_balance <- function(dat_input, grouping_var) {
  ther_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_nev = sum(ther_status == 0, na.rm=TRUE), p_nev = n_nev / n(),
              n_for = sum(ther_status == 1, na.rm=TRUE), p_for = n_for / n(),
              n_cur = sum(ther_status == 2, na.rm=TRUE), p_cur = n_cur / n())
}

get_trt_balance <- function(dat_input, grouping_var) {
  trt_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_tot = sum((trt_1 + trt_2 + trt_3 + trt_4 + trt_5 + trt_7) > 0, na.rm=TRUE), p_tot = n_tot / n(),
              n_sh = sum(trt_1 == 1 & !is.na(trt_sub) & (trt_sub > 0), na.rm=TRUE), p_sh = n_sh / n(),
              n_op = sum(trt_2 == 1 & !is.na(trt_sub) & (trt_sub > 0), na.rm=TRUE), p_op = n_op / n(),
              n_ir = sum(trt_3 == 1| trt_4 ==1 & !is.na(trt_sub) & (trt_sub > 0), na.rm=TRUE), p_ir = n_ir / n(),
              n_pna = sum(trt_99, na.rm=TRUE), p_pna = n_pna / n())
}

get_med_balance <- function(dat_input, grouping_var) {
  med_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_med = sum(med == 1, na.rm=TRUE), p_med = n_med / n(),
              n_pna = sum(med == 99, na.rm=TRUE), p_pna = n_pna / n())
}

get_psych_med_balance <- function(dat_input, grouping_var) {
  psych_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_med = sum(psych_med == 1, na.rm=TRUE), p_med = n_med / n(),
              n_nmed = sum(psych_med == 0, na.rm=TRUE), p_nmed = n_nmed / n(),
              n_pna = sum(psych_med == 99, na.rm=TRUE), p_pna = n_pna / n())
}

get_mh_balance <- function(dat_input, grouping_var, mult_inclusive=TRUE) {
  if (mult_inclusive) {
    dat_input |>
      group_by(across(all_of(grouping_var))) |>
      mutate(mh_mult = rowSums(pick(mh_1, mh_2, mh_3, mh_4, mh_5, mh_6, mh_7, mh_8, mh_9, mh_10, mh_11, mh_12), na.rm=TRUE) > 1,
             mh_other = rowSums(pick(mh_1, mh_2, mh_6, mh_7, mh_8, mh_10), na.rm=TRUE) > 0) |>
      summarize(n_nh = sum(mh_12 == 1), p_nh = n_nh / n(),
                n_ud = sum(mh_4 == 1), p_ud = n_ud / n(), 
                n_bp = sum(mh_3 == 1), p_bp = n_bp / n(),
                n_ad = sum(mh_5 == 1), p_ad = n_ad / n(),
                n_sud = sum(mh_9 == 1), p_sud = n_sud / n(),
                n_o = sum(mh_other), p_o = n_o / n(),
                n_mult = sum(mh_mult), p_mult = n_mult / n(),
                n_pna = sum(mh_99 == 1, na.rm=TRUE), p_pna = n_pna / n())
  } else {
    dat_input |>
      group_by(across(all_of(grouping_var))) |>
      mutate(mh_mult = rowSums(pick(mh_1, mh_2, mh_3, mh_4, mh_5, mh_6, mh_7, mh_8, mh_9, mh_10, mh_11, mh_12), na.rm=TRUE) > 1,
             mh_other = rowSums(pick(mh_1, mh_2, mh_6, mh_7, mh_8, mh_10), na.rm=TRUE) > 0) |>
      summarize(n_nh = sum(mh_12 == 1 & !mh_mult, na.rm=TRUE), p_nh = n_nh / n(),
                n_ud = sum((mh_4 == 1 | mh_11 == 1) & !mh_mult, na.rm=TRUE), p_ud = n_ud / n(), # one response of 11 was recoded to 4
                n_bp = sum(mh_3 == 1 & !mh_mult, na.rm=TRUE), p_bp = n_bp / n(),
                n_ad = sum(mh_5 == 1 & !mh_mult, na.rm=TRUE), p_ad = n_ad / n(),
                n_sud = sum(mh_9 == 1 & !mh_mult, na.rm=TRUE), p_sud = n_sud / n(),
                n_o = sum(mh_other & !mh_mult), p_o = n_o / n(),
                n_mult = sum(mh_mult), p_mult = n_mult / n(),
                n_pna = sum(mh_99 == 1 & !mh_mult, na.rm=TRUE), p_pna = n_pna / n())
  }
}

get_phq_balance <- function(dat_input, grouping_var) {
  phq_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n = sum(!is.na(phq)),
              p = n / n(),
              phq_mean = mean(phq, na.rm=TRUE),
              phq_sd = sd(phq, na.rm=TRUE),
              phq_min = min(phq, na.rm=TRUE),
              phq_max = max(phq, na.rm=TRUE),
              n_modsev = sum(phq >= 10, na.rm=TRUE),
              p_modsev = n_modsev / n())
}

get_gad_balance <- function(dat_input, grouping_var) {
  gad_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n = sum(!is.na(gad)),
              p = n / n(),
              gad_mean = mean(gad, na.rm=TRUE),
              gad_sd = sd(gad, na.rm=TRUE),
              gad_min = min(gad, na.rm=TRUE),
              gad_max = max(gad, na.rm=TRUE),
              n_modsev = sum(gad >= 10, na.rm=TRUE),
              p_modsev = n_modsev / n())
}

get_sps_balance <- function(dat_input, grouping_var) {
  sps_bal <- dat_input |> 
    group_by(across(all_of(grouping_var))) |>
    summarize(n =sum(!is.na(sps)),
              p = n / n(),
              n_sps_6_10 = sum(sps >= 6 & sps <= 10, na.rm=TRUE),
              p_sps_6_10 = n_sps_6_10 / n(),
              n_sps_11_15 = sum(sps >= 11 & sps <= 15, na.rm=TRUE),
              p_sps_11_15 = n_sps_11_15 / n(),
              n_sps_16_20 = sum(sps >= 16 & sps <= 20, na.rm=TRUE),
              p_sps_16_20 = n_sps_16_20 / n(),
              n_sps_21_25 = sum(sps >= 21 & sps <= 25, na.rm=TRUE),
              p_sps_21_25 = n_sps_21_25 / n(),
              n_sps_26_30 = sum(sps >= 26 & sps <= 30, na.rm=TRUE),
              p_sps_26_30 = n_sps_26_30 / n()
    ) |>
    pivot_longer(
      -all_of(grouping_var),
      names_to = 'metric',
      values_to = 'value'
    )
}

get_pst_sub_balance <- function(dat_input, grouping_var) {
  dat_input |>
    group_by(across(all_of(grouping_var))) |>
    mutate_at(vars(starts_with('psub_')), list(~if_else(is.na(.), 0, .))) |>
    mutate(psub_1_main = (psub_1 == 1 | psub_1 == 3 | psub_1 == 7 | psub_1 == 10 | psub_1 == 11),
           psub_2_main = (psub_2 == 1 | psub_2 == 3 | psub_2 == 7 | psub_2 == 10 | psub_2 == 11),
           psub_3_main = (psub_3 == 1 | psub_3 == 3 | psub_3 == 7 | psub_3 == 10 | psub_3 == 11),
           psub_1_oth = (psub_1 == 2 | psub_1 == 4 | psub_1 == 5 | psub_1 == 6 | psub_1 == 8 | psub_1 == 9 | psub_1 == 12),
           psub_2_oth = (psub_2 == 2 | psub_2 == 4 | psub_2 == 5 | psub_2 == 6 | psub_2 == 8 | psub_2 == 9 | psub_2 == 12),
           psub_3_oth = (psub_3 == 2 | psub_3 == 4 | psub_3 == 5 | psub_3 == 6 | psub_3 == 8 | psub_3 == 9 | psub_3 == 12)) |>
    summarize(n_alc = sum(psub_1 == 1 | psub_2 == 1 | psub_3 == 1, na.rm=TRUE),
              p_alc = n_alc / n(),
              n_can = sum(psub_1 == 7 | psub_2 == 7 | psub_3 == 7, na.rm=TRUE),
              p_can = n_can / n(),
              n_sti = sum(psub_1 == 10 | psub_2 == 10 | psub_3 == 10, na.rm=TRUE),
              p_sti = n_sti / n(),
              n_coc = sum(psub_1 == 3 | psub_2 == 3 | psub_3 == 3, na.rm=TRUE),
              p_coc = n_coc / n(),
              n_tob = sum(psub_1 == 11 | psub_2 == 11 | psub_3 == 11, na.rm=TRUE),
              p_tob = n_tob / n(),
              n_oth_any = sum(psub_1_oth | psub_2_oth | psub_3_oth, na.rm=TRUE), p_oth_any = n_oth_any / n(),
              n_oth_all = sum(psub_1_oth & psub_2_oth & psub_3_oth, na.rm=TRUE),
              n_main_any = sum((psub_1_main | psub_2_main | psub_3_main), na.rm=TRUE),
              n_main_all = sum(psub_1_main & psub_2_main & psub_3_main, na.rm=TRUE),
              n_pna_all = sum(psub_1 == 99 & psub_2 == 99 & psub_3 == 99, na.rm=TRUE),
    )
}

get_pri_sub_balance <- function(dat_input, grouping_var) {
  pri_sub_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_alc = sum(psub_1 == 1, na.rm=TRUE),
              p_alc = n_alc / n(),
              n_can = sum(psub_1 == 7, na.rm=TRUE),
              p_can = n_can / n(),
              n_stim = sum(psub_1 == 10, na.rm=TRUE),
              p_stim = n_stim / n(),
              n_coc = sum(psub_1 == 3, na.rm=TRUE),
              p_coc = n_coc / n(),
              n_tob = sum(psub_1 == 11, na.rm=TRUE),
              p_tob = n_tob / n())
  
}

get_sec_sub_balance <- function(dat_input, grouping_var) {
  sec_sub_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_alc = sum(psub_2 == 1, na.rm=TRUE),
              p_alc = n_alc / n(),
              n_can = sum(psub_2 == 7, na.rm=TRUE),
              p_can = n_can / n(),
              n_stim = sum(psub_2 == 10, na.rm=TRUE),
              p_stim = n_stim / n(),
              n_coc = sum(psub_2 == 3, na.rm=TRUE),
              p_coc = n_coc / n(),
              n_tob = sum(psub_2 == 11, na.rm=TRUE),
              p_tob = n_tob / n())
}

get_ter_sub_balance <- function(dat_input, grouping_var) {
  ter_sub_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_alc = sum(psub_3 == 1, na.rm=TRUE),
              p_alc = n_alc / n(),
              n_can = sum(psub_3 == 7, na.rm=TRUE),
              p_can = n_can / n(),
              n_stim = sum(psub_3 == 10, na.rm=TRUE),
              p_stim = n_stim / n(),
              n_coc = sum(psub_3 == 3, na.rm=TRUE),
              p_coc = n_coc / n(),
              n_tob = sum(psub_3 == 11, na.rm=TRUE),
              p_tob = n_tob / n())
}

get_dast_balance <- function(dat_input, grouping_var) {
  dast_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n = sum(!is.na(dast)),
              dast_mean = mean(dast, na.rm=TRUE),
              dast_sd = sd(dast, na.rm=TRUE),
              dast_min = min(dast, na.rm=TRUE),
              dast_max = max(dast, na.rm=TRUE),
              n_modsev = sum(dast >= 3, na.rm=TRUE),
              p_modsev = n_modsev / n())
}

get_sipad_balance <- function(dat_input, grouping_var) {
  sipad_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n = sum(!is.na(sipad)),
              sipad_mean = mean(sipad, na.rm=TRUE),
              sipad_sd = sd(sipad, na.rm=TRUE),
              sipad_min = min(sipad, na.rm=TRUE),
              sipad_max = max(sipad, na.rm=TRUE))
}

get_cageaid_balance <- function(dat_input, grouping_var) {
  cageaid_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n = sum(!is.na(sipad)),
              cageaid_mean = mean(cageaid, na.rm=TRUE),
              cageaid_sd = sd(cageaid, na.rm=TRUE),
              cageaid_min = min(cageaid, na.rm=TRUE),
              cageaid_max = max(cageaid, na.rm=TRUE))
}

get_bscq_balance <- function(dat_input, grouping_var) {
  bscq_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n = sum(!is.na(bscq)),
              bscq_mean = mean(bscq, na.rm=TRUE),
              bscq_sd = sd(bscq, na.rm=TRUE),
              bscq_min = min(bscq, na.rm=TRUE),
              bscq_max = max(bscq, na.rm=TRUE))
}

get_crave_balance <- function(dat_input, grouping_var) {
  crave_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n = sum(!is.na(crave)),
              crave_mean = mean(crave, na.rm=TRUE),
              crave_sd = sd(crave, na.rm=TRUE),
              crave_min = min(crave, na.rm=TRUE),
              crave_max = max(crave, na.rm=TRUE))
}

get_p30_per_sub_balance <- function(dat_input, grouping_var) {
  p30_per_sub_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(across(starts_with("p30_"),
                     list(mean = ~mean(.x[.x != 99], na.rm=TRUE),
                          sd = ~sd(.x[.x != 99], na.rm=TRUE),
                          min = ~min(.x[.x != 99], na.rm=TRUE),
                          max = ~max(.x[.x != 99], na.rm=TRUE),
                          n_any = ~sum(.x > 0 & .x != 99, na.rm=TRUE),
                          p_any = ~sum(.x > 0 & .x != 99, na.rm=TRUE) / n()))) |>
    pivot_longer(
      cols = -all_of(grouping_var),
      names_to = c('substance', '.value') ,
      names_pattern = paste0('(', 
                             str_c('p30_', c('alc','can','coc','sti','met', 'inh', 
                                             'sed', 'hal', 'sop', 'pop', 'tob'), collapse='|'),
                             ')', 
                             '(.*)', 
                             collapse=''))
}

get_p30_balance <- function(dat_input, grouping_var) {
  p30_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n = sum(!is.na(p30)),
              p30_mean = mean(p30, na.rm=TRUE),
              p30_sd = sd(p30, na.rm=TRUE),
              p30_min = min(p30, na.rm=TRUE),
              p30_max = max(p30, na.rm=TRUE))
}

get_qds_balance <- function(dat_input, grouping_var) {
  qds_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n_dpw = sum(!is.na(qds_1)),
              days_per_week_mean = mean(qds_1, na.rm=TRUE),
              days_per_week_sd = sd(qds_1, na.rm=TRUE),
              days_per_week_min = min(qds_1, na.rm=TRUE),
              days_per_week_max = max(qds_1, na.rm=TRUE),
              n_dpd = sum(!is.na(qds_2)),
              drinks_per_day_mean = mean(qds_2, na.rm=TRUE),
              drinks_per_day_sd = sd(qds_2, na.rm=TRUE),
              drinks_per_day_min = min(qds_2, na.rm=TRUE),
              drinks_per_day_max = max(qds_2, na.rm=TRUE),
              n_heavy = sum(!is.na(heavy)),
              heavy_mean = mean(heavy, na.rm=TRUE),
              heavy_sd = sd(heavy, na.rm=TRUE),
              heavy_min = min(heavy, na.rm=TRUE),
              heavy_max = max(heavy, na.rm=TRUE))
}

get_taa_balance <- function(dat_input, grouping_var) {
  taa_bal <- dat_input |>
    group_by(across(all_of(grouping_var))) |>
    summarize(n = sum(!is.na(taa)),
              taa_mean = mean(taa, na.rm=TRUE),
              taa_sd = sd(taa, na.rm=TRUE),
              taa_min = min(taa, na.rm=TRUE),
              taa_max = max(taa, na.rm=TRUE))
}

get_balance <- function(dat_input, grouping_var, var_names, save_prefix, save_dir) {
  if (!file.exists(save_dir)) {
    dir.create(save_dir)
  }
  
  for (var_name in var_names) {
    if (var_name == 'group') {
      bal <- get_group_balance(dat_input, grouping_var)
    } else if (var_name == 'age') {
      bal <- get_age_balance(dat_input, grouping_var)
    } else if (var_name == 'sex') {
      bal <- get_sex_balance(dat_input, grouping_var)
    } else if (var_name == 'gender') {
      bal <- get_gender_balance(dat_input, grouping_var)
    } else if (var_name == 'orient') {
      bal <- get_orient_balance(dat_input, grouping_var)
    } else if (var_name == 'eth') {
      bal <- get_eth_balance(dat_input, grouping_var)
    } else if (var_name == 'race') {
      bal <- get_race_balance(dat_input, grouping_var)
    } else if (var_name == 'marital') {
      bal <- get_marital_balance(dat_input, grouping_var)
    } else if (var_name == 'educ') {
      bal <- get_educ_balance(dat_input, grouping_var)
    } else if (var_name == 'empl') {
      bal <- get_empl_balance(dat_input, grouping_var)
    } else if (var_name == 'disab') {
      bal <- get_disab_balance(dat_input, grouping_var)
    } else if (var_name == 'insur') {
      bal <- get_insur_balance(dat_input, grouping_var)
    } else if (var_name == 'ther') {
      bal <- get_ther_balance(dat_input, grouping_var)
    } else if (var_name == 'trt') {
      bal <- get_trt_balance(dat_input, grouping_var)
    } else if (var_name == 'med') {
      bal <- get_med_balance(dat_input, grouping_var)
    } else if (var_name == 'psych_med') {
      bal <- get_psych_med_balance(dat_input, grouping_var)
    } else if (var_name == 'mh') {
      bal <- get_mh_balance(dat_input, grouping_var)
    } else if (var_name == 'phq') {
      bal <- get_phq_balance(dat_input, grouping_var)
    } else if (var_name == 'gad') {
      bal <- get_gad_balance(dat_input, grouping_var)
    } else if (var_name == 'sps') {
      bal <- get_sps_balance(dat_input, grouping_var)
    } else if (var_name == 'pst_sub') {
      bal <- get_pst_sub_balance(dat_input, grouping_var)
    } else if (var_name == 'pri_sub') {
      bal <- get_pri_sub_balance(dat_input, grouping_var)
    } else if (var_name == 'sec_sub') {
      bal <- get_sec_sub_balance(dat_input, grouping_var)
    } else if (var_name == 'ter_sub') {
      bal <- get_ter_sub_balance(dat_input, grouping_var)
    } else if (var_name == 'dast') {
      bal <- get_dast_balance(dat_input, grouping_var)
    } else if (var_name == 'sipad') {
      bal <- get_sipad_balance(dat_input, grouping_var)
    } else if (var_name == 'cageaid') {
      bal <- get_cageaid_balance(dat_input, grouping_var)
    } else if (var_name == 'bscq') {
      bal <- get_bscq_balance(dat_input, grouping_var)
    } else if (var_name == 'crave') {
      bal <- get_crave_balance(dat_input, grouping_var)
    } else if (var_name == 'p30_per_sub') {
      bal <- get_p30_per_sub_balance(dat_input, grouping_var)
    } else if (var_name == 'p30') {
      bal <- get_p30_balance(dat_input, grouping_var)
    } else if (var_name == 'qds') {
      bal <- get_qds_balance(dat_input, grouping_var)
    } else if (var_name == 'taa') {
      bal <- get_taa_balance(dat_input, grouping_var)
    }
    write.csv(bal, file.path(save_dir, str_c(save_prefix, var_name, 'balance.csv', sep='_')), row.names=FALSE)
  }
}