load_survey_dat <- function(rcc_rand_path, 
                            rcc_baseline_path, 
                            rcc_withdraw_path, 
                            rcc_mid_path, 
                            rcc_eot_path, 
                            rcc_followup_path) {
  # Read in data
  rand_dat <- read_csv(rcc_rand_path, show_col_types=FALSE) |>
    select(-redcap_system_data_format_version, 
           -redcap_study, 
           -redcap_event_name, 
           -redcap_study_metadata, 
           -redcap_record_metadata)
  
  baseline_dat <- read_csv(rcc_baseline_path, show_col_types=FALSE) |>
    select(-redcap_system_data_format_version, 
           -redcap_study, 
           -redcap_event_name, 
           -redcap_study_metadata, 
           -redcap_record_metadata)
  
  withdraw_dat <- read_csv(rcc_withdraw_path, show_col_types=FALSE) |>
    select(-redcap_system_data_format_version, 
           -redcap_study, 
           -redcap_event_name, 
           -redcap_study_metadata, 
           -redcap_record_metadata)
  
  mid_dat <- read_csv(rcc_mid_path, show_col_types=FALSE) |>
    select(-redcap_system_data_format_version, 
           -redcap_study, 
           -redcap_event_name, 
           -redcap_study_metadata, 
           -redcap_record_metadata)
  
  eot_dat <- read_csv(rcc_eot_path, show_col_types=FALSE) |>
    select(-redcap_system_data_format_version, 
           -redcap_study, 
           -redcap_event_name, 
           -redcap_study_metadata, 
           -redcap_record_metadata)
  
  followup_dat <- read_csv(rcc_followup_path, show_col_types=FALSE) |>
    select(-redcap_system_data_format_version, 
           -redcap_study, 
           -redcap_event_name, 
           -redcap_study_metadata, 
           -redcap_record_metadata)
  
  rcc_id_rm_wd_dat <- read_excel(id_rm_wd_path, sheet='Subject IDs RCC Screening') |>
    mutate(screen = 1)
  q_id_rm_wd_dat <- read_excel(id_rm_wd_path, sheet='Subject IDs Q Screening') |>
    mutate(screen = 2)
  id_rm_wd_dat <- bind_rows(q_id_rm_wd_dat, rcc_id_rm_wd_dat)
  
  # Join dataframes
  dat <- rand_dat |>
    left_join(baseline_dat, by=c('participant_id', 'redcap_subject_screening_number')) |>
    left_join(mid_dat, by=c('participant_id', 'redcap_subject_screening_number')) |>
    left_join(eot_dat, by=c('participant_id', 'redcap_subject_screening_number')) |>
    left_join(followup_dat, by=c('participant_id', 'redcap_subject_screening_number')) |>
    left_join(withdraw_dat, by=c('participant_id', 'redcap_subject_screening_number')) 
  
  # Remove test participant
  dat <- dat |>
    filter(participant_id != 1)
}

load_screen_q_dat <- function(q_screen_path, id_rm_wd_path) {
  q_id_rm_wd_dat <- read_excel(id_rm_wd_path, sheet='Subject IDs Q Screening')
  screen_q_dat <- read_csv(q_screen_path, show_col_types=F) |>
    slice(-c(1, 2)) |>
    left_join(q_id_rm_wd_dat, by=c('ResponseId' = 'Screening ID')) |>
    rename(screening_id = ResponseId,
           participant_id = `Subject ID`) |>
    filter(screening_id != 'Test') 
}

load_screen_rcc_dat <- function(rcc_screen_path, id_rm_wd_path) {
  rcc_id_rm_wd_dat <- read_excel(id_rm_wd_path, sheet='Subject IDs RCC Screening')
  screen_rcc_dat <- read_csv(rcc_screen_path, show_col_types=F, col_types=list(sq37_other=col_character())) |>
    mutate_at(c('participant_id'), as.character) |>
    left_join(rcc_id_rm_wd_dat, by=c('participant_id' = 'Screening ID')) |>
    rename(screening_id = participant_id,
           participant_id = `Subject ID`) |>
    filter(screening_id != 'Test') |>
    select(-redcap_subject_screening_number)
}

extract_cols_q_screen_dat <- function(q_screen_dat) {
  q_screen_dat <- q_screen_dat |>
    mutate(screen = 2) |>
    mutate(SQ5_temp_vec = strsplit(SQ5, ',')) |>
    mutate(age = as.numeric(SQ2),
           sex = as.numeric(SQ6),
           eth = as.numeric(SQ4)) |>
    rowwise() |>
    mutate(
      race_1 = case_when(is.element(1, unlist(SQ5_temp_vec))~1, .default=0),
      race_2 = case_when(is.element(2, unlist(SQ5_temp_vec))~1, .default=0),
      race_3 = case_when(is.element(3, unlist(SQ5_temp_vec))~1, .default=0),
      race_4 = case_when(is.element(4, unlist(SQ5_temp_vec))~1, .default=0),
      race_5 = case_when(is.element(5, unlist(SQ5_temp_vec))~1, .default=0),
      race_6 = case_when(is.element(6, unlist(SQ5_temp_vec))~1, .default=0),
      race_99 = case_when(is.element(99, unlist(SQ5_temp_vec))~1, .default=0),
      race_mult = (race_1 + race_2 + race_3 + race_4 + race_5 + race_6 > 1)) |>
    # recode the one response of Other as "multiracial"
    mutate(race_mult = case_when(participant_id == 170 ~ 1, .default=race_mult),
           race_6 = case_when(participant_id == 170 ~ 0, .default=race_6)) |>
    ungroup() |>
    mutate(race_1 = case_when(race_mult == 1 ~ 0, .default=race_1),
           race_2 = case_when(race_mult == 1 ~ 0, .default=race_2),
           race_3 = case_when(race_mult == 1 ~ 0, .default=race_3),
           race_4 = case_when(race_mult == 1 ~ 0, .default=race_4),
           race_5 = case_when(race_mult == 1 ~ 0, .default=race_5),
           race_6 = case_when(race_mult == 1 ~ 0, .default=race_6)) |>
    select(participant_id, screen, age, sex, eth, race_1, race_2, race_3, race_4, race_5, race_6, race_99, race_mult)
}

extract_cols_rcc_screen_dat <- function(rcc_screen_dat) {
  rcc_screen_dat <- rcc_screen_dat |>
    mutate(screen = 1) |>
    rename(age=sq2,
           sex=sq6,
           eth=sq4,
           race_1=sq5___1, 
           race_2=sq5___2, 
           race_3=sq5___3, 
           race_4=sq5___4, 
           race_5=sq5___5, 
           race_6=sq5___6,
           race_99=sq5___99) |>
    mutate(race_mult = (race_1 + race_2 + race_3 + race_4 + race_5 + race_6 > 1)) |>
    mutate(race_1 = case_when(race_mult == 1 ~ 0, .default=race_1),
           race_2 = case_when(race_mult == 1 ~ 0, .default=race_2),
           race_3 = case_when(race_mult == 1 ~ 0, .default=race_3),
           race_4 = case_when(race_mult == 1 ~ 0, .default=race_4),
           race_5 = case_when(race_mult == 1 ~ 0, .default=race_5),
           race_6 = case_when(race_mult == 1 ~ 0, .default=race_6)) |>
    select(participant_id, screen, age, sex, eth, race_1, race_2, race_3, race_4, race_5, race_6, race_99, race_mult, sq36)
}

load_data_raw <- function(rcc_rand_path, 
                          rcc_baseline_path, 
                          rcc_withdraw_path, 
                          rcc_mid_path, 
                          rcc_eot_path, 
                          rcc_followup_path,
                          id_rm_wd_path,
                          screen_type,
                          screen_path) {
  
  dat_survey <- load_survey_dat(rcc_rand_path, 
                                rcc_baseline_path, 
                                rcc_withdraw_path, 
                                rcc_mid_path, 
                                rcc_eot_path, 
                                rcc_followup_path)
  
  if (screen_type == 'q') {
    dat_screen <- load_screen_q_dat(screen_path, id_rm_wd_path)
  } else if (screen_type == 'rcc') {
    dat_screen <- load_screen_rcc_dat(screen_path, id_rm_wd_path)
  }
  
  dat_survey <- dat_survey |> select(-starts_with('sq'), -starts_with('SQ'))
  dat <- dat_survey |> inner_join(dat_screen, by=c('participant_id'))
}
  
load_data_analysis <- function(rcc_rand_path, 
                               rcc_baseline_path, 
                               rcc_withdraw_path, 
                               rcc_mid_path, 
                               rcc_eot_path, 
                               rcc_followup_path,
                               id_rm_wd_path,
                               q_screen_path, 
                               rcc_screen_path,
                               var_name_mapping_path) {
  
  dat_survey <- load_survey_dat(rcc_rand_path, 
                                rcc_baseline_path, 
                                rcc_withdraw_path, 
                                rcc_mid_path, 
                                rcc_eot_path, 
                                rcc_followup_path)
  
  dat_screen_q <- load_screen_q_dat(q_screen_path, id_rm_wd_path) |>
    extract_cols_q_screen_dat()
  dat_screen_rcc <- load_screen_rcc_dat(rcc_screen_path, id_rm_wd_path) |>
    extract_cols_rcc_screen_dat()
  
  dat_survey_screen_q <- dat_survey |> inner_join(dat_screen_q, by=c('participant_id'))
  dat_survey_screen_rcc <- dat_survey |> 
    select(-sq36) |>
    inner_join(dat_screen_rcc, by=c('participant_id'))
  
  dat <- bind_rows(dat_survey_screen_q, dat_survey_screen_rcc)
  
  var_name_mapping <- data.frame()
  for (sheet_name in c('baseline', 'mid', 'eot', 'followup')) {
    var_name_mapping_sheet <- read_xlsx(var_name_mapping_path, sheet=sheet_name)
    var_name_mapping <- bind_rows(var_name_mapping, var_name_mapping_sheet)
  }
  setnames(dat, old=var_name_mapping$q_num, new=var_name_mapping$var_name)
  
  var_name_no_mapping <- read_xlsx(var_name_mapping_path, sheet="no_mapping")
  
  dat |> select(all_of(var_name_mapping$var_name), all_of(var_name_no_mapping$var_name))
  
}