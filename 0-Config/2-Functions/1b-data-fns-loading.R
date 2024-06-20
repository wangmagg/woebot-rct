### Functions for loading data

#' Extract email from metadata in screening file
#' 
#' @param metadata String containing metadata
extract_email <- function(metadata) {
  rhs_email_field <- strsplit(metadata, "email")[[1]][2]
  email_field <- strsplit(rhs_email_field, ",")[[1]][1]
  email <- str_replace_all(email_field, "(['\"':])", "")
  email_lower <- tolower(email)
}

#' Merge app engagement data with survey data
#' 
#' @param base_dat Baseline survey data dataframe
#' @param app_engagement_path Path to file with app engagement data
#' @param rcc_pdf_path Path to file with PDF engagement data
add_engage_dat <- function(base_dat, 
                           app_engage_path,
                           rcc_pdf_path) {
  
  app_engage_dat <- read_excel(app_engage_path, sheet='sud2 raw data') |>
    mutate(emailaddress = tolower(emailaddress))
  
  pdf_dat <- read_csv(rcc_pdf_path, show_col_types=FALSE) |>
    group_by(participant_id) |>
    summarize(w1_complete = sum(`Group 2 Info_complete` == 2, na.rm=TRUE),
              w2_complete = sum(`Week 2 PDF_complete` == 2, na.rm=TRUE),
              w3_complete = sum(`Week 3 PDF_complete` == 2, na.rm=TRUE),
              w4_complete = sum(`Week 4 PDF_complete` == 2, na.rm=TRUE),
              w5_complete = sum(`Week 5 PDF_complete` == 2, na.rm=TRUE),
              w6_complete = sum(`Week 6 PDF_complete` == 2, na.rm=TRUE),
              w7_complete = sum(`Week 7 PDF_complete` == 2, na.rm=TRUE),
              w8_complete = sum(`Week 8 PDF_complete` == 2, na.rm=TRUE), .groups='drop')
  
  base_dat <- base_dat |>  
    rowwise() |>
    mutate(emailaddress = extract_email(redcap_record_metadata)) |>
    left_join(app_engage_dat, by=c("emailaddress")) |>
    left_join(pdf_dat, by=c("participant_id"))
}

#' Load survey and engagement data
#' 
#' @param rcc_rand_path Path to randomization information
#' @param rcc_baseline_path Path to baseline survey data
#' @param rcc_withdraw_path Path to withdrawal information
#' @param rcc_mid_path Path to midpoint (4wk) survey data
#' @param rcc_eot_path Path to EOT (8wk) survey data
#' @param rcc_followup_path Path to followup (12wk) survey data
#' @param app_engagement_path Path to app engagement data
#' @param rcc_pdf_path Path to PDF engagement data
#' @param add_engage Boolean flag for whether or not to merge engagement data
#' @returns Combined survey and engagement data
load_survey_dat <- function(rcc_rand_path, 
                            rcc_baseline_path, 
                            rcc_withdraw_path, 
                            rcc_mid_path, 
                            rcc_eot_path, 
                            rcc_followup_path,
                            app_engage_path,
                            rcc_pdf_path,
                            add_engage=TRUE) {
  # Randomization data
  rand_dat <- read_csv(rcc_rand_path, show_col_types=FALSE) |>
    select(-redcap_system_data_format_version, 
           -redcap_study, 
           -redcap_event_name, 
           -redcap_study_metadata, 
           -redcap_record_metadata)
  
  # Baseline survey 
  baseline_dat <- read_csv(rcc_baseline_path, show_col_types=FALSE) 
  if (add_engage) {
    baseline_dat <- baseline_dat |> add_engage_dat(app_engage_path, rcc_pdf_path)
  } 
  baseline_dat <- baseline_dat |>
    select(-redcap_system_data_format_version, 
           -redcap_study, 
           -redcap_event_name, 
           -redcap_study_metadata,
           -redcap_record_metadata)
  
  # Removals and withdrawals
  withdraw_dat <- read_csv(rcc_withdraw_path, show_col_types=FALSE) |>
    select(-redcap_system_data_format_version, 
           -redcap_study, 
           -redcap_event_name, 
           -redcap_study_metadata,
           -redcap_record_metadata)
  
  # 4-week (midpoint) survey
  mid_dat <- read_csv(rcc_mid_path, show_col_types=FALSE) |>
    select(-redcap_system_data_format_version, 
           -redcap_study, 
           -redcap_event_name, 
           -redcap_study_metadata, 
           -redcap_record_metadata)
  
  # 8-week (EOT) survey
  eot_dat <- read_csv(rcc_eot_path, show_col_types=FALSE) |>
    select(-redcap_system_data_format_version, 
           -redcap_study, 
           -redcap_event_name, 
           -redcap_study_metadata, 
           -redcap_record_metadata)
  
  # 12-week (follow-up) survey
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
  dat <- dat |> filter(participant_id != 1)
}

#' Load Qualtrics screening data
#' 
#' @param q_screen_path Path to Qualtrics screening data file
#' @param id_rm_wd_path Path to IDs for removals and withdrawals,
#' used to map the ID's in the Qualtrics screening file to the ID's
#' in the survey files
#' @returns Dataframe with Qualtrics screening data
load_screen_q_dat <- function(q_screen_path, id_rm_wd_path) {
  q_id_rm_wd_dat <- read_excel(id_rm_wd_path, sheet='Subject IDs Q Screening')
  screen_q_dat <- read_csv(q_screen_path, show_col_types=F) |>
    slice(-c(1, 2)) |>
    left_join(q_id_rm_wd_dat, by=c('ResponseId' = 'Screening ID')) |>
    rename(screening_id = ResponseId,
           participant_id = `Subject ID`) |>
    filter(screening_id != 'Test') 
}

#' Load REDCap screening data
#' 
#' @param q_screen_path Path to REDCap screening data file
#' @param id_rm_wd_path Path to IDs for removals and withdrawals,
#' used to map the ID's in the REDCap screening file to the ID's
#' in the survey files
#' @returns Dataframe with REDCap screening data
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

#' Extract and reformat demographic information from Qualtrics screening data
#' 
#' @param q_screen_dat Dataframe of Qualtrics screening data
#' @returns Dataframe containing the demographic columns needed for analyses
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

#' Extract and reformat demographic information from REDCap screening data
#' 
#' @param q_screen_dat Dataframe of REDCap screening data
#' @returns Dataframe containing the demographic columns needed for analyses
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

#' Load all (survey + screening) data
#' 
#' @param rcc_rand_path Path to randomization information
#' @param rcc_baseline_path Path to baseline survey data
#' @param rcc_withdraw_path Path to withdrawal information
#' @param rcc_mid_path Path to midpoint (4wk) survey data
#' @param rcc_eot_path Path to EOT (8wk) survey data
#' @param rcc_followup_path Path to followup (12wk) survey data
#' @param id_rm_wd_path Path to IDs for removals and withdrawals,
#' used to map the ID's in the screening files to the ID's
#' in the survey files
#' @param screen_type String 'q' for Qualtrics screen, 'rcc' for REDCap screen
#' @param screen_path Path to screening file
#' @param app_engagement_path Path to app engagement data
#' @param rcc_pdf_path Path to PDF engagement data
#' @param add_engage Boolean flag for whether or not to merge engagement data
load_data_raw <- function(rcc_rand_path, 
                          rcc_baseline_path, 
                          rcc_withdraw_path, 
                          rcc_mid_path, 
                          rcc_eot_path, 
                          rcc_followup_path,
                          id_rm_wd_path,
                          screen_type,
                          screen_path,
                          app_engage_path,
                          rcc_pdf_path,
                          add_engage=TRUE) {
  
  # Load survey data, then merge with screening data
  dat_survey <- load_survey_dat(rcc_rand_path, 
                                rcc_baseline_path, 
                                rcc_withdraw_path, 
                                rcc_mid_path, 
                                rcc_eot_path, 
                                rcc_followup_path,
                                app_engage_path,
                                rcc_pdf_path,
                                add_engage)
  
  if (screen_type == 'q') {
    dat_screen <- load_screen_q_dat(screen_path, id_rm_wd_path)
  } else if (screen_type == 'rcc') {
    dat_screen <- load_screen_rcc_dat(screen_path, id_rm_wd_path)
  }
  
  dat_survey <- dat_survey |> select(-starts_with('sq'), -starts_with('SQ'))
  dat <- dat_screen |> inner_join(dat_survey, by=c('participant_id'))
}
  
#' Load all (survey + screening) data and keep only variables used in analysis,
#' renaming the variables so that they have more interpretable names
#' 
#' @param rcc_rand_path Path to randomization information
#' @param rcc_baseline_path Path to baseline survey data
#' @param rcc_withdraw_path Path to withdrawal information
#' @param rcc_mid_path Path to midpoint (4wk) survey data
#' @param rcc_eot_path Path to EOT (8wk) survey data
#' @param rcc_followup_path Path to followup (12wk) survey data
#' @param id_rm_wd_path Path to IDs for removals and withdrawals,
#' used to map the ID's in the screening files to the ID's
#' in the survey files
#' @param screen_type String 'q' for Qualtrics screen, 'rcc' for REDCap screen
#' @param screen_path Path to screening file
#' @param app_engagement_path Path to app engagement data
#' @param rcc_pdf_path Path to PDF engagement data
#' @param var_name_mapping_path Path to file containing variable name mappings
#' @param add_engage Boolean flag for whether or not to merge engagement data
load_data_analysis <- function(rcc_rand_path, 
                               rcc_baseline_path, 
                               rcc_withdraw_path, 
                               rcc_mid_path, 
                               rcc_eot_path, 
                               rcc_followup_path,
                               id_rm_wd_path,
                               q_screen_path, 
                               rcc_screen_path,
                               app_engage_path,
                               rcc_pdf_path,
                               var_name_mapping_path,
                               add_engage=TRUE) {
  
  # Load survey data, then merge with screening data
  dat_survey <- load_survey_dat(rcc_rand_path, 
                                rcc_baseline_path, 
                                rcc_withdraw_path, 
                                rcc_mid_path, 
                                rcc_eot_path, 
                                rcc_followup_path,
                                app_engage_path,
                                rcc_pdf_path,
                                add_engage)

  dat_screen_q <- load_screen_q_dat(q_screen_path, id_rm_wd_path) |>
    extract_cols_q_screen_dat()
  dat_screen_rcc <- load_screen_rcc_dat(rcc_screen_path, id_rm_wd_path) |>
    extract_cols_rcc_screen_dat()
  
  dat_survey_screen_q <- dat_screen_q |> 
    inner_join(dat_survey, by=c('participant_id'))
  dat_survey_screen_rcc <- dat_screen_rcc |> 
    inner_join(dat_survey |> select(-sq36), by=c('participant_id'))
  
  dat <- bind_rows(dat_survey_screen_q, dat_survey_screen_rcc)
  
  var_name_mapping <- data.frame()
  for (sheet_name in c('baseline', 'mid', 'eot', 'followup')) {
    var_name_mapping_sheet <- read_xlsx(var_name_mapping_path, sheet=sheet_name)
    var_name_mapping <- bind_rows(var_name_mapping, var_name_mapping_sheet)
  }
  setnames(dat, old=var_name_mapping$q_num, new=var_name_mapping$var_name)
  
  id_rand_screen_vars <- c("participant_id",
                           "emailaddress",
                           "screen",
                           "cageaid",
                           "Randomization_complete",
                           "group",
                           "withdraw",
                           "age",
                           "sex",
                           "eth",
                           "race_1",
                           "race_2",
                           "race_3",
                           "race_4",
                           "race_5",
                           "race_6",
                           "race_99",
                           "race_mult")
  pdf_engagement_cols <- str_c(str_c("w", 1:8), "complete", sep="_")
  app_engagement_col_prefixes <- c("days_active", 
                                   "user_messages", 
                                   "stories", 
                                   "tools", 
                                   "moods")
  
  dat |> select(all_of(id_rand_screen_vars),
                all_of(var_name_mapping$var_name), 
                all_of(pdf_engagement_cols),
                starts_with(app_engagement_col_prefixes))
}