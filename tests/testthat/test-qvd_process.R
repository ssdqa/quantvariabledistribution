
qvd_input <- tibble(value_name = c('antihtn supply', 'outpatient visits'),
                    domain_tbl = c('drug_exposure', 'visit_occurrence'),
                    value_field = c('days_supply', 'person_id'),
                    date_field = c('drug_exposure_start_date', 'visit_start_date'),
                    concept_field = c('drug_concept_id', NA),
                    codeset_name = c('rx_antihypertensive', NA),
                    filter_logic = c(NA, 'visit_concept_id == 9202'))


test_that('only single & multi are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(qvd_process(cohort = cht,
                           multi_or_single_site = 'test',
                           anomaly_or_exploratory = 'exploratory',
                           omop_or_pcornet = 'omop'))
})

test_that('only anomaly & exploratory are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(qvd_process(cohort = cht,
                           multi_or_single_site = 'single',
                           anomaly_or_exploratory = 'test',
                           omop_or_pcornet = 'omop'))
})

test_that('only omop & pcornet are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(qvd_process(cohort = cht,
                           multi_or_single_site = 'single',
                           anomaly_or_exploratory = 'exploratory',
                           omop_or_pcornet = 'test'))
})


test_that('qvd exp cs -- omop', {


  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'qvd_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  expect_no_error(qvd_process(cohort = cohort,
                              qvd_value_file = qvd_input,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'exploratory',
                              omop_or_pcornet = 'omop'))

})

test_that('qvd exp la -- omop', {


  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'qvd_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  expect_no_error(qvd_process(cohort = cohort,
                              qvd_value_file = qvd_input,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'exploratory',
                              omop_or_pcornet = 'omop',
                              time = TRUE))

})


test_that('qvd ss anom cs -- omop', {


  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'qvd_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  expect_no_error(qvd_process(cohort = cohort,
                              qvd_value_file = qvd_input,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'anomaly',
                              omop_or_pcornet = 'omop'))

})

test_that('qvd ms anom cs -- omop', {


  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'qvd_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  expect_no_error(qvd_process(cohort = cohort,
                              qvd_value_file = qvd_input,
                              multi_or_single_site = 'multi',
                              anomaly_or_exploratory = 'anomaly',
                              omop_or_pcornet = 'omop'))

})

test_that('qvd ss anom la -- omop', {


  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'qvd_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  expect_no_error(qvd_process(cohort = cohort,
                              qvd_value_file = qvd_input,
                              multi_or_single_site = 'single',
                              anomaly_or_exploratory = 'anomaly',
                              omop_or_pcornet = 'omop',
                              time = TRUE))

})

test_that('qvd ms anom la -- omop', {


  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'qvd_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  expect_error(qvd_process(cohort = cohort,
                              qvd_value_file = qvd_input,
                              multi_or_single_site = 'multi',
                              anomaly_or_exploratory = 'anomaly',
                              omop_or_pcornet = 'omop',
                              time = TRUE))

})
