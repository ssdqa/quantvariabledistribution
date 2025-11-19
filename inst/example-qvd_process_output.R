
#' Source setup file
source(system.file('setup.R', package = 'quantvariabledistribution'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'qvd_process_test',
                      working_directory = my_directory,
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = my_file_folder,
                      cdm_schema = NA)

#' Build mock study cohort
cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  dplyr::mutate(start_date = -10000, # RSQLite does not store date objects,
                                      # hence the numerics
                end_date = 30000,
                site = ifelse(person_id %in% 1:6, 'synth1', 'synth2'))

#' Create `qvd_value_file` input
qvd_input <- dplyr::tibble('value_name' = c('ibuprofen days supply',
                                            'outpatient visits'),
                           'domain_tbl' = c("drug_exposure",
                                            'visit_occurrence'),
                           'value_field' = c('days_supply',
                                             'person_id'),
                           'date_field' = c('drug_exposure_start_date',
                                            'visit_start_date'),
                           'concept_field' = c('drug_concept_id',
                                               NA),
                           'codeset_name' = c('rx_ibuprofen',
                                              NA),
                           'filter_logic' = c(NA,
                                              'visit_concept_id == 9202'))

#' Execute `qvd_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
qvd_process_example <- qvd_process(cohort = cohort,
                                   multi_or_single_site = 'single',
                                   anomaly_or_exploratory = 'exploratory',
                                   time = FALSE,
                                   omop_or_pcornet = 'omop',
                                   qvd_value_file = qvd_input) %>%
  suppressMessages()

qvd_process_example

#' Execute qvd_output` function
qvd_output_example <- qvd_output(process_output = qvd_process_example)

qvd_output_example

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(qvd_output_example)
