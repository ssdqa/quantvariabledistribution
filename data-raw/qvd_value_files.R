## code to prepare `qvd_value_file_omop` dataset goes here

qvd_value_file_omop <- tibble(value_name = c('hydroxyurea dosage', 'inpatient visits'),
                              domain_tbl = c('drug_exposure', 'visit_occurrence'),
                              value_field = c('effective_drug_dose', 'person_id'),
                              date_field = c('drug_exposure_start_date', 'visit_start_date'),
                              concept_field = c('drug_concept_id', NA),
                              codeset_name = c('rx_hydroxyurea', NA),
                              filter_logic = c(NA, 'visit_concept_id == 9201'))

usethis::use_data(qvd_value_file_omop, overwrite = TRUE)

## pcornet
qvd_value_file_pcornet <- tibble(value_name = c('hydroxyurea dosage', 'inpatient covid diagnoss'),
                                 domain_tbl = c('prescribing', 'diagnosis'),
                                 value_field = c('rx_dose_ordered', 'patid'),
                                 date_field = c('rx_order_date', 'admit_date'),
                                 concept_field = c('rxnorm_cui', 'dx'),
                                 codeset_name = c('rx_hydroxyurea', 'dx_covid'),
                                 vocabulary_field = c(NA, 'dx_type'),
                                 filter_logic = c(NA, 'enc_type == IP'))

usethis::use_data(qvd_value_file_pcornet, overwrite = TRUE)
