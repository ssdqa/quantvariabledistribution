
#' QVD Sample Value File (OMOP)
#'
#' A sample version of the file structure expected for the `qvd_value_file`
#' when the `omop` CDM is selected. The user should recreate this file and
#' include their own value definitions.
#'
#' Files referenced in the `codeset_name` column should be kept in the `file_subdirectory`
#' identified when `initialize_dq_session` is called.
#'
#' @format ## qvd_value_file_omop
#' A data frame with 7 columns
#' \describe{
#'   \item{value_name}{*string* | a string label for the value variable}
#'   \item{domain_tbl}{*character* | CDM table where the value data is found}
#'   \item{value_field}{*character* | the name of the field with the quantitative variable OR the name of the person identifier column for patient count checks}
#'   \item{date_field}{*character* | a date field in the `domain_tbl` that should be used for over time analyses}
#'   \item{concept_field}{*character* | concept_id field with codes from the associated codeset (only needed when codeset is provided)}
#'   \item{codeset_name}{*character* | the name of the codeset file; DO NOT include the file extension; optional}
#'   \item{filter_logic}{*character* | a string indicating filter logic that should be applied to achieve the desired variable; optional}
#' }
#'
"qvd_value_file_omop"

#' QVD Sample Value File (PCORnet)
#'
#' A sample version of the file structure expected for the `qvd_value_file`
#' when the `pcornet` CDM is selected. The user should recreate this file and
#' include their own value definitions.
#'
#' Files referenced in the `codeset_name` column should be kept in the `file_subdirectory`
#' identified when `initialize_dq_session` is called.
#'
#' @format ## qvd_value_file_omop
#' A data frame with 8 columns
#' \describe{
#'   \item{value_name}{*string* | a string label for the value variable}
#'   \item{domain_tbl}{*character* | CDM table where the value data is found}
#'   \item{value_field}{*character* | the name of the field with the quantitative variable OR the name of the person identifier column for patient count checks}
#'   \item{date_field}{*character* | a date field in the `domain_tbl` that should be used for over time analyses}
#'   \item{concept_field}{*character* | concept_id field with codes from the associated codeset (only needed when codeset is provided)}
#'   \item{codeset_name}{*character* | the name of the codeset file; DO NOT include the file extension; optional}
#'   \item{vocabulary_field}{*character* | the name of the column where the vocabulary type associated with the concepts are stored (typically dx_type or px_type, where needed)}
#'   \item{filter_logic}{*character* | a string indicating filter logic that should be applied to achieve the desired variable; optional}
#' }
#'
"qvd_value_file_pcornet"
