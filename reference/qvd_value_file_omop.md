# QVD Sample Value File (OMOP)

A sample version of the file structure expected for the `qvd_value_file`
when the `omop` CDM is selected. The user should recreate this file and
include their own value definitions.

## Usage

``` r
qvd_value_file_omop
```

## Format

### qvd_value_file_omop

A data frame with 7 columns

- value_name:

  *string* \| a string label for the value variable

- domain_tbl:

  *character* \| CDM table where the value data is found

- value_field:

  *character* \| the name of the field with the quantitative variable OR
  the name of the person identifier column for patient count checks

- date_field:

  *character* \| a date field in the `domain_tbl` that should be used
  for over time analyses

- concept_field:

  *character* \| concept_id field with codes from the associated codeset
  (only needed when codeset is provided)

- codeset_name:

  *character* \| the name of the codeset file; DO NOT include the file
  extension; optional

- filter_logic:

  *character* \| a string indicating filter logic that should be applied
  to achieve the desired variable; optional

## Details

Files referenced in the `codeset_name` column should be kept in the
`file_subdirectory` identified when `initialize_dq_session` is called.
