# Compute quantitative variable distribution

Compute quantitative variable distribution

## Usage

``` r
compute_quant_val_dist(
  qvd_value_file,
  cohort = results_tbl("scd_cohort"),
  grouped_list = "site",
  omop_or_pcornet = "omop",
  time = FALSE
)
```

## Arguments

- qvd_value_file:

  *tabular input* \| dataframe or CSV file with information about each
  of the variables that should be examined in the function. contains the
  following columns:

  - `value_name` \| *string* \|

  - `domain_tbl` \| *character* \| CDM table where the value data is
    found

  - `value_field` \| *character* \| the name of the field with the
    quantitative variable OR the name of the person identifier column
    for patient count checks

  - `date_field` \| *character* \| a date field in the `domain_tbl` that
    should be used for over time analyses

  - `concept_field` \| *character* \| concept_id field with codes from
    the associated codeset (only needed when codeset is provided)

  - `codeset_name` \| *character* \| the name of the codeset file; DO
    NOT include the file extension; optional

  - `filter_logic` \| *character* \| a string indicating filter logic
    that should be applied to achieve the desired variable; optional

- cohort:

  *tabular input* \| A dataframe with the cohort of patients for your
  study. Should include the columns:

  - `person_id` / `patid` \| *integer* / *character*

  - `start_date` \| *date*

  - `end_date` \| *date*

  - `site` \| *character*

- grouped_list:

  *vector* \| the list of columns that should be used to group the
  variable table

- omop_or_pcornet:

  *string* \| Option to run the function using the OMOP or PCORnet CDM
  as the default CDM accepts `omop` or `pcornet`

- time:

  *boolean* \| a logical that tells the function whether you would like
  to look at the output over time

## Value

a dataframe with the frequency distribution for each value associated
with the variable of interest and summary statistics (mean, median, q1,
q3, sd) describing the distribution
