# *Single Site, Exploratory, Cross-Sectional*

*Single Site, Exploratory, Cross-Sectional*

## Usage

``` r
qvd_ss_exp_cs(
  process_output,
  display_outliers = FALSE,
  frequency_min = 5,
  value_type_filter = NULL
)
```

## Arguments

- process_output:

  the output of the `qvd_process` function

- display_outliers:

  for boxplot output, a boolean to indicate whether outliers should be
  displayed; defaults to FALSE

- frequency_min:

  an integer to establish a minimum amount of times a value should occur
  to be included in the output; aimed at trimming infrequently occurring
  outliers for a cleaner plot; defaults to 5

- value_type_filter:

  a string or vector of strings to filter the graph to specific
  variables of interest

## Value

a halfeye density plot showing the density of the distribution for each
variable with a boxplot underneath showing the summary of the
distribution
