# *Multi Site, Exploratory, Cross-Sectional*

*Multi Site, Exploratory, Cross-Sectional*

## Usage

``` r
qvd_ms_exp_cs(
  process_output,
  value_type_filter = NULL,
  frequency_min = 5,
  display_outliers = FALSE,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  the output of the `qvd_process` function

- value_type_filter:

  a string or vector of strings to filter the graph to specific
  variables of interest

- frequency_min:

  an integer to establish a minimum amount of times a value should occur
  to be included in the output; aimed at trimming infrequently occurring
  outliers for a cleaner plot; defaults to 5

- display_outliers:

  for boxplot output, a boolean to indicate whether outliers should be
  displayed; defaults to FALSE

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a plot with boxplots for the distributions at each site, stratified by
variable
