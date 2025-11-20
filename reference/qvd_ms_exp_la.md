# *Multi Site, Exploratory, Longitudinal*

*Multi Site, Exploratory, Longitudinal*

## Usage

``` r
qvd_ms_exp_la(
  process_output,
  value_type_filter = NULL,
  summary_stat = "mean",
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

- summary_stat:

  a string indicating the summary statistic that should be displayed on
  the plot; required for exploratory, longitudinal results; defaults to
  `mean`, but `median`, `q1`, `q3`, or `sd` are also accepted

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a line plot displaying the summary_stat of interest across the time
period for each site
