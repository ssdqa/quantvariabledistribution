# *Single Site, Exploratory, Longitudinal*

*Single Site, Exploratory, Longitudinal*

## Usage

``` r
qvd_ss_exp_la(process_output, value_type_filter = NULL, summary_stat = "mean")
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

## Value

a line plot displaying the summary_stat of choice across the time span
