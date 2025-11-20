# *Single Site, Anomaly Detection, Longitudinal*

*Single Site, Anomaly Detection, Longitudinal*

## Usage

``` r
qvd_ss_anom_la(process_output, value_type_filter = NULL)
```

## Arguments

- process_output:

  the output of the `qvd_process` function

- value_type_filter:

  a string or vector of strings to filter the graph to specific
  variables of interest

## Value

a line plot showing the proportion of values falling the user-selected
number of SD away from the mean at each time point; lower and upper
outliers are displayed as separate lines
