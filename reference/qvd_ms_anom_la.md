# *Multi Site, Anomaly Detection, Longitudinal*

*Multi Site, Anomaly Detection, Longitudinal*

## Usage

``` r
qvd_ms_anom_la(
  process_output,
  value_type_filter,
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

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

three graphs:

1.  line graph that shows the smoothed euclidean_stat of a variable
    across time computation with the Euclidean distance associated with
    each line

2.  line graph that shows the raw euclidean_stat of a variable across
    time computation with the Euclidean distance associated with each
    line

3.  a bar graph with the Euclidean distance value for each site, with
    the average euclidean_stat as the fill

THIS GRAPH SHOWS ONLY ONE VALUE TYPE AT A TIME!
