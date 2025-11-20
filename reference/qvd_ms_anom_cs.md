# *Multi Site, Anomaly Detection, Cross-Sectional*

*Multi Site, Anomaly Detection, Cross-Sectional*

## Usage

``` r
qvd_ms_anom_cs(
  process_output,
  value_type_filter = NULL,
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

a radial lolipop graph displaying the KL divergence value for each site,
indicating the divergence from the all-site frequency distritbution
