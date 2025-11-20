# Multi-Site Analysis for Independent Data Sources

The multi-site analyses included in this suite are intended to be
executed against data that are all stored in the same place. However,
there may be some instances where the data associated with each site is
stored in independent locations. This vignette outlines how the
multi-site analysis can be executed in these instances.

After following the instructions to reproduce the analysis, you will
also need to change the `output_function` column to tell the
`qvd_output` function which check you executed. Reference the table
below for the labels that are associated with each check:

| Check Type                                     | output_function |
|:-----------------------------------------------|:----------------|
| Multi Site, Exploratory, Cross-Sectional       | qvd_ms_exp_cs   |
| Multi Site, Exploratory, Longitudinal          | qvd_ms_exp_la   |
| Multi Site, Anomaly Detection, Cross-Sectional | qvd_ms_anom_cs  |
| Multi Site, Anomaly Detection, Longitudinal    | qvd_ms_anom_la  |

## Multi Site Exploratory Analysis

The process for the exploratory analysis is the same for both the
cross-sectional and longitudinal configurations.

First, execute either of the **Single Site, Exploratory** analyses,
configured appropriately for your study, against each data source.

``` r
library(quantvariabledistribution)

my_table <- qvd_process(cohort = my_cohort,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory = 'exploratory',
                        time = T / F,
                        ...)
```

Then, combine these results into a single table with the different sites
delineated in the `site` column.

``` r
my_final_results <- my_table1 %>% dplyr::union(my_table2) ... %>%
  dplyr::union(my_table_n) %>%
  dplyr::mutate(output_function = '{see table above}')
```

## Multi Site Anomaly Detection

For anomaly detection analysis, start by executing the same steps as the
exploratory analysis. Then, you will execute the relevant anomaly
detection algorithm against the resulting table. See below for the
different processes for cross-sectional and longitudinal analysis.

### Cross-Sectional

For a cross-sectional analysis, the `compute_kl_divergence` function in
this package should be executed against your results. This will compute
the Kullback-Leibler divergence of each site’s value distribution to the
overall value distribution.

The `kl_log_base` value can be selected by the user out of the following
options: `log`, `log2`, or `log10`. The standard default is `log2`.

``` r

df <- 
  quantvariabledistribution:::compute_kl_divergence(frequency_tbl = my_table,
                                                    kl_log_base = kl_log_base) %>%
  dplyr::mutate(output_function = '{see table above}')
```

### Longitudinal

For a longitudinal analysis, the `qvd_euclidean` function in this
package should be executed against your results. This will compute the
Euclidean distance from the site’s time series to the overall time
series.

The `euclidean_stat` value can be either `mean` or `median`, as desired
by the user.

``` r

df <- 
  quantvariabledistribution:::qvd_euclidean(fot_input_tbl = my_table,
                                            grp_vars = c('site', 'value_type'),
                                            euclidean_stat = euclidean_stat) %>%
  dplyr::mutate(output_function = '{see table above}')
```
