# Compute Kullback-Liebler divergence

Compute Kullback-Liebler divergence

## Usage

``` r
compute_kl_divergence(frequency_tbl, kl_log_base = "log2")
```

## Arguments

- frequency_tbl:

  the output of `compute_quant_val_dist`

- kl_log_base:

  a string indicating the log base to be used in the computations;
  defaults to `log2`, `log` and `log10` are also acceptable

## Value

a dataframe with the kullback-liebler divergence value for the site's
distribution compared to the overall, all-site distribution
