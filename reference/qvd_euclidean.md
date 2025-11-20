# Euclidean distance for QVD

Euclidean distance for QVD

## Usage

``` r
qvd_euclidean(fot_input_tbl, grp_vars, euclidean_stat = "mean")
```

## Arguments

- fot_input_tbl:

  the output of `compute_fot` when `compute_quant_val_dist` is used as
  the check_func input

- grp_vars:

  list of columns that should be used to group the table

- euclidean_stat:

  the statistic that should be computed across all sites to be used as a
  comparison to the same site-specific value (i.e. the distance from the
  site mean to the all site mean) defaults to `mean` but `median` is
  also accepted

## Value

a dataframe with the original stat value, the newly computed all site
stat value, and the euclidean distance between the two distributions
stratified by each time point and the additional grp_vars provided
