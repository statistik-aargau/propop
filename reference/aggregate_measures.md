# Aggregate evaluation measures

Returns descriptive summary statistics of model accuracy and bias
measures across demographic groups and years. The returned statistics
are particularly useful for comparing the model performance for
different groups or different models.

## Usage

``` r
aggregate_measures(data, weight_groups = NULL)
```

## Arguments

- data:

  data frame created with function `compute_measures`.

- weight_groups:

  character, optional argument indicating one or more column names to
  obtain evaluation criteria weighted for specific groups (e.g., age
  groups, nationality).

## Value

\#' A data frame. The data frame includes the following summary
measures:

- `mpe` is the mean percentage error (**mpe**; or mean algebraic
  percentage error **malpe**); it is a bias indicator as it takes the
  **direction** of the error into account. Positive values indicate that
  the projections were, overall, too high. Negative values indicate that
  the projections were, overall, too low. The closer the value is too
  zero, the lower the bias.

- `medpe` is the median (or middle value) of the percentage error
  (**medpe**). Particularly useful for small samples or skewed
  distributions. The closer the value is too zero, the lower the bias.

- `mape` is the mean **absolute** percentage / proportional error
  (**mape**). It considers variance (or amplitude) and can be seen as a
  measure of precision. The smaller the value, the lower is the average
  error.

- `medape` is the median (or middle value) of the **absolute**
  percentage error (**medape**). Particularly useful for small samples
  or skewed distributions. The smaller the value, the lower is the
  average error.

- `rmse` is the root mean square error; it is an indication of the
  robustness or quality of the projection. The smaller the value, the
  more robust the projection.

- `wmape` is the **weighted** mean **absolute** percentage error
  (**wmape**); in contrast to `mape`, this measure weights each absolute
  percentage error according to the population size of the "focal" group
  (e.g., nationality, age group) and thus considers domain size. Put
  differently, errors count more in populous groups than in less
  populous groups. This measure is particularly useful when population
  sizes vary strongly. The closer the value, the more precise is the
  projection.

- `n_measure` is the number of times a summary measure occurs (per
  weight group if requested).

- `ape_under_1` is a measure to gauge the error distribution; it
  indicates the proportion of observations that have absolute percentage
  errors smaller than 1%.

- `ape_under_5` is a measure to gauge the error distribution; it
  indicates the proportion of observations that have absolute percentage
  errors smaller than 5%.

## References

Baker, J., et al. (2015). Sub-county population estimates using
administrative records: A municipal-level case study in New Mexico. In
M. N. Hoque & L. B. Potter (Eds.), Emerging techniques in applied
demography (pp. 63-79). Springer,
<https://doi.org/10.1007/978-94-017-8990-5_6>

Bérard-Chagnon, J. (2015) Using tax data to estimate the number of
families and households in Canada. In M. N. Hoque & L. B. Potter (Eds.),
Emerging techniques in applied demography (pp. 137-153). Springer,
<https://doi.org/10.1007/978-94-017-8990-5_10>

Reinhold M. & Thomsen, S. L. (2015) Subnational population projections
by age: An evaluation of combined forecast techniques, Population
Research and Policy Review, 34, 593-613,
<https://doi.org/10.1007/s11113-015-9362-0>

Wilson, T. (2012). Forecast accuracy and uncertainty of Australian
Bureau of Statistics state and territory population projections,
International Journal of Population Research, 1, 419824,
<https://doi.org/10.1155/2012/419824>

Wilson, T. (2016). Evaluation of alternative cohort-component models for
local area population forecasts, Population Research and Policy Review,
35, 241-261, <https://doi.org/10.1007/s11113-015-9380-y>
