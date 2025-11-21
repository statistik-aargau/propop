# Compute evaluation measures

Uses the differences between the results from a projection and a
benchmark to compute performance measures.

## Usage

``` r
compute_measures(combined, weight_groups = NULL)
```

## Arguments

- combined:

  data frame created with
  [`propop::prepare_evaluation()`](https://statistik-aargau.github.io/propop/reference/prepare_evaluation.md).

- weight_groups:

  character, optional argument indicating one or more column names to
  obtain evaluation criteria weighted for specific groups (e.g., age
  groups, nationality).

## Value

A data frame. The following evaluation criteria can directly be
interpreted and used for descriptive comparisons:

- `error` is the forecast error; it quantifies the level of
  under-projection (negative values) and over-projection (positive
  values) relative to the benchmark `n_benchmark`.

- `pe` is the percentage error and expresses the under- /
  over-projection in percent of the benchmark `n_benchmark`.

- `ape` is the absolute percentage error; it is the absolute deviation
  in percent of the benchmark `n_benchmark`, thus only showing the
  extent of the error but not the direction.

- `w_ape` is the weighted absolute percentage error; it weighs each
  absolute percentage error according to the population size of the
  focal group (e.g., nationality, age group). The weighted version is
  useful as an aggregated measure when groups vary strongly in terms of
  population size. Only returned when the argument `weight_groups`
  contains at least one grouping variable.

The following helper variables are used to compute aggregate measures.
They are only returned when weight groups are provided via the argument
`weight_groups`.

- `n_tot` is the total number of people (i.e., sum of the number of
  people in all demographic groups); used to compute the weighted
  absolute percentage error.

- `group_tot` is the number of people in the focal group; used to
  compute the weighted absolute percentage error.

- `weight` is the share of the (optional) focal group (e.g.,
  municipality type / size, nationality, age group) relative to all
  people; used to compute the weighted absolute percentage error.

## Details

The input is a data frame created with
[`propop::prepare_evaluation()`](https://statistik-aargau.github.io/propop/reference/prepare_evaluation.md).
It includes a benchmark (typically the observed population records,
i.e., the number of people per spatial unit, demographic group, and
year) and the corresponding projected number of people. The input can
range from low resolution (e.g., total number of people per
municipality) to high resolution (e.g., 101 age classes, nationality,
sex).

For more details on usage, see
[`vignette("evaluate", package = "propop")`](https://statistik-aargau.github.io/propop/articles/evaluate.md).

## References

Baker, J., et al. (2015). Sub-county population estimates using
administrative records: A municipal-level case study in New Mexico. In
M. N. Hoque & L. B. Potter (Eds.), Emerging techniques in applied
demography (pp. 63-79). Springer,
<https://doi.org/10.1007/978-94-017-8990-5_6>

Wilson, T. (2012). Forecast accuracy and uncertainty of Australian
Bureau of Statistics state and territory population projections,
International Journal of Population Research, 1, 419824,
<https://doi.org/10.1155/2012/419824>

Wilson, T. (2016). Evaluation of alternative cohort-component models for
local area population forecasts, Population Research and Policy Review,
35, 241-261, <https://doi.org/10.1007/s11113-015-9380-y>

## Examples

``` r
if (FALSE) { # \dontrun{
# Get evaluation measures without weights
compute_measures(combined)
# Get evaluation measures weighted for groups
compute_measures(combined, weight_groups = c("age", "nat"))
} # }
```
