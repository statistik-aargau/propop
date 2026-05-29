# Calculate shares for distributing immigration among subregions

Calculates historical immigration shares across spatial units within a
region.  
To compute the shares, the counts are summed within each spatial unit
and across all spatial units combined, across all years. Dividing the
spatial unit count by the total gives the group's share.  
These shares are used to allocate emigrants moving from one subregion to
another subregion (which can be done with `calculate_rates`).

## Usage

``` r
calculate_shares(
  past_migration,
  imm_n,
  year_range = NULL,
  age_group = 1,
  binational = TRUE,
  two_sex = TRUE
)
```

## Arguments

- past_migration:

  data frame, historical records (e.g., immigration from other cantons,
  countries or subregions). Required columns are: `year`,
  `spatial_unit`, `age` and a column that contains aggregated historical
  migration records. The columns `nat` and `sex` are optional.

- imm_n:

  character, name of the column which contains the data for aggregated
  historical migration records.

- year_range:

  **(optional)** vector, years taken into consideration to calculate
  historical shares. Default uses all years present in the data.

- age_group:

  **(optional)** integer, divides continuous age values into intervals
  for calculating shares. If the argument is not specified, the default
  uses 1-year age groups.

- binational:

  **(optional)** boolean, `TRUE` indicates that the calculation
  discriminates between two groups of nationalities. `FALSE` indicates
  that the calculation does not distinguish between nationalities.

- two_sex:

  **(optional)** boolean, `TRUE` indicates that the calculation
  discriminates between two sexes. `FALSE` indicates that the
  calculation does not distinguish between sexes.

## Value

A data frame that includes the average share per demographic group and
spatial unit. `imm_share` can be used as `imm_sub` parameter when
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
uses `subregional = "rate"`.

## See also

[`propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
for details on how to account for subregional migration using the rate
method,
[`calculate_rates()`](https://statistik-aargau.github.io/propop/reference/calculate_rates.md)
for calculating the associated emigration rate `emi_sub`.

## Examples

``` r
# Calculate shares to distribute subregional immigration among spatial units
calculate_shares(
  past_migration = ag_migration_subregional,
  imm_n = hist_imm_sub_n,
  year_range = c(2022:2024),
  age_group = 10,
  binational = TRUE,
  two_sex = TRUE
)
#> # A tibble: 2,020 × 9
#>    spatial_unit   age age_group nat   sex   sum_imm_n total_imm_n imm_share
#>    <chr>        <dbl> <chr>     <chr> <chr>     <int>       <int>     <dbl>
#>  1 1                0 age_0_9   ch    m           435        2944     0.148
#>  2 1                1 age_0_9   ch    m           435        2944     0.148
#>  3 1                2 age_0_9   ch    m           435        2944     0.148
#>  4 1                3 age_0_9   ch    m           435        2944     0.148
#>  5 1                4 age_0_9   ch    m           435        2944     0.148
#>  6 1                5 age_0_9   ch    m           435        2944     0.148
#>  7 1                6 age_0_9   ch    m           435        2944     0.148
#>  8 1                7 age_0_9   ch    m           435        2944     0.148
#>  9 1                8 age_0_9   ch    m           435        2944     0.148
#> 10 1                9 age_0_9   ch    m           435        2944     0.148
#> # ℹ 2,010 more rows
#> # ℹ 1 more variable: method <chr>
```
