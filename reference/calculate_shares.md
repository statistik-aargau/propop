# Calculate shares for distributing immigration among subregions

Calculates historical immigration shares across spatial units within a
region. These shares are used to allocate emigrants moving from one
subregion to another subregion (which can be done with
`calculate_rate`).

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
[`calculate_rate()`](https://statistik-aargau.github.io/propop/reference/calculate_rate.md)
for calculating the associated emigration rate `emi_sub`.

## Examples

``` r
# Calculate shares to distribute subregional immigration among spatial units
calculate_shares(
  past_migration = ag_migration_subregional,
  imm_n = "imm_n",
  year_range = c(2022:2024),
  age_group = 10,
  binational = TRUE,
  two_sex = TRUE
)
#> # A tibble: 6,060 × 10
#>     year spatial_unit   age age_group nat   sex   imm_n sum_imm_n imm_share
#>    <int> <chr>        <dbl> <chr>     <chr> <chr> <int>     <int>     <dbl>
#>  1  2022 1                0 age_0_9   ch    m         8       435    0.0184
#>  2  2022 1                1 age_0_9   ch    m        37       435    0.0851
#>  3  2022 1                2 age_0_9   ch    m        29       435    0.0667
#>  4  2022 1                3 age_0_9   ch    m        20       435    0.0460
#>  5  2022 1                4 age_0_9   ch    m        18       435    0.0414
#>  6  2022 1                5 age_0_9   ch    m        11       435    0.0253
#>  7  2022 1                6 age_0_9   ch    m         8       435    0.0184
#>  8  2022 1                7 age_0_9   ch    m        11       435    0.0253
#>  9  2022 1                8 age_0_9   ch    m         7       435    0.0161
#> 10  2022 1                9 age_0_9   ch    m         8       435    0.0184
#> # ℹ 6,050 more rows
#> # ℹ 1 more variable: method <chr>
```
