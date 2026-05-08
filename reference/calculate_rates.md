# Calculate emigration rates to subregions

The function calculates the mean of past emigration to other
municipalities within the canton for all municipalities and all possible
combinations of age group, sex, and nationality.

## Usage

``` r
calculate_rates(
  past_migration,
  n_jan,
  births,
  emi_n,
  spatial_unit,
  method,
  year_range = NULL,
  age_group = 1,
  binational = TRUE,
  two_sex = TRUE
)
```

## Arguments

- past_migration:

  data frame, containing aggregated migration records. The data frame
  must include one row for each combination of year, spatial unit,
  nationality, sex, and age.

- n_jan:

  character, column containing the starting population (typically the
  number of people per group in January).

- births:

  character, column containing the number of births in the group of
  0-year olds.

- emi_n:

  character, column containing the total number of people emigrating per
  spatial unit and demographic group.

- spatial_unit:

  character, column containing the spatial units.

- method:

  character, method to calculate average shares, i.e. `mean` or
  `median`.

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

A data frame that includes the average emigration rate.

## Examples

``` r
# Compute mean emigration rate
calculate_rates(
 past_migration = ag_migration_subregional,
 n_jan = n_jan,
 births = births,
 emi_n = emi_n,
 spatial_unit = spatial_unit,
 method = "mean",
 year_range = c(2022:2024),
 age_group = 5,
 binational = TRUE,
 two_sex = TRUE
)
#> # A tibble: 6,060 × 12
#>     year spatial_unit   age age_group nat   sex   n_jan births n_base emi_n
#>    <int> <chr>        <dbl> <chr>     <chr> <chr> <int>  <int>  <int> <int>
#>  1  2022 1                0 age_0_4   ch    m         0    480    480    24
#>  2  2022 1                1 age_0_4   ch    m       552      0    552    62
#>  3  2022 1                2 age_0_4   ch    m       526      0    526    38
#>  4  2022 1                3 age_0_4   ch    m       501      0    501    35
#>  5  2022 1                4 age_0_4   ch    m       478      0    478    30
#>  6  2022 1                5 age_5_9   ch    m       464      0    464    21
#>  7  2022 1                6 age_5_9   ch    m       464      0    464     9
#>  8  2022 1                7 age_5_9   ch    m       465      0    465    14
#>  9  2022 1                8 age_5_9   ch    m       469      0    469     8
#> 10  2022 1                9 age_5_9   ch    m       453      0    453     9
#> # ℹ 6,050 more rows
#> # ℹ 2 more variables: emi_rate <dbl>, method <chr>
```
