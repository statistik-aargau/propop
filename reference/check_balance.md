# Check if population equation components add up within one year

Function to check if the components in the
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
output add up. Takes the population at the beginning of the year
(`n_jan`), adds all components (births - mor - emi_int - emi_nat +
imm_int + imm_nat + acq) and checks if the sum is equal to the
population at the end of the year (`n_dec`).

## Usage

``` r
check_balance(data)
```

## Arguments

- data:

  data frame containing population projections; can be created with
  [`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md).

## Value

data frame containing summary statistics.

## Examples

``` r
propop(
 parameters = fso_parameters,
 year_first = 2024,
 year_last = 2027,
 population = fso_population,
 subregional = FALSE,
 binational = TRUE
) |>
 check_balance()
#> 
#> ── Starting population projection ──────────────────────────────────────────────
#> ℹ Processing...
#> 
#> ℹ Processing...
#> ── Settings used for the projection ────────────────────────────────────────────
#> ℹ Processing...
#> Scenario(s): "high", "low", and "reference"
#> ℹ Processing...
#> Year of starting population: 2023
#> ℹ Processing...
#> Number of age groups: 101
#> ℹ Processing...
#> Fertile period: 16-50
#> ℹ Processing...
#> Share of female newborns: 0.488
#> ℹ Processing...
#> Size of starting population: 726894
#> ℹ Processing...
#> Projection period: 2024-2027
#> ℹ Processing...
#> Nationality-specific projection: "yes"
#> ℹ Processing...
#> Subregional migration: "yes"
#> ℹ Processing...
#> ────────────────────────────────────────────────────────────────────────────────
#> ℹ Processing...
#> Projected population size by 2027:
#> ℹ Processing...
#> - Scenario "high": 768888
#> ℹ Processing...
#> - Scenario "low": 748703
#> ℹ Processing...
#> - Scenario "reference": 758993
#> ℹ Processing...
#> ════════════════════════════════════════════════════════════════════════════════
#> ℹ Processing...
#> 
#> ℹ Processing...
#> ── Please note ─────────────────────────────────────────────────────────────────
#> ℹ Processing...
#> ℹ As of propop v2.0.0, `propop()` uses tables instead of matrices to calculate projections. The matrix-function was renamed to `propop_legacy()`. It is still operational but won't be further maintained.
#> ℹ Processing...
#> 
#> ℹ Processing...
#> ────────────────────────────────────────────────────────────────────────────────
#> ℹ Processing...
#> ✔ Processing completed in [1.5s]
#> 
#> 
#> ── Result of population equation components check ──────────────────────────────
#> ℹ Total rows checked: 4848
#> ✔ Check passed: Equations in all rows add up and there are no missing values.
```
