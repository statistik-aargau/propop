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
#> ✔ Processing completed in [1.4s]
#> 
#> 
#> ── Settings used for the projection ────────────────────────────────────────────
#> Scenario(s): "high", "low", and "reference"
#> Year of starting population: 2023
#> Number of age groups: 101
#> Fertile period: 16-50
#> Share of female newborns: 0.488
#> Size of starting population: 726894
#> Projection period: 2024-2027
#> Nationality-specific projection: "yes"
#> Subregional migration: "yes"
#> ────────────────────────────────────────────────────────────────────────────────
#> Projected population size by 2027:
#> - Scenario "high": 768888
#> - Scenario "low": 748703
#> - Scenario "reference": 758993
#> ════════════════════════════════════════════════════════════════════════════════
#> 
#> ── Please note ─────────────────────────────────────────────────────────────────
#> ℹ As of propop v2.0.0, propop() uses tables instead of matrices to calculate projections.
#> ℹ The old function is still available as propop_legacy() but won't be further maintained.
#> ════════════════════════════════════════════════════════════════════════════════
#> 
#> ── Result of population equation components check ──────────────────────────────
#> ℹ Total rows checked: 4848
#> ✔ Check passed: Equations in all rows add up and there are no missing values.
```
