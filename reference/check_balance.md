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
  year_first = 2025,
  year_last = 2027,
  population = fso_population,
  subregional = FALSE,
  binational = TRUE
) |>
  check_balance()
#> 
#> ── Running projection for 3 scenario(s). ───────────────────────────────────────
#> ℹ Process...
#> ✔ Processing completed in [946ms]
#> 
#> 
#> ── Settings used for the projection ────────────────────────────────────────────
#> Scenario(s): "high", "low", and "reference"
#> Year of starting population: 2024
#> Number of age groups: 101
#> Fertile period: 16-50
#> Share of female newborns: 0.488
#> Size of starting population: 735808
#> Projection period: 2025-2027
#> Nationality-specific projection: "yes"
#> Subregional migration: "yes"
#> ────────────────────────────────────────────────────────────────────────────────
#> Projected population size by 2027:
#> - Scenario "high": 767384
#> - Scenario "low": 751337
#> - Scenario "reference": 759508
#> ════════════════════════════════════════════════════════════════════════════════
#> 
#> ── Please note ─────────────────────────────────────────────────────────────────
#> ℹ As of propop v2.0.0, propop() uses tables instead of matrices to calculate projections.
#> ℹ The old function is still available as propop_legacy() but won't be further maintained.
#> ════════════════════════════════════════════════════════════════════════════════
#> 
#> ── Result of population equation components check ──────────────────────────────
#> ℹ Total rows checked: 3636
#> ✔ Check passed: Equations in all rows add up and there are no missing values.
```
