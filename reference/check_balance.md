# Check if population equation components add up within one year

Function to check if the components in the
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
output add up. Takes the population at the beginning of the year
(`n_jan`), adds all obligatory components (births - mor_n - emi_int_n -
emi_nat_n + imm_int_n + imm_nat_n) and optional components (acq_n,
mig_sub, emi_sub_n, imm_sub_n) and checks if the sum is equal to the
population at the end of the year (`n_dec`).

## Usage

``` r
check_balance(.data)
```

## Arguments

- .data:

  data frame containing population projections; can be created with
  [`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md).

## Value

data frame containing summary statistics.

## Examples

``` r
propop(
  parameters = fso_parameters,
  year_first = 2026,
  year_last = 2027,
  population = fso_population,
  subregional = FALSE,
  binational = TRUE
) |>
  check_balance()
#> 
#> ── Running projection for 3 scenario(s). ───────────────────────────────────────
#> ℹ Process...
#> ✔ Processing completed in [721ms]
#> 
#> 
#> ── Settings used for the projection ────────────────────────────────────────────
#> Scenario(s): "high", "low", and "reference"
#> Year of starting population: 2025
#> Number of age groups: 101
#> Fertile period: 16-50
#> Share of female newborns: 0.488
#> Size of starting population: 743768
#> Projection period: 2026-2027
#> Nationality-specific projection: "yes"
#> Subregional migration: "no"
#> ────────────────────────────────────────────────────────────────────────────────
#> Projected population size by 2027:
#> - Scenario "high": 764992
#> - Scenario "low": 753709
#> - Scenario "reference": 759453
#> ════════════════════════════════════════════════════════════════════════════════
#> 
#> ── Result of population equation components check ──────────────────────────────
#> ℹ Total rows checked: 2424
#> ✔ Check passed: Equations in all rows add up and there are no missing values.
```
