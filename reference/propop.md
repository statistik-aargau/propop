# Project population development

Wrapper function to project population development using the cohort
component method (see e.g.,
[here](https://www.ag.ch/media/kanton-aargau/dfr/dokumente/statistik/statistische-daten/oeffentliche-statistik/01-bevoelkerung/kantonsdaten/bevoelkerungsprognosen/bev-lkerungsprojektion-technischerbegleitbericht-2025.pdf)
and
[here](https://github.com/statistik-aargau/propop-additional-resources/blob/358ffa280f3777af34d3ac4b2782c1171ed93beb/FSO_2020_Meth_scenarios%20cant.pdf)
for more details). This function calls `project_raw.R`, which uses
matrix algebra to implement the demographic balancing equations.

You can either use your own parameters and starting population or
download these data from the Swiss Federal Statistical Office (FSO). For
instructions on how to download this information from
[STAT-TAB](https://www.bfs.admin.ch/bfs/en/home/services/recherche/stat-tab-online-data-search.html),
see
[`vignette("prepare_data", package = "propop")`](https://statistik-aargau.github.io/propop/articles/prepare_data.md).

The projection parameters need to be passed to `propop::propop()` as a
**single data frame** (with the parameters as columns). The column
types, names, and factor levels need to match the specifications listed
below under `parameters`.

If nothing else is indicated in argument `scenarios`, `propop()` runs
and returns all **scenarios** provided via `parameters`.

For more details on how to use this function to project the population
development on the level of a canton, see
[`vignette("project_single_region", package = "propop")`](https://statistik-aargau.github.io/propop/articles/project_single_region.md).

## Usage

``` r
propop(
  parameters,
  population,
  year_first,
  year_last,
  scenarios = NULL,
  age_groups = 101,
  fert_first = 16,
  fert_last = 50,
  share_born_female = 100/205,
  subregional = FALSE,
  binational = TRUE,
  spatial_unit = "spatial_unit"
)
```

## Arguments

- parameters:

  data frame containing the FSO rates and numbers to run the projection
  for a specific spatial level (e.g., canton, municipality).

  - `year`, character, projection year.

  - `spatial_unit`, character, ID of spatial entity (e.g., canton,
    municipality) for which to run the projections.

  - `scen`, character, one or several projection scenario(s). The main
    scenarios are usually "reference", "low" growth, and "high" growth.

  - `nat`, character, nationality (`ch` = Swiss; `int` = foreign /
    international). Required if binational = `TRUE`.

  - `sex`, character (`f` = female, `m` = male).

  - `age`, numeric, typically ranging from 0 to 100 (incl. \>100).

  - `birthrate`, numeric, number of births per mother

  - `int_mothers`, numeric, proportion of children with Swiss
    nationality born to non-Swiss mothers. Required if binational =
    `TRUE`.

  - `mor`, numeric, prospective mortality rate (probability of death).

  - `acq`, numeric, rate of acquisition of Swiss citizenship. Required
    if binational = `TRUE`.

  - `emi_int`, numeric, rate of people emigrating abroad (number of
    immigrants - number of emigrants).

  - `emi_nat`, rate of people emigrating to other cantons.

  - `imm_int_n`, numeric, number of people immigrating from abroad.

  - `imm_nat_n`, numeric, number of people immigrating from other
    cantons.

  - `mig_sub` **(optional)**, numeric, net migration per subregion; this
    is the migration from / to other subregions (e.g., municipalities,
    districts) within the main superordinate projection unit (e.g., a
    canton). Accounts for movements between different subregions. Needs
    to be provided by the user.

- population:

  data frame including the starting population of each demographic group
  and each spatial unit. Possible values are the same as in `parameters`
  (apart from year). The data frame only includes one year, usually the
  one preceding the first year of the projection.

  - `year` character, should be `year_first` - 1.

  - `spatial_unit` character.

  - `nat` character.

  - `sex` character.

  - `age` numeric.

  - `n` numeric, number of people per demographic group.

- year_first:

  numeric, first year to be projected.

- year_last:

  numeric, last year to be projected.

- scenarios:

  **(optional)**, character, indicating which projection scenario(s)
  shall be run; the corresponding information must be available in
  `parameters`. Defaults to the values in variable `scen` in
  `parameters`.

- age_groups:

  numeric, number of age classes. Creates a vector with 1-year age
  classes running from `0` to (`age_groups` - 1). Must currently be set
  to `= 101` (FSO standard number of age groups).

- fert_first:

  numeric, first year of female fertility. Defaults to 16 (FSO standard
  value).

- fert_last:

  numeric, last year of female fertility. Defaults to 50 (FSO standard
  value).

- share_born_female:

  numeric, fraction of female babies. Defaults to 100 / 205 (FSO
  standard value).

- subregional:

  boolean, `TRUE` indicates that subregional migration patterns (e.g.,
  movement between municipalities within a canton) are part of the
  projection. Requires input on the level of subregions (in `parameters`
  and `population`).

- binational:

  boolean, `TRUE` indicates that projections discriminate between two
  groups of nationalities. `FALSE` indicates that the projection is run
  without distinguishing between nationalities.

- spatial_unit:

  character, name of variable containing the names of the region or
  subregions for which the projection shall be performed.

## Value

Returns a **data frame** that includes the number of people for each
demographic group per year (for projected years) and spatial unit. The
number of rows is the product of all scenarios times all years times all
demographic groups times all spatial units. The output includes several
**identifiers** that indicate to which scenario, demographic group,
year, and spatial unit the results in the rows refer to:

- year:

  integer, indicating the projected years.

- scen:

  character, indicating the projected scenario(s).

- spatial_unit:

  factor, spatial units for which the projection was run (e.g., canton,
  districts, municipalities).

- age:

  integer, ranging from `0`n to `100` years (including those older than
  100).

- sex:

  factor, female (`f`) and male (`m`).

- nat:

  factor, indicates if the nationality is Swiss (`ch`) or international
  / foreign (`int`). This variable is only returned if
  `binational = TRUE`.

The output also includes columns related to the **size and change of the
population:**

- n_jan:

  numric, start-of-year population per demographic group.

- n_dec:

  numeric, end-of-year population per demographic group.

- delta_n:

  numeric, population change per demographic group from the start to the
  end of the year in *absolute numbers*.

- delta_perc:

  numeric, population change per demographic group from the start to the
  end of the year in *percentages*.

The **components** that are used to project the development of the
population are also included in the output:

- births:

  numeric, number of births (non-zero values are only available for age
  = 0).

- mor:

  numeric, number of deaths.

- emi_int:

  numeric, number of people who emigrate to other countries.

- emi_nat:

  numeric, number of people who emigrate to other cantons.

- imm_int:

  numeric, number of people who immigrate from other countries.

- imm_nat:

  numeric, number of people who immigrate from other cantons.

- acq:

  numeric, number of people who acquire Swiss citizenship (only returned
  if `binational = TRUE`.)

## Examples

``` r
# Run projection for the sample data (whole canton of Aargau)
propop(
  parameters = fso_parameters,
  year_first = 2024,
  year_last = 2027,
  population = fso_population,
  subregional = FALSE,
  binational = TRUE
)
#> Running projection for: Aargau (Scenario: high)
#> ✔ Year: 2024
#> ✔ Year: 2025
#> ✔ Year: 2026
#> ✔ Year: 2027
#> Running projection for: Aargau (Scenario: low)
#> ✔ Year: 2024
#> ✔ Year: 2025
#> ✔ Year: 2026
#> ✔ Year: 2027
#> Running projection for: Aargau (Scenario: reference)
#> ✔ Year: 2024
#> ✔ Year: 2025
#> ✔ Year: 2026
#> ✔ Year: 2027
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
#> Subregional migration: "no"
#> ────────────────────────────────────────────────────────────────────────────────
#> Projected population size by 2027:
#> - Scenario "high": 768888
#> - Scenario "low": 748703
#> - Scenario "reference": 758993
#> ════════════════════════════════════════════════════════════════════════════════
#> # A tibble: 4,848 × 17
#>     year scen  spatial_unit   age sex   nat   n_jan births   mor emi_int emi_nat
#>    <dbl> <fct> <fct>        <dbl> <fct> <fct> <dbl>  <dbl> <dbl>   <dbl>   <dbl>
#>  1  2024 high  Aargau           0 m     ch        0  2633. 9.00     6.00    21.0
#>  2  2024 high  Aargau           1 m     ch     2371     0  1.000   13.0     49.1
#>  3  2024 high  Aargau           2 m     ch     2542     0  0.998   13.0     56.0
#>  4  2024 high  Aargau           3 m     ch     2891     0  0.983   14.0     54.0
#>  5  2024 high  Aargau           4 m     ch     2766     0  0       13.0     44.0
#>  6  2024 high  Aargau           5 m     ch     2794     0  0       12.0     38.0
#>  7  2024 high  Aargau           6 m     ch     2782     0  0       11.0     33.0
#>  8  2024 high  Aargau           7 m     ch     2787     0  0       10.00    29.0
#>  9  2024 high  Aargau           8 m     ch     2726     0  0        9.00    24.0
#> 10  2024 high  Aargau           9 m     ch     2837     0  0        9.00    22.0
#> # ℹ 4,838 more rows
#> # ℹ 6 more variables: imm_int <dbl>, imm_nat <dbl>, acq <dbl>, n_dec <dbl>,
#> #   delta_n <dbl>, delta_perc <dbl>
propop(
  parameters = fso_parameters |>
    dplyr::filter(scen == "reference" | scen == "high"),
  year_first = 2024,
  year_last = 2026,
  scenarios = c("reference", "high"),
  population = fso_population,
  subregional = FALSE,
  binational = TRUE
)
#> Running projection for: Aargau (Scenario: high)
#> ✔ Year: 2024
#> ✔ Year: 2025
#> ✔ Year: 2026
#> Running projection for: Aargau (Scenario: reference)
#> ✔ Year: 2024
#> ✔ Year: 2025
#> ✔ Year: 2026
#> 
#> ── Settings used for the projection ────────────────────────────────────────────
#> Scenario(s): "high" and "reference"
#> Year of starting population: 2023
#> Number of age groups: 101
#> Fertile period: 16-50
#> Share of female newborns: 0.488
#> Size of starting population: 726894
#> Projection period: 2024-2026
#> Nationality-specific projection: "yes"
#> Subregional migration: "no"
#> ────────────────────────────────────────────────────────────────────────────────
#> Projected population size by 2026:
#> - Scenario "high": 758421
#> - Scenario "reference": 751159
#> ════════════════════════════════════════════════════════════════════════════════
#> # A tibble: 2,424 × 17
#>     year scen  spatial_unit   age sex   nat   n_jan births   mor emi_int emi_nat
#>    <dbl> <fct> <fct>        <dbl> <fct> <fct> <dbl>  <dbl> <dbl>   <dbl>   <dbl>
#>  1  2024 high  Aargau           0 m     ch        0  2633. 9.00     6.00    21.0
#>  2  2024 high  Aargau           1 m     ch     2371     0  1.000   13.0     49.1
#>  3  2024 high  Aargau           2 m     ch     2542     0  0.998   13.0     56.0
#>  4  2024 high  Aargau           3 m     ch     2891     0  0.983   14.0     54.0
#>  5  2024 high  Aargau           4 m     ch     2766     0  0       13.0     44.0
#>  6  2024 high  Aargau           5 m     ch     2794     0  0       12.0     38.0
#>  7  2024 high  Aargau           6 m     ch     2782     0  0       11.0     33.0
#>  8  2024 high  Aargau           7 m     ch     2787     0  0       10.00    29.0
#>  9  2024 high  Aargau           8 m     ch     2726     0  0        9.00    24.0
#> 10  2024 high  Aargau           9 m     ch     2837     0  0        9.00    22.0
#> # ℹ 2,414 more rows
#> # ℹ 6 more variables: imm_int <dbl>, imm_nat <dbl>, acq <dbl>, n_dec <dbl>,
#> #   delta_n <dbl>, delta_perc <dbl>
```
