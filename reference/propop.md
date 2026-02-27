# Project population development

Wrapper function to project population development using the cohort
component method. This function iterates across years and spatial units
calling the function `project_population.R`, which performs the
calculations.

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
  subregional = NULL,
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

  character or NULL, indicates if subregional migration patterns (e.g.,
  movement between municipalities within a canton) are part of the
  projection (default `subregional = NULL`). Requires input on the level
  of subregions (in `parameters` and `population`). Two calculation
  methods are supported to distribute people between subregions: With
  `subregional = "net"`, the net migration between subregions is added
  to the population balance. Net migration numbers must be specified in
  a data column `mig_sub` in `parameters`. With `subregional = "rate"`,
  the numbers for subregional emigrants are subtracted from the
  population balance, then redistributed back to all subregional units
  as subregional immigration; `parameters` must contain the columns
  `emi_sub` and `imm_sub`.

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

- mor_n:

  numeric, number of deaths.

- emi_int_n:

  numeric, number of people who emigrate to other countries.

- emi_nat_n:

  numeric, number of people who emigrate to other cantons.

- imm_int_n:

  numeric, number of people who immigrate from other countries.

- imm_nat_n:

  numeric, number of people who immigrate from other cantons.

- acq_n:

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
  subregional = NULL,
  binational = TRUE
)
#> 
#> ── Running projection for 3 scenario(s). ───────────────────────────────────────
#> 
#> ── Starting population projection ──────────────────────────────────────────────
#> ℹ Processing...
#> ✔ Processing completed in [1.1s]
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
#> Subregional migration: "no"
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
#> # A tibble: 4,848 × 17
#>     year scen  spatial_unit nat   sex     age births n_jan mor_n emi_int_n
#>    <int> <chr> <chr>        <fct> <fct> <dbl>  <dbl> <dbl> <dbl>     <dbl>
#>  1  2024 high  Aargau       ch    m         0  2633.     0 9.00       6.00
#>  2  2024 high  Aargau       ch    m         1     0   2371 1.000     13.0 
#>  3  2024 high  Aargau       ch    m         2     0   2542 0.998     13.0 
#>  4  2024 high  Aargau       ch    m         3     0   2891 0.983     14.0 
#>  5  2024 high  Aargau       ch    m         4     0   2766 0         13.0 
#>  6  2024 high  Aargau       ch    m         5     0   2794 0         12.0 
#>  7  2024 high  Aargau       ch    m         6     0   2782 0         11.0 
#>  8  2024 high  Aargau       ch    m         7     0   2787 0         10.00
#>  9  2024 high  Aargau       ch    m         8     0   2726 0          9.00
#> 10  2024 high  Aargau       ch    m         9     0   2837 0          9.00
#> # ℹ 4,838 more rows
#> # ℹ 7 more variables: emi_nat_n <dbl>, imm_int_n <dbl>, imm_nat_n <dbl>,
#> #   acq_n <dbl>, n_dec <dbl>, delta_n <dbl>, delta_perc <dbl>
propop(
  parameters = fso_parameters |>
    dplyr::filter(scen == "reference" | scen == "high"),
  year_first = 2024,
  year_last = 2026,
  scenarios = c("reference", "high"),
  population = fso_population,
  subregional = NULL,
  binational = TRUE
)
#> 
#> ── Running projection for 2 scenario(s). ───────────────────────────────────────
#> 
#> ── Starting population projection ──────────────────────────────────────────────
#> ℹ Processing...
#> ✔ Processing completed in [598ms]
#> 
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
#> - Scenario "high": 758422
#> - Scenario "reference": 751159
#> ════════════════════════════════════════════════════════════════════════════════
#> 
#> ── Please note ─────────────────────────────────────────────────────────────────
#> ℹ As of propop v2.0.0, propop() uses tables instead of matrices to calculate projections.
#> ℹ The old function is still available as propop_legacy() but won't be further maintained.
#> ════════════════════════════════════════════════════════════════════════════════
#> # A tibble: 2,424 × 17
#>     year scen  spatial_unit nat   sex     age births n_jan mor_n emi_int_n
#>    <int> <chr> <chr>        <fct> <fct> <dbl>  <dbl> <dbl> <dbl>     <dbl>
#>  1  2024 high  Aargau       ch    m         0  2633.     0 9.00       6.00
#>  2  2024 high  Aargau       ch    m         1     0   2371 1.000     13.0 
#>  3  2024 high  Aargau       ch    m         2     0   2542 0.998     13.0 
#>  4  2024 high  Aargau       ch    m         3     0   2891 0.983     14.0 
#>  5  2024 high  Aargau       ch    m         4     0   2766 0         13.0 
#>  6  2024 high  Aargau       ch    m         5     0   2794 0         12.0 
#>  7  2024 high  Aargau       ch    m         6     0   2782 0         11.0 
#>  8  2024 high  Aargau       ch    m         7     0   2787 0         10.00
#>  9  2024 high  Aargau       ch    m         8     0   2726 0          9.00
#> 10  2024 high  Aargau       ch    m         9     0   2837 0          9.00
#> # ℹ 2,414 more rows
#> # ℹ 7 more variables: emi_nat_n <dbl>, imm_int_n <dbl>, imm_nat_n <dbl>,
#> #   acq_n <dbl>, n_dec <dbl>, delta_n <dbl>, delta_perc <dbl>
```
