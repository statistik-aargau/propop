# Projections for a single region

``` r
library(propop)

# load package data
data("fso_parameters")
data("fso_population")
```

## Overview

This vignette explains how to use
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
to perform population projections for a **single region** such as a
**canton** (for projections of subregions, see [this
vignette](https://statistik-aargau.github.io/propop/articles/project_subregions.md)).

The function was tailored to the context of Switzerland. It uses the
cohort component method for cantonal scenarios, which was developed by
the Federal Statistical Office (FSO,
[2020](https://github.com/statistik-aargau/propop-additional-resources/blob/358ffa280f3777af34d3ac4b2782c1171ed93beb/FSO_2020_Meth_scenarios%20cant.pdf);
only available in French). To run the function, you need the following
input:

- a data frame with the **starting population**, that is, the most
  up-to-date number of people for each demographic group before the
  first projection year; to illustrate, the example population data in
  `propop` are from 31. December 2023.

- a data frame containing model **parameters**, that is, information
  about how key demographic variables such as mortality are expected to
  develop in the future; these are available for 2024-2055.

- some global arguments which do not change over time or across
  demographic groups (e.g., proportion of female to male newborns).
  `propop` uses FSO’s standard values as default option for these
  arguments.

Importantly, the two data frames’ structure (number, names, type of
columns) must correspond exactly to the **specifications** shown in
[this
vignette](https://statistik-aargau.github.io/propop/articles/prepare_data.md).
Among other things, it is **mandatory** to provide two levels for sex.
Nationality can have either one or two levels. The function requires
1-year age groups ranging from 0 to 100 years (incl. those who are
older).

## Projections for a single region

The package `propop` includes the population data from the canton of
Aargau from 2023 and the FSO parameters from the [population development
scenarios
2025](https://www.bfs.admin.ch/bfs/en/home/statistics/catalogues-databases.assetdetail.14963221.html).
Using these resources, we can project the population for the canton as a
whole for 1-year age groups for the period 2024-2055.

The start and end of women’s fertile period, the proportion of babies
born as female, and the share of babies born by mothers who are not
Swiss are defined in
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md).
We use FSO’s standard values as default for these arguments.

If the argument `scenarios` is not specified,
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
returns a projection for all scenarios available in `parameters`.  

``` r
projection_canton_2030 <- propop(
  parameters = fso_parameters,
  scenarios = "reference",
  year_first = 2024,
  year_last = 2030,
  population = fso_population,
  subregional = FALSE,
  binational = TRUE
)
#> 
#> ── Running projection for 3 scenario(s). ───────────────────────────────────────
#> ℹ Process...
#> ✔ Processing completed in [2s]
#> 
#> ── Settings used for the projection ────────────────────────────────────────────
#> Scenario(s): "reference"
#> Year of starting population: 2023
#> Number of age groups: 101
#> Fertile period: 16-50
#> Share of female newborns: 0.488
#> Size of starting population: 726894
#> Projection period: 2024-2030
#> Nationality-specific projection: "yes"
#> Subregional migration: "yes"
#> ────────────────────────────────────────────────────────────────────────────────
#> Projected population size by 2030:
#> - Scenario "reference": 781521
#> ════════════════════════════════════════════════════════════════════════════════
#> 
#> ── Please note ─────────────────────────────────────────────────────────────────
#> ℹ As of propop v2.0.0, propop() uses tables instead of matrices to calculate projections.
#> ℹ The old function is still available as propop_legacy() but won't be further maintained.
#> ════════════════════════════════════════════════════════════════════════════════

projection_canton_2030 |>
  # round to 2 digits
  dplyr::mutate(across(n_jan:n_dec, \(x) sprintf(fmt = "%.0f", x))) |>
  DT::datatable(filter = "top") |>
  DT::formatStyle(
    "n_jan",
    backgroundColor = "#ffcc8f"
  ) |>
  DT::formatStyle(
    c("births", "mor_n", "emi_int_n", "emi_nat_n", "imm_int_n", "imm_nat_n", "acq_n"),
    backgroundColor = "#96D4FF"
  ) |>
  DT::formatStyle(
    "n_dec",
    backgroundColor = "#007AB8"
  )
#> Warning in instance$preRenderHook(instance): It seems your data is too big for
#> client-side DataTables. You may consider server-side processing:
#> https://rstudio.github.io/DT/server.html
```

  
The **output** file includes the number of people in January (`n_jan`)
and the number of people in December (`n_dec`) per demographic group and
year. The components that are used to project the population growth from
the start to the end of the year are also included in the output. Thus,
all elements of the cohort component equation are directly available
from the output. Note that the number of people who acquire the Swiss
citizenship (`acq_n`) are added if the demographic group is Swiss and
subtracted if the demographic group has a different nationality.  

## No distinction between nationalities

As of version 0.2.0, it is possible to run projections without
distinguishing between Swiss and non-Swiss nationals. The simplest way
to achieve this is to provide population data and parameters without the
nationality-specific columns (remove `nat`, `acq`, `births_int_ch`).

Let’s adapt the input files accordingly. To keep things simple, we run
the projection for **non-Swiss nationals**. It goes without saying that
using
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
like this in real settings requires more preparation (e.g., determining
a single value for the joint population when parameters differ between
Swiss and non-Swiss people).

``` r
fso_parameters_int <- fso_parameters |>
  # drop Swiss people, keep reference scenario
  dplyr::filter(nat == "int" & scen == "reference") |>
  #   remove `nat`, `acq` and `births_int_ch` from `parameters`
  dplyr::select(-c(nat, acq, int_mothers))

fso_population_int <- fso_population |>
  # drop Swiss people
  dplyr::filter(nat == "int") |>
  # remove `nat` from `population`
  dplyr::select(-nat)
```

When calling
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md),
you need to set `binational = FALSE`.

``` r
projection_int <- propop(
  parameters = fso_parameters_int,
  scenarios = "reference",
  year_first = 2024,
  year_last = 2030,
  population = fso_population_int,
  subregional = FALSE,
  binational = FALSE
)
#> 
#> ── Running projection for 1 scenario(s). ───────────────────────────────────────
#> ℹ Process...
#> ✔ Processing completed in [701ms]
#> 
#> ── Settings used for the projection ────────────────────────────────────────────
#> Scenario(s): "reference"
#> Year of starting population: 2023
#> Number of age groups: 101
#> Fertile period: 16-50
#> Share of female newborns: 0.488
#> Size of starting population: 198599
#> Projection period: 2024-2030
#> Nationality-specific projection: "no"
#> Subregional migration: "yes"
#> ────────────────────────────────────────────────────────────────────────────────
#> Projected population size by 2030:
#> - Scenario "reference": 252660
#> ════════════════════════════════════════════════════════════════════════════════
#> 
#> ── Please note ─────────────────────────────────────────────────────────────────
#> ℹ As of propop v2.0.0, propop() uses tables instead of matrices to calculate projections.
#> ℹ The old function is still available as propop_legacy() but won't be further maintained.
#> ════════════════════════════════════════════════════════════════════════════════

projection_int |>
  # round to two digits
  dplyr::mutate(across(n_jan:n_dec, \(x) sprintf(fmt = "%.0f", x))) |>
  DT::datatable(filter = "top")
```

 
