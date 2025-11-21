# Get population data from FSO

Users who do not have the required population data can use this
convenience function to get the mandatory starting population for
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
from the Federal Statistical Office (FSO). The function can also be used
to obtain the population records for several years (e.g., for model
performance evaluations). This function can be used to obtain data at
various spatial levels (e.g., cantons, municipalities). The most recent
data are usually about 6 to 18 months old.

To get the population data, you must use the **spelling** defined in the
corresponding FSO table (STATTAB cube
[px-x-0102010000_101](https://www.bfs.admin.ch/asset/de/32207872)).
Inspecting the column 'valueTexts' in the following package data may
also help: `data('stattab_pop_snap')`.

Changes to the API interface may break this function.

## Usage

``` r
get_population(
  number_fso = "px-x-0102010000_101",
  year,
  year_last = NULL,
  spatial_units
)
```

## Source

Federal Statistical Office:
<https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0102010000_101/-/px-x-0102010000_101.px/>

## Arguments

- number_fso:

  character, px-x table ID for population records, defaults to
  `px-x-0102010000_101`.

- year:

  numeric, year for which the population records are to be downloaded.
  This usually is the starting population. To download longer time
  periods, use `year` to indicate the first year of the period.

- year_last:

  numeric **(optional)**; specifies the final year of the time period
  for which data will be downloaded.

- spatial_units:

  character vector, indicating at least one spatial entity for which the
  projection will be run. Typically a canton, several districts or
  municipalities.

## Value

A data frame. For each of the four demographic groups (female / male,
Swiss / foreign nationals), there are 101 age classes, resulting in a
total of 404 rows per requested year and spatial unit. Columns included
in the returned data frame:

- year:

  numeric, year in which the population was recorded.

- spatial_unit:

  character, indicating the spatial entities (e.g., cantons, districts,
  municipalities).

- nat:

  character, `ch` = Swiss, `int` = foreign / international.

- sex:

  character `f` = female, `m` = male.

- age:

  numeric, 101 one-year age classes, ranging from `0` to `100`
  (including those older than 100).

- n:

  numeric, number of people per year, spatial entity, and demographic
  group.

## Examples

``` r
if (FALSE) { # (Sys.getenv("RUN_EXPENSIVE_TESTS") == "true")
if (FALSE) { # \dontrun{
get_population(
  number_fso = "px-x-0102010000_101",
  year = 2020,
  year_last = 2023,
  spatial_units = "- Aargau"
)
get_population(
  year = 2023,
  spatial_units = c("- Aargau", "......0301 Aarberg")
)
} # }
}
```
