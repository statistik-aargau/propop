# Sample population data from the Federal Statistical Office

Example data frame containing the starting population required to
project the development of four demographic groups for a selected canton
(Aargau). Population data can be downloaded from the FSO using the
`get_population`.

At the time of the release of `{propop}` v1.3 in May 2025, the most
recent data available via `get_population` are from 2023.

## Usage

``` r
fso_population
```

## Format

The example population records include the number of people of each
demographic group (nationality (2) X sex (2) X age classes (101)) for
the canton of Aargau in 2023.

## Source

Federal Statistical Office:
<https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0102010000_101/-/px-x-0102010000_101.px/>

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
