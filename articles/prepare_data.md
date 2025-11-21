# Prepare data

## Overview

To run projections with
[propop](https://statistik-aargau.github.io/propop), you need a starting
population and projection parameters. You can either use your own data
or download them from the Federal Statistical Office (FSO). This
vignette explains how to get the data from FSO. You’ll also learn how to
prepare the relevant information to run population projections
[propop](https://statistik-aargau.github.io/propop).

*Note that some data are only available for certain administrative
levels.*

## Required data

If you don’t have the information and data required to run
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
(or
[`propop::project_raw()`](https://statistik-aargau.github.io/propop/reference/project_raw.md)),
you can download most of the data from
[STAT-TAB](https://www.pxweb.bfs.admin.ch/pxweb). More specifically, the
information from the following tables are needed:

[TABLE]

Overview of required FSO tables (STAT-TAB)

## Convenient way to get FSO data

[propop](https://statistik-aargau.github.io/propop) provides two
convenience functions to download data from the FSO. These are strongly
based on the `BFS` package and its documentation.

To get the **starting population** for a spatial unit, you must use the
spelling defined in the corresponding FSO table. The entries in the FSO
tables may contain special characters. The spelling may also vary
between FSO tables.

[`BFS::bfs_get_metadata()`](https://felixluginbuhl.com/BFS/reference/bfs_get_metadata.html)
is helpful to identify the required spelling(s).

Here’s an example of how to get the population for the canton of Aargau:

``` r
library(propop) 
ag_population <- get_population(
  number_fso = "px-x-0102010000_101",
  year = 2023, 
  spatial_units = "- Aargau"
)
```

Get the parameters for a sample canton (mind using the same spelling as
in the FSO tables; see comment above):

``` r
ag_parameters <- get_parameters(
  year = 2024,
  year_last = 2026,
  spatial_units = "Aargau"
)
```

The projection can be run as follows:

``` r
# select reference scenario
ag_parameters_ref <- ag_parameters |>
  dplyr::filter(scen == "reference")

propop(
  parameters = ag_parameters_ref,
  year = 2024,
  year_last = 2026,
  age_groups = 101,
  fert_first = 16,
  fert_last = 50,
  share_born_female = 100 / 205,
  population = ag_population,
  subregional = FALSE,
  binational = TRUE
)
```

**Note of caution:** As long as the FSO’s API interface and the
underlying data structure remain stable, the functions will work.
However, changes in the API are likely to break the functions.
