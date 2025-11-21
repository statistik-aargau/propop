# Get projection parameters from FSO

Users who do not have the mandatory projection parameters for
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
can use this convenience function to download them from the Federal
Statistical Office (FSO). The parameters are only available on the level
of cantons. For smaller-scale projections, the parameters must be scaled
down. In addition to the parameters, the function also returns the
projected population (i.e., number of people estimated in the FSO model
released in 2025). All parameters and projections are from the [FSO
model published in
2025](https://www.bfs.admin.ch/bfs/en/home/statistics/population/population-projections/national-projections.html).
The parameters are available for the years 2024-2055. The variables
`int_mothers` and `mig_nat_n` are not directly available from the FSO.
They are calculated within this function.

To get projection parameters, you must use the spelling defined in the
corresponding FSO table. See
[`vignette("prepare_data", package = "propop")`](https://statistik-aargau.github.io/propop/articles/prepare_data.md).
Inspecting the column 'valueTexts' in the following package data may
also help:

- data('stattab_101_snap')

- data('stattab_102_snap')

- data('stattab_103_snap')

- data('stattab_106_snap')

- data('stattab_109_snap')

Changes to the API interface may break this function. If problems occur,
we recommend following the step-by-step procedure described in
[`vignette("prepare_data", package = "propop")`](https://statistik-aargau.github.io/propop/articles/prepare_data.md).

## Usage

``` r
get_parameters(
  number_fso_ref = "px-x-0104020000_101",
  number_fso_high = "px-x-0104020000_102",
  number_fso_low = "px-x-0104020000_103",
  number_fso_rates = "px-x-0104020000_109",
  number_fso_births = "px-x-0104020000_106",
  year_first,
  year_last,
  spatial_units
)
```

## Source

Data obtained from the Swiss Federal Statistical Office (FSO):

- <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_101/-/px-x-0104020000_101.px/>

- <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_102/-/px-x-0104020000_102.px>

- <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_103/-/px-x-0104020000_103.px/>

- <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_106/-/px-x-0104020000_106.px/>

- <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_109/-/px-x-0104020000_109.px/>

## Arguments

- number_fso_ref:

  character, px-x table ID for number parameters (reference scenario),
  defaults to "px-x-0104020000_101".

- number_fso_high:

  character, px-x table ID for number parameters (high growth scenario),
  defaults to "px-x-0104020000_102".

- number_fso_low:

  character, px-x table ID for for number parameters (low growth
  scenario, defaults to "px-x-0104020000_103".

- number_fso_rates:

  character, px-x table ID for rate parameters, defaults to
  "px-x-0104020000_109".

- number_fso_births:

  character, px-x table ID required to compute the share of Swiss
  newborns from non-Swiss mothers, defaults to "px-x-0104020000_106".

- year_first:

  numeric, first year for which the parameters and projections are to be
  downloaded.

- year_last:

  numeric, last year for which the parameters and projections are to be
  downloaded.

- spatial_units:

  character vector, indicating at least one spatial entity for which the
  projection will be run. Typically a canton.

## Value

A data frame with the rates and number of people from the [Federal
Statistical Office (FSO)](https://www.bfs.admin.ch/bfs/en/home.html)
required to project the population development of the requested spatial
entities. For each of the four demographic groups (nationality x sex),
there are 101 age classes, resulting in a total of 404 rows per
requested year and spatial unit.

## Demographic groups

The returned data frame includes parameters for each unique combination
of the following demographic variables:

- `nat`: ch = Swiss; int = foreign / international.

- `sex`: f = female, m = male.

- `age`: 101 one-year age classes, ranging from 0 to 100 (including
  those older than 100).

## Parameters

The following parameters are included in the returned data frame:

- `start_n`: numeric, number of people in the corresponding demographic
  group on 1st of January.

- `year`: numeric, year of projection.

- `scen`: character, projection scenario.

- `spatial_unit`: character, indicating the user requested spatial
  unit(s).

- `birthrate`: numeric, total number of live human births per 1,000
  inhabitants. (formerly `birth_rate`).

- `int_mothers`: numeric, proportion of children with Swiss nationality
  born to non-Swiss mothers (formerly `births_int_ch`).

- `mor`: numeric, prospective mortality (probability of death).

- `emi_int`: numeric, rate of people emigrating to other countries
  (formerly `emi`).

- `emi_nat`: numeric, rate of people emigrating to other cantons (new
  parameter).

- `acq`: numeric, rate of acquisition of Swiss citizenship.

- `imm_int_n`: numeric, number of people immigrating from abroad
  (formelry `imm_int`).

- `imm_nat_n`: numeric, number of people immigrating from other cantons
  (new parameter).

- `emi_nat_n`: numeric, number of people emigrating to other cantons
  (parameter previously used to compute `mig_nat_n`).

- `mig_nat_n`: numeric, national / inter-cantonal net migration (number
  of immigrants minus number of emigrants). (formerly `mig_ch`, will
  soon be obsolete and removed).

## Projected population

`n_projected` is the number of people per demographic group and year on
December 31 (as projected by the FSO in the 2025 model).

## Details about calculated variables

`births_int_ch` is calculated by dividing the number of live newborns
with Swiss citizenship born to non-Swiss mothers by the number of all
live newborns born to non-Swiss mothers.

`mig_ch` is calculated as the difference between the immigration from
other cantons and the emigration to other cantons.

## Examples

``` r
if (FALSE) { # (Sys.getenv("RUN_EXPENSIVE_TESTS") == "true")
if (FALSE) { # \dontrun{
one_canton <- get_parameters(
  year_first = 2024,
  year_last = 2055,
  spatial_units = "Aargau"
)
two_cantons_4years <- get_parameters(
  year_first = 2025,
  year_last = 2028,
  spatial_units = c("Aargau", "Zug")
)
} # }
}
```
