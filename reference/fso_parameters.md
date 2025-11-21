# Sample parameters to run population projection

Data frame containing the rates and number of people from the [Federal
Statistical Office (FSO)](https://www.bfs.admin.ch/bfs/en/home.html)
required to project the development of four demographic groups for a
selected canton (Aargau). The parameters are from the [model published
in
2025](https://www.bfs.admin.ch/bfs/en/home/statistics/population/population-projections/national-projections.html).
The sample data include three scenarios for the years 2024-2055.

## Usage

``` r
fso_parameters
```

## Format

The example data include the required parameters for each demographic
group (nationality (2) X sex (2) X age classes (101)) for the years
2024-2055.

## Source

Data obtained from the Swiss Federal Statistical Office (FSO):

- <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_101/-/px-x-0104020000_101.px/>

- <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_102/-/px-x-0104020000_102.px>

- <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_103/-/px-x-0104020000_103.px/>

- <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_106/-/px-x-0104020000_106.px/>

- <https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0104020000_109/-/px-x-0104020000_109.px/>

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
