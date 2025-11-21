# Project population development (raw results)

Core function that uses the cohort component method and matrix algebra
to project population development (for more details, see
[here](https://www.ag.ch/media/kanton-aargau/dfr/dokumente/statistik/statistische-daten/oeffentliche-statistik/01-bevoelkerung/kantonsdaten/bevoelkerungsprognosen/bev-lkerungsprojektion-technischerbegleitbericht-2025.pdf)).
The function can be used for different spatial levels (e.g., cantons,
municipalities) and for one scenario at a time.

This function provides projections in a **raw** version in which key
information is missing (e.g., which age groups the rows represent). To
conveniently obtain an enriched, more informative output, use the
**wrapper function**
[`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
(which internally uses `propop::project_raw()`).

The parameters and starting populations for different spatial levels can
be obtained from the Swiss Federal Statistical Office (FSO). For
instructions on how to download this information from
[STAT-TAB](https://www.bfs.admin.ch/bfs/en/home/services/recherche/stat-tab-online-data-search.html),
see
[`vignette("prepare_data", package = "propop")`](https://statistik-aargau.github.io/propop/articles/prepare_data.md).

The projection parameters need to be passed on as a single data frame to
`project_raw` with (with the parameters as columns). The column types,
names, and factor levels need to match those specified below.

The 'cohort component method' is implemented with matrices to enhance
performance and enable efficient code execution. In a nutshell, the
starting population ('n') is multiplied by the survival rate to obtain
the people which transition into the projected next year (year + 1).
Then, the absolute number of people immigrating from other cantons and
other countries is added to the "surviving" population. This results in
the starting population for projecting the next year. Newborn children
are added separately to the new starting population of each year.

The starting population is clustered in 404 groups: 101 age groups times
two nationalities times 2 genders. The survival rate is calculated in
the function 'create_transition_matrix()' resulting in the matrix 'L'.
We use the rates for mortality, emigration towards countries outside
Switzerland, emigration to other cantons and the rate for the
acquisition of the Swiss citizenship by the foreign population to
calculate survival rates.

For the optional distribution of the population between subregions
within the canton, we add the absolute migration balance (German =
'saldo') (immigration + emigration) afterwards.

Steps in this function:

1.  Checks: Checking input data and parameter settings for correct
    formats.

2.  Data preparation: Preparing vectors, for example, for the projection
    time frame and creation of empty vectors to be filled with data
    later on.

3.  Loop over years for calculating the projections

    - Subsetting parameters: Depending on the selected projection year
      and on the demographic unit, the parameters for mortality,
      fertility, acquisition of Swiss citizenship as well as migration
      parameters are subset by demographic group.

    - Create matrices: Matrices are built for the survival rate,
      mortality, fertility and for calculating the number of newborn
      babies.

    - Creating vectors: Vectors are built for mortality and migration
      parameters.

    - Projection: The transition matrix 'L' is multiplied by the
      starting population for the next year. Migrating people are added
      in absolute numbers. People that are 100 years old and older are
      clustered into one age group (age = 100+). The newborn babies are
      added to the resulting starting population for the next projection
      year taking into account their survival rates.

4.  Aggregating the data: All projected years are aggregated into one
    data frame. The function 'propop()', in which this function is
    contained, automatically adds relevant meta data to the results.

## Usage

``` r
project_raw(
  parameters,
  year_first,
  year_last,
  age_groups = 101,
  fert_first = 16,
  fert_last = 50,
  share_born_female = 100/205,
  n,
  subregional
)
```

## Arguments

- parameters:

  data frame containing the FSO rates and numbers to run the projection
  for a specific spatial level (e.g., canton, municipality).

  - `year`: projection year.

  - `spatial_unit`: ID of spatial entity (e.g., canton, municipality)
    for which to run the projections.

  - `scen`: projection scenario, used to subset data frames with
    multiple scenarios (r = reference, l = low growth, h = high growth
    scenario).

  - `nat`: nationality (ch = Swiss; int = foreign / international).

  - `sex`: sex (f = female, m = male).

  - `age`: age classes; typically ranging from 0 to 100 (incl. \>100).

  - `birthrate`: numeric, total number of live human births per 1,000
    inhabitants.

  - `int_mothers` proportion of children with Swiss nationality born to
    non-Swiss mothers.

  - `mor`: prospective mortality rate (probability of death).

  - `acq`: rate of acquisition of Swiss citizenship.

  - `emi_int`: rate of people emigrating abroad.

  - `emi_nat`: rate of people emigrating to other cantons.

  - `imm_int_n`: number of people immigrating from abroad.

  - `imm_nat_n`: number of people immigrating from other cantons.

  - `mig_sub`: within canton net migration. Useful to account for
    movements between different subregions (e.g., municipalities). This
    argument is **optional.**

- year_first:

  numeric, first year to be projected.

- year_last:

  numeric, last year to be projected.

- age_groups:

  numeric, number of age classes. Creates a vector with 1-year age
  classes running from `0` to (`age_groups` - 1). Defaults to `101` (FSO
  standard number of age groups).

- fert_first:

  numeric, first year of female fertility. Defaults to 16 (FSO standard
  value).

- fert_last:

  numeric, last year of female fertility. Defaults to 50 (FSO standard
  value).

- share_born_female:

  numeric, fraction of female babies. Defaults to 100 / 205 (FSO
  standard value).

- n:

  number of people per demographic group and year; should be the year
  before `year_first`. Typically extracted from data frame created with
  [`propop::get_population()`](https://statistik-aargau.github.io/propop/reference/get_population.md).

- subregional:

  boolean, TRUE indicates that subregional migration patterns (e.g.,
  movement between municipalities within a canton) are part of the
  projection.

## Value

Returns an unformatted and unlabeled data frame. It includes the number
of people for each demographic group per year (starting year and
projected years. The number of rows corresponds to the product of years
and demographic groups (e.g., nationality (2) X sex (2) X age groups
(101) = 404). Variables included in the output:

- scen:

  projected scenarios.

- spatial_unit:

  spatial unit for which the projection was run (e.g., canton, district,
  municipality).

- N:

  number of people per demographic group.

- IMM_INT:

  number of immigrants from other countries.

- MIG_NAT:

  number of people migrating from / to other superordinate spatial units
  (typically cantons).

- MIG_SUB:

  number of migrants within the superordinate spatial unit (typically a
  canton).

- MOR:

  number of deaths (among people older than 0).

- EMI_INT:

  number of emigrants to other countries.

- ACQ:

  number of foreigners who acquire Swiss citizenship (naturalisations).

- BIRTHS:

  number of births.

## See also

[`propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)

## Examples

``` r
# load package data
data(fso_parameters)
data(fso_population)

# run projection
project_raw(
  parameters = fso_parameters |>
 dplyr::filter(scen == "reference"),
  year_first = 2025,
  year_last = 2026,
  n = fso_population |> dplyr::pull(n),
  subregional = FALSE
) |>
  head(10)
#> Running projection for: Aargau (Scenario: reference)
#> ✔ Year: 2025
#> ✔ Year: 2026
#>         scen spatial_unit    N   BIRTHS       MOR   EMI_INT  EMI_NAT IMM_INT
#> 1  reference       Aargau 2371 2467.882 8.0133597  4.007841 22.04313       5
#> 2  reference       Aargau 2542    0.000 0.9568541 10.513014 48.73828      17
#> 3  reference       Aargau 2891    0.000 1.0356512 11.403412 55.98247      16
#> 4  reference       Aargau 2766    0.000 1.0947324 12.254949 54.58786      16
#> 5  reference       Aargau 2794    0.000 0.0000000 11.310174 44.29472      15
#> 6  reference       Aargau 2782    0.000 0.0000000  9.963404 38.86175      14
#> 7  reference       Aargau 2787    0.000 0.0000000  9.839934 33.45911      14
#> 8  reference       Aargau 2726    0.000 0.0000000  8.912826 28.72282      13
#> 9  reference       Aargau 2837    0.000 0.0000000  7.744566 24.20143      12
#> 10 reference       Aargau 2866    0.000 0.0000000  8.224463 21.58673      11
#>    IMM_NAT ACQ MIG_SUB
#> 1       51   0       0
#> 2       94   0       0
#> 3       84   0       0
#> 4       73   0       0
#> 5       64   0       0
#> 6       50   0       0
#> 7       41   0       0
#> 8       34   0       0
#> 9       30   0       0
#> 10      26   0       0
```
