# Prepare data for evaluation

Takes population projections and benchmark data (e.g., population
records) and prepares a combined data frame to evaluate the performance
of the projection. For more details on usage, see
[`vignette("evaluate", package = "propop")`](https://statistik-aargau.github.io/propop/articles/evaluate.md).

## Usage

``` r
prepare_evaluation(
  data_benchmark,
  n_benchmark,
  data_projected,
  n_projected,
  age_groups = NULL
)
```

## Arguments

- data_benchmark:

  data frame containing benchmark data (e.g., actual / official
  population records; can be obtained with
  [`propop::get_population()`](https://statistik-aargau.github.io/propop/reference/get_population.md)).

- n_benchmark:

  numeric column containing the benchmark population of each demographic
  group.

- data_projected:

  data frame containing population projections; can be created with
  [`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md).

- n_projected:

  numeric column containing the projected size of each demographic
  group.

- age_groups:

  character, optional argument with options `"age_groups_3"` or
  `"age_groups_5"`. The option `"age_groups_3"` groups the data into
  three age ranges (0-19, 20-64, 65 years and older). The option
  `"age_groups_5"` groups the data into five age ranges (0-19, 20-39,
  40-59, 60-79, 80 years and older). Using aggregated groups will lead
  to smaller projection errors than using 101 age classes. Defaults to
  using 101 one-year age classes if no option is chosen.

## Value

Returns a data frame with the number of people from the benchmark and
from the projection. Each row contains a unique combination of year,
spatial unit, and demographic group.

## Input data and variables

Both input data frames must contain the following variables for the
**same range of years**:

- year:

  character, year in which the population was recorded.

- spatial_unit:

  character, indicating the projected spatial entities (e.g., cantons,
  districts, municipalities).

- nat:

  character, `ch` = Swiss, `int` = foreign / international.

- sex:

  character, `f` = female, `m` = male.

- age:

  numeric, 101 one-year age classes, ranging from 0 to 100 (including
  those older than 100).

- n:

  numeric, number of people per year, spatial entity, and demographic
  group.

## Examples

``` r
if (FALSE) { # \dontrun{
combined <- prepare_evaluation(
  data_benchmark = output_get_population,
  data_projected = output_propop
)
combined_grouped <- prepare_evaluation(
  data_benchmark = output_get_population,
  data_projected = output_propop,
  age_groups = "age_groups_3"
)
} # }
```
