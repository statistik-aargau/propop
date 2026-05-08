# Package index

## Prepare data

Functions for downloading and preparing data

- [`get_parameters()`](https://statistik-aargau.github.io/propop/reference/get_parameters.md)
  : Get projection parameters from FSO
- [`get_population()`](https://statistik-aargau.github.io/propop/reference/get_population.md)
  : Get population data from FSO
- [`calculate_rate()`](https://statistik-aargau.github.io/propop/reference/calculate_rate.md)
  : Get mean historical subregional emigration to municipalities within
  the canton

## Run projections

Functions for running population projections

- [`propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
  : Project population development

## Evaluate projections

Functions to evaluate projection results

- [`check_balance()`](https://statistik-aargau.github.io/propop/reference/check_balance.md)
  : Check if population equation components add up within one year
- [`prepare_evaluation()`](https://statistik-aargau.github.io/propop/reference/prepare_evaluation.md)
  : Prepare data for evaluation
- [`compute_measures()`](https://statistik-aargau.github.io/propop/reference/compute_measures.md)
  : Compute evaluation measures
- [`aggregate_measures()`](https://statistik-aargau.github.io/propop/reference/aggregate_measures.md)
  : Aggregate evaluation measures

## Distribute migration

Example for distributing international immigration

- [`calculate_shares()`](https://statistik-aargau.github.io/propop/reference/calculate_shares.md)
  : Calculate shares for distributing immigration among subregions
- [`calculate_rate()`](https://statistik-aargau.github.io/propop/reference/calculate_rate.md)
  : Get mean historical subregional emigration to municipalities within
  the canton
- [`ag_population_subregional`](https://statistik-aargau.github.io/propop/reference/ag_population_subregional.md)
  : Population data for the canton of Aargau (five subregions)
- [`ag_migration_subregional`](https://statistik-aargau.github.io/propop/reference/ag_migration_subregional.md)
  : Subregional migration records for the canton of Aargau (five
  subregions)

## Sample data

Sample data from the Federal Statistical Office

- [`fso_parameters`](https://statistik-aargau.github.io/propop/reference/fso_parameters.md)
  : Sample parameters to run population projection
- [`fso_population`](https://statistik-aargau.github.io/propop/reference/fso_population.md)
  : Sample population data from the Federal Statistical Office

## Deprecated functions

Functions are still operational but won’t be further maintained

- [`propop_legacy()`](https://statistik-aargau.github.io/propop/reference/propop_legacy.md)
  : Project population development
- [`project_raw()`](https://statistik-aargau.github.io/propop/reference/project_raw.md)
  : Project population development (raw results)
