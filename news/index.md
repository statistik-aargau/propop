# Changelog

## propop 2.0.0

### Breaking changes

- The computations of the key function
  [`propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
  are now implemented using data frames and `purrr` / `dplyr` rather
  than base `R` matrices. This change makes the computational steps and
  intermediate results more accessible and easier to follow. Moreover,
  the new tabular approach facilitates the integration of new features.
  On the downside, the new approach takes approximately 25% longer to
  run. The data input remains unchanged.

- While implementing this alternative computation we fixed a minor bug
  that had a tiny effect on the oldest age group. The function now
  offers a new feature: subregional migration can be calculated using
  rates in addition to using net numbers.

- Projection output: The components now include the suffix `_n`, which
  more clearly  
  distinguishes *input parameters* (e.g. `emi_int`) from *number of
  people* (e.g. `emi_nat_n`).

- New helper functions for
  [`propop()`](https://statistik-aargau.github.io/propop/reference/propop.md):
  `project_population()` is a wrapper function; `advance_population()`
  ages the population by one year; `calculate_projection()` advances the
  cohort to the next year; `calculate_newborns()` computes births.

- New tests have been added for
  [`propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
  and helper functions, as well as for comparing the output of
  [`propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
  and
  [`propop_legacy()`](https://statistik-aargau.github.io/propop/reference/propop_legacy.md).
  The comparison is sensitive to differences larger than three people in
  total by year 2055 (using data for the Canton of Aargau).

### Deprecated

- The old function
  [`propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
  was renamed to
  [`propop_legacy()`](https://statistik-aargau.github.io/propop/reference/propop_legacy.md)
  and was deprecated together with `complement_projection()`,
  `create_transition_matrix()`, `create_birth_matrix()`,
  `create_fertility_matrix()`, `create_mortality_matrix()`,
  `create_empty_vector()` and
  [`project_raw()`](https://statistik-aargau.github.io/propop/reference/project_raw.md).
  [`propop_legacy()`](https://statistik-aargau.github.io/propop/reference/propop_legacy.md)
  and associated helper function will stay operational.

## propop 1.4.1

- maintenance work; no breaking changes

## propop 1.4.0

- [`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
  can now perform multiple projection scenarios in a single run.
- New function `check_balance` to check population equation for each row
  in the output of
  [`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
  (population at the beginning of the year plus components must be equal
  to the population at the end of the year).
- The function `prepare_evaluation` has an additional set of age groups
  (0-19, 20-39, 40-59, 60-79, 80 years and older). The output that is
  created by this function can be used by `compute_measures` and
  `aggregate_measures` to evaluate projection models against a
  benchmark.

## propop 1.3.0

- Uses new scenarios published by the FSO in April 2025
- Updated functions, package data, and documentation
- Added “scenario” to output in console

## propop 1.2.2

- Projections enabled beyond 2018-2050.

## propop 1.2.1

- `get_population`: Using clearer, more intuitive name for argument to
  specify start year; less typing for users;
- revised function documentation
- More detailed feedback after running propop::propop(), stating which
  arguments were used)

## propop 1.2.0

- Added function `calculate_shares` to distribute FSO estimates
  expressed as “number of people” among sub-cantonal entitities (e.g.,
  immigration to municipalities).

## propop 1.1.0

- Added unit test that fails if projection outcomes differ from FSO
  projections
- Increased flexibility of
  [`prepare_evaluation()`](https://statistik-aargau.github.io/propop/reference/prepare_evaluation.md)
  by offering possibility to indicate which column contains the
  population (rather than assuming it’s called `n`).

## propop 1.0.0

### Projection using separate parameters for migration between cantons

- [`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
  now uses *separate* input parameters for emigration to / immigration
  from other cantons. Previously
  [`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
  used the net difference between emigration to / immigration from other
  cantons.
- With this change, the new version now directly corresponds to the
  projection equations provided by the FSO.
- The change has also facilated the inclusion of all components of the
  cohort component method in the output. This makes it easy to track the
  transition from one year to the next.

### New variable names

- This change required a comprehensive renaming of several variables,
  which makes the new version incompatible with previous code.
- If you want to continue using old code, we suggest using version
  0.2.0. Note that the components of the projection equation are not
  included in the output in version 0.2.0.

## propop 0.2.0

### Projection without distinction of nationalities

- If desired, projections can be run for only one nationality (no longer
  required to provide two nationalities).  
- [`propop::propop()`](https://statistik-aargau.github.io/propop/reference/propop.md)
  returns a plain text feedback in the console summarizing the settings
  used to run the projection.

## propop 0.1.0

### Development history and earlier versions

The `propop` package is a successor of `staagBevProj` (not publicly
available), which was last updated in December 2022. `staagBevProj` was
developed by Tina Cornioley, Jan Wunder, and Niklas Haffert. As head of
[Official Statistics
Aargau](https://www.ag.ch/de/verwaltung/dfr/statistik), Andrea Plüss
supported the devlopment as well as the publication of `staagBevProj`
and `propop`.

`propop` was developed by Norah Efosa and Adrian Gadient ([Statistik
Aargau](https://www.ag.ch/de/verwaltung/dfr/statistik)). The main
innovations are:

- Clearer separation between code that prepares data and code that runs
  the projection
- Centrally united FSO parameters and corrections / adjustments; these
  are now gathered in a single data frame that is passed to the
  projection function
- Only one function to run projections (rather than `projectPop` and
  `projectAll`)
- Convenience functions to directly download data and parameters from
  STAT-TAB
- Incorporation of assertions and unit tests
- Function to evaluate projection results
- Clearer documentation
