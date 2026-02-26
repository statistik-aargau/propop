# propop 2.0.0
## Breaking changes
- New function syntax for `propop()` to run population projections with data frames
instead of matrices, using identical data input. The function also comes with a new feature: distributing
subregional migration can be calculated using rates in addition to using net numbers. 

- Projection output: Components are neatly arranged which enables calculating
the population balance ad hoc. Metrics for annual population change per demographic 
group in absolute numbers (`delta_n`) and as percentages (`delta_perc`) are automatically
caluclated and appear as columns in the output of `propop()`. Components were renamed 
with the suffix `_n`; this helps to distinguish between numberof people (e.g. `emi_nat_n`) 
and input parameters (e.g. `emi_int`).  

- New helper functions for `propop()`: `project_population()` is a wrapper function,
`advance_population()` ages the population by one year, `calculate_projection()`
fosters the transition of the cohort to the next year and `calculate_newborns()`
computes births.

- New tests for `propop()` and helper functions as well as for comparing the output 
between `propop()` and `propop_legacy()`, sensitive to differences larger than three 
people in total by year 2055 for the Canton of Aargau.

## Deprecated
- The old function `propop()` was renamed to `propop_legacy()` and was deprecated 
together with `complement_projection()`, `create_transition_matrix()`,
`create_birth_matrix()`, `create_fertility_matrix()`, `create_mortality_matrix()`,
`create_empty_vector()` and `project_raw()`. `propop_legacy()` and associated
helper function will stay operational.

# propop 1.4.1
- maintenance work; no breaking changes

# propop 1.4.0
- `propop::propop()` can now perform multiple projection scenarios in a 
single run. 
- New function `check_balance` to check population equation for each row in 
the output of `propop::propop()` (population at the beginning of the year 
plus components must be equal to the population at the end of the year).
- The function `prepare_evaluation` has an additional set of age groups (0-19,
20-39, 40-59, 60-79, 80 years and older). The output that is created by this 
function can be used by `compute_measures` and `aggregate_measures` 
to evaluate projection models against a benchmark. 

# propop 1.3.0
- Uses new scenarios published by the FSO in April 2025
- Updated functions, package data, and documentation 
- Added "scenario" to output in console

# propop 1.2.2
- Projections enabled beyond 2018-2050.

# propop 1.2.1
- `get_population`: Using clearer, more intuitive name for argument to specify
start year; less typing for users; 
- revised function documentation
- More detailed feedback after running propop::propop(), stating which arguments 
were used)

# propop 1.2.0

- Added function `calculate_shares` to distribute FSO estimates expressed as 
"number of people" among sub-cantonal entitities (e.g., immigration to 
municipalities). 

# propop 1.1.0

- Added unit test that fails if projection outcomes differ from FSO projections
- Increased flexibility of `prepare_evaluation()` by offering possibility to 
indicate which column contains the population (rather than assuming it's 
called `n`). 

# propop 1.0.0

## Projection using separate parameters for migration between cantons

- `propop::propop()` now uses *separate* input parameters for emigration to / 
immigration from other cantons. Previously `propop::propop()` used the net 
difference between emigration to / immigration from other cantons. 
- With this change, the new version now directly corresponds to the projection 
equations provided by the FSO.
- The change has also facilated the inclusion of all components of the cohort
component method in the output. This makes it easy to track the transition from 
one year to the next.

## New variable names

- This change required a comprehensive renaming of several variables, which makes
the new version incompatible with previous code. 
- If you want to continue using 
old code, we suggest using version 0.2.0. Note that the components of the 
projection equation are not included in the output in version 0.2.0.

# propop 0.2.0

## Projection without distinction of nationalities

- If desired, projections can be run for only one nationality (no longer
required to provide two nationalities).   
- `propop::propop()` returns a plain text feedback in the console summarizing 
the settings used to run the projection.  

# propop 0.1.0

## Development history and earlier versions

The `propop` package is a successor of `staagBevProj` (not publicly available), 
which was last updated in December 2022. `staagBevProj` was developed by 
Tina Cornioley, Jan Wunder, and Niklas Haffert. As head of [Official Statistics 
Aargau](https://www.ag.ch/de/verwaltung/dfr/statistik), Andrea Plüss supported 
the devlopment as well as the 
publication of `staagBevProj` and `propop`.

`propop` was developed by Norah Efosa and Adrian Gadient 
([Statistik Aargau](https://www.ag.ch/de/verwaltung/dfr/statistik)). 
The main innovations are:

- Clearer separation between code that prepares data and code that runs the 
projection
-	Centrally united FSO parameters and corrections / adjustments; these are now 
gathered in a single data frame that is passed to the projection function
-	Only one function to run projections (rather than `projectPop` and 
`projectAll`)
-	Convenience functions to directly download data and parameters from STAT-TAB
-	Incorporation of assertions and unit tests
-	Function to evaluate projection results 
-	Clearer documentation
