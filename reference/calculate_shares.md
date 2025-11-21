# Calculate shares for distributing people among subregions

Calculate shares for distributing people among subregions

## Usage

``` r
calculate_shares(data, col, age_group = "default")
```

## Arguments

- data:

  data frame, historical records (e.g., immigration from other cantons
  or countries) aggregated across demographic groups.

- col:

  character, name of the column which contains the data for historical
  occurrences.

- age_group:

  character **(optional)**, either 1-year, 5-year, or 10-year age group
  used as basis for calculating shares. If the argument is not
  specified, the default attempts to avoid age groups without any
  observations. It prioritizes age groups based on their resolution
  (1-year age groups = most informative and highest priority, 10-year
  age groups = least informative and lowest priority). Users can
  override the default and enforce the use of a specific age group for
  all demographic groups by setting the argument to "age_group_5" or
  "age_group_10".

## Value

Returns the input data frame with the following new columns:

- `age_group_5`: character, indicates the 5-year age group to which the
  1-year age group is assigned to.

- `age_group_10`: character, indicates the 10-year age group to which
  the 1-year age group is assigned to.

- `sum_5`: numeric, total number of people in the 5-year age group.

- `prop_5`: numeric, proportion of the the 5-year age group total that
  is allocated to each 1-year age group.

- `sum_10`: numeric, total number of people in the 10-year age group.

- `prop_10`: numeric, proportion of the the 10-year age group total that
  is allocated to each 1-year age group.

- `use_age_group`: character, preference for 1-year, 5-year, or 10-year
  age group. Defaults to `age_group_1` if at least one observation was
  recorded in all 5 years belonging to the respective 5-year age groups.

- `n`: numeric, number of people to be used according to `use_age_group`
  to compute the share.

- `n_sum`: numeric, total per demographic group and across all spatial
  units.

- `share`: numeric, the spatial unit's share relative to the total of
  people within the same demographic group (across all spatial units;
  i.e., `n` / `n_sum`).
