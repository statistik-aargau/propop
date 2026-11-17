## Submission

Update from 'propop' v2.2.1 to v2.2.2. This update adapts the fertile age range 
for women to 15-49, fixes inactive scenario filtering in `propop()`, and removes 
the outdated `start_n` column from the parameters.

## Test environments
* local: Ubuntu 24, R 4.5.3
* GitHub Actions: ubuntu-latest (release, devel), macOS-latest (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 0 note

* Resolved failure of `test-prepare_evaluation.R` by rounding results to five digits.
