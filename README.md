
# Welcome to the `propop` package <a href="https://statistik-aargau.github.io/propop/"><img src="man/figures/logo.png" align="right" width = "134" /></a>

<!-- badges: start -->

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build status](https://github.com/statistik-aargau/propop/workflows/R-CMD-check/badge.svg)](https://github.com/statistik-aargau/propop/actions)
<!-- badges: end -->


## Overview 

The goal of `propop` is to create population projections using the **cohort 
component method** (see e.g., 
[Preston, Guillot, and Heuveline, 2000](https://www.wiley.com/en-us/Demography%3A+Measuring+and+Modeling+Population+Processes-p-9781557864512); 
[Statistik Aargau, 2025a](https://www.ag.ch/media/kanton-aargau/dfr/dokumente/statistik/statistische-daten/oeffentliche-statistik/01-bevoelkerung/kantonsdaten/bevoelkerungsprognosen/bev-lkerungsprojektion-technischerbegleitbericht-2025.pdf); 
[Statistik Aargau, 2025b](https://www.ag.ch/media/kanton-aargau/dfr/dokumente/statistik/statistische-daten/oeffentliche-statistik/01-bevoelkerung/kantonsdaten/bevoelkerungsprognosen/bev-lkerungsprohektion-hauptbericht-2025.pdf)). 
For a breakdown of the components, see [this vignette](https://statistik-aargau.github.io/propop/articles/project_single_region.html).

The package was developed for use with [projection scenarios](https://www.bfs.admin.ch/bfs/en/home/statistics/population/population-projections/national-projections.html) 
from the Swiss Federal Statistical Office (FSO). The current scenarios are 
available for the years 2024-2055. As starting population, you can either use
[population data from the FSO](https://www.pxweb.bfs.admin.ch/pxweb) or your own
population data.

`propop` uses the same calculation method as the FSO 
([2020](https://github.com/statistik-aargau/propop-additional-resources/blob/358ffa280f3777af34d3ac4b2782c1171ed93beb/FSO_2020_Meth_scenarios%20cant.pdf); only available in French) 
and therefore reveals almost identical results (see vignette 
[Evaluate projections](https://statistik-aargau.github.io/propop/articles/evaluate.html)). 
The package can be used to project the development of different 
**demographic groups** for different **scenarios** and at different 
**spatial levels** (e.g., single canton or municipalities within a canton). 

While the package was primarily designed for use with FSO input data (e.g., 
mortality rates), it should in principle also work for other contexts (e.g., 
custom input data). However, it is important to provide the required input data 
in the 
[specified form](https://statistik-aargau.github.io/propop/articles/prepare_data.html).


## Installation

To install the current github version of the package, make sure you have devtools
installed and type:

``` r
devtools::install_github("statistik-aargau/propop")
```

## Vignettes

The package includes four vignettes.  

- Vignette 1 shows how to 
[prepare](https://statistik-aargau.github.io/propop/articles/prepare_data.html) 
the FSO population data and parameters. (Users can of course use their own data 
and parameters).
- Vignette 2 illustrates how to run 
[population projections for a single region](https://statistik-aargau.github.io/propop/articles/project_single_region.html). 
- Vignette 3 illustrates how to pepare and run 
[population projections for subregions](https://statistik-aargau.github.io/propop/articles/project_subregions.html).
- Vignette 4 shows 
[how to evaluate projections](https://statistik-aargau.github.io/propop/articles/evaluate.html) against benchmarks.

## Features 

- `propop::propop()` works with either two nationalities (usually Swiss vs. 
non-Swiss nationals) or without distinguishing between nationalities.  
- The FSO projections are only published in five-year intervals, the most recent 
ones being based on population records from 2023. `propop` enables you to run 
projections with more recent population records.

## Limitations 

- `propop::propop()` currently requires *1-year age classes* (0-100+) 
and two genders.    
- This package was developed for use with *FSO parameters* (e.g., mortality rate,
or emigration rate). Most parameters are only available for cantons and the 
whole of Switzerland. If you wish to run projections at smaller scales (e.g., 
districts), you need to prepare the parameters for each spatial entity before 
running the projection. 
[This vignette](https://statistik-aargau.github.io/propop/articles/project_subregions.html)
includes tips of how to prepare your input data.  
- Similarly, if you wish to adjust parameters (e.g., mortality rates that 
vary between subregions), you need to prepare the parameters accordingly. To 
adjust birth rates, you can use the package 
[`propopbirth`](https://github.com/statistik-aargau/propopbirth).    
- `propop::propop()`offers the possibility to account for varying 
subregional migration patterns and migration between subregions. However, before
using this feature, users must adjust or calculate the required parameters 
([see this vignette](https://statistik-aargau.github.io/propop/articles/project_subregions.html)). 
- Custom parameters (e.g., consideration of sub-cantonal spatial entities, 
adjustments to reflect regional differences, or addition of subregional migration 
parameter) can be passed through the `parameters` object to `propop::propop()`. 
- The FSO uses additional *ex-post* adjustments to ensure that all the sums 
involving different cantons add up, which leads to some differences
between the projections from the FSO and `propop`. 

## Future plans  
 
- There are also *possible future features* that could increase the accuracy of 
the projections. For example, considering the attractiveness of municipalities 
or planned housing developments could help to better explain why population 
growth varies between municipalities.  

**If you are interested in contributing to these or other developments, please
get in touch with the package maintainer.**

## Quick example

To run `propop::propop()` with the example data included in the package 
(canton of Aargau), use the following code:

``` r
library(propop)
projection_canton_2030 <- propop(
parameters = fso_parameters,
year_first = 2024,
year_last = 2030,
population = fso_population,
subregional = FALSE,
binational = TRUE)
```
