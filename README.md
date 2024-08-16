
# Welcome to the `propop` package <a href="https://statistik-aargau.github.io/propop/"><img src="man/figures/logo.png" align="right" width = "134" /></a>

<!-- badges: start -->

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build status](https://github.com/statistik-aargau/propop/workflows/R-CMD-check/badge.svg)](https://github.com/statistik-aargau/propop/actions)
<!-- badges: end -->


## Overview 

The goal of `propop` is to create population projections using the **cohort 
component method** (see e.g., 
[Prestom, Guillot, and Heuveline, 2000](https://www.wiley.com/en-us/Demography%3A+Measuring+and+Modeling+Population+Processes-p-9781557864512); [Statistik Aargau, 2020a](https://www.ag.ch/media/kanton-aargau/dfr/dokumente/statistik/statistische-daten/oeffentliche-statistik/01-bevoelkerung/kantonsdaten/bevoelkerungsprognosen/bevoelkerungsprojektionen-2020-technischer-begleitbericht.pdf); 
[Statistik Aargau, 2020b](https://www.ag.ch/media/kanton-aargau/dfr/dokumente/statistik/statistische-daten/oeffentliche-statistik/01-bevoelkerung/kantonsdaten/bevoelkerungsprognosen/bevoelkerungsprojektionen-2020-hauptbericht-v2.pdf)). 

The package was developed for use with 
[population data](https://www.pxweb.bfs.admin.ch/pxweb) 
and [projection scenarios](https://www.bfs.admin.ch/bfs/en/home/statistics/population/population-projections/national-projections.html) 
from the Swiss Federal Statistical Office (FSO). `propop` uses the same matrix 
calculation method as the FSO and therefore reveals almost identical results 
(see vignette 
[Evaluate projections](https://statistik-aargau.github.io/propop/articles/evaluate.html)). 
The package can be used to project the development of different 
**demographic groups** for different **scenarios** and at different 
**spatial levels** (e.g., single canton or municipalities within a canton). 

Although the package was developed for use in Switzerland, it should in principle 
also work for other contexts, provided the required input data is available in 
the 
[specified form](https://statistik-aargau.github.io/propop/articles/prepare_data.html).

## Package website

For a detailed documentation, visit the 
[package website](https://statistik-aargau.github.io/propop/).

## Installation

To install the current github version of the package, make sure you have devtools
installed and type:

``` r
devtools::install_github("statistik-aargau/propop")
```

## Vignettes

The package currently includes three vignettes. The first vignette demonstrates 
[how to download and prepare](https://statistik-aargau.github.io/propop/articles/prepare_data.html) 
the FSO population data and parameters. The second vignette illustrates in more detail 
[how to project](https://statistik-aargau.github.io/propop/articles/run_projections.html) 
the population development for single and multiple regions. The third vignette shows 
[how to evaluate projections](https://statistik-aargau.github.io/propop/articles/evaluate.html).

## Features, limitations, future plans

- Currently, `propop::propop()` only works with *1-year age classes* starting 
at age zero and including all fertile years of females.  
- The FSO uses additional *ex-post* adjustments to ensure that all the sums 
involving different cantons add up, which leads to some differences
between the projections from the FSO and `propop`. Moreover, the FSO projections
are only published in five-year intervals, the most recent ones being based on 
population records from 2018. `propop` enables you to run projections with more
recent population records.
- This package was developed for use with *FSO parameters*. Most parameters are only 
available for cantons and the whole of Switzerland. If you wish to run 
projections at smaller scales, you need to prepare the parameters for each 
spatial entity before running the projection. Similarly, you may wish to adjust 
the parameters to account for regional differences. In both cases you must pass 
the updated parameters through the `parameters`object to `propop::propop()`.
- `propop::propop()`offers the possibility to account for varying 
subregional migration patterns and migration between subregions. However, before
using this feature, users must adjust or calculate the required parameters. 
- There are also *possible future features* that could increase the accuracy of 
the projections. For example, considering the attractiveness of municipalities 
or planned housing developments could help to better explain why population 
growth varies between municipalities.  
- A limitation of `propop::propop()` is that only the final number of projected 
people per demographic group (`n`) is returned. It is currently not possible to 
obtain the individual components (e.g., births, deaths, immigration, emigration)
of the population equation. 

**If you are interested in contributing to these or other developments, please
get in touch with the package maintainer.**

## Example

To run `propop::propop()` with the example data included in the package 
(canton of Aargau) for the period 2019-2030, use the following code:

``` r
library(propop)
projection_canton_2030 <- propop(
  parameters = fso_parameters,
  year_first = 2019,
  year_last = 2030,
  population = fso_population,
  subregional = FALSE)
```
