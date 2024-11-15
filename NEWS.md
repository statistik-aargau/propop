# propop 0.2.0

## Projection without distinction of nationalities

It is no longer required to provide two nationalities. Projections can now 
be run without distinguishing between two nationalities.

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
