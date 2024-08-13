## What is lambda (ƛ)?
The average distance travelled per timestep (usually one day), given as an absolute magnitude in kilometers.

## The relationship between the average distance travelled per timestep (ƛ) and the effective infectious area (neighbourhood)
The population data, used for the susceptible compartment (less the initial
values of the other compartments), is taken from the 2020 United
Nations-adjusted population count. The raster data has a thirty arc-second
resolutiohn.

The 2020 U.N.-Adjusted Population Count raster at the 30-arc second resolution
consists of hundreds of thousands (for some countries millions) of grid cells. A
summary of the total number of grid cells for several countries are tabulated in
this spreadsheet.

A recommended aggregation factor (expressed in km) is provided in the
spreadsheet however the modeller may modify this to what best suits their needs.
We call this aggregation factor rasterAgg. The user provides a constant ƛ, the
average distance travelled (in km) by an individual in a given cell per
timestep. This constant is viewed as a characteristic of the distance at which
the disease spreads.

If ƛ ≤ rasterAgg we set radius = rasterAgg and set the effective infectious area
(or neighbourhood) as the [Moore
neighbourhood](https://en.wikipedia.org/wiki/Moore_neighborhood). The Moore
neighborhood is composed of nine cells: a central cell and the eight cells which
surround it. Basically this is the same as r = 1.

If ƛ > rasterAgg we set radius = (ƛ + rasterAgg) and set the effective
infectious area (or neighbourhood) as r > 1.

The exact value of r is calculated as r = roundƛ - rasterAgg rasterAgg+eps + 1.

The number of grid cells in the effective area is Ωn = (2r +1)2

#### Examples
For example, when r = 1 we have Ωn = 9 cells.

For example, when r = 2 we have Ωn = 25 cells.

Suppose rasterAgg = 10 (the aggregation factor we used for CZE and ƛ = 15
kms/timestep then we have r = round15 - 10 10 + 1 = 2 with Ωn = 25 cells.

Suppose rasterAgg = 15 (the aggregation factor we used for NGA and ƛ = 25
kms/timestep then we have r = 2 with Ωn = 25 cells.

Suppose rasterAgg = 15 (the aggregation factor we used for NGA and ƛ = 15
kms/timestep then we have r = 1 with Ωn = 9 cells.

Suppose rasterAgg = 10 (the aggregation factor we used for CZE and ƛ = 25
kms/timestep when the human mobility has limited restrictions and no lockdown
are in place in CZE then we have r = round25 - 10 10 + 1 = 3 with Ωn = 49 cells.

Suppose rasterAgg = 5 (the aggregation factor we used for BEL and ƛ = 10
kms/timestep then we have r = round10 - 55 + 1 = 2 with Ωn = 25 cells.


