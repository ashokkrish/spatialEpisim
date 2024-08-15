## What is lambda ($\lambda$)?
The average distance travelled per timestep (usually one day), given as an absolute magnitude in kilometers.

## The relationship between the average distance travelled per timestep ($\lambda$) and the effective infectious area (neighbourhood)
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
We call this aggregation factor rasterAgg. The user provides a constant $\lambda$, the
average distance travelled (in km) by an individual in a given cell per
timestep. This constant is viewed as a characteristic of the distance at which
the disease spreads.

If $\lambda <= rasterAgg$ we set $radius = rasterAgg$ and set the effective infectious area
(or neighbourhood) as the [Moore
neighbourhood](https://en.wikipedia.org/wiki/Moore_neighborhood). The Moore
neighborhood is composed of nine cells: a central cell and the eight cells which
surround it. Basically this is the same as $r = 1$.

If $\lambda > rasterAgg$ we set $radius = \lambda + rasterAgg$ and set the effective
infectious area (or neighbourhood) as $r > 1$.

The exact value of $r$ is calculated as `r = round((lambda - rasterAgg) / rasterAgg + eps) + 1`.

The number of grid cells in the effective area is $\Omega_n = (2r + 1)2$

#### Examples
For example, when `r = 1` we have $\Omega_n = 9$ cells.

For example, when `r = 2` we have $\Omega_n = 25$ cells.

Suppose `rasterAgg = 10` (the aggregation factor we used for the Czech Republic and $\lambda = \frac{15 km}{timestep}$
then we have `r = round((15 - 10)/10 + 0) + 1 = 1` with $\Omega_n = 25$.

Suppose `rasterAgg = 15` (the aggregation factor we used for Nigeria and $\lambda = \frac{25 km}{timestep}$
then we have `r = 2` with $\Omega_n = 25$.

Suppose `rasterAgg = 15` (the aggregation factor we used for Nigeria and $\lambda = \frac{25 km}{timestep}$
then we have `r = 1` with $\Omega_n = 9$.

Suppose `rasterAgg = 10` (the aggregation factor we used for the Czech Republic and $\lambda = \frac{25 km}{timestep}$
when the human mobility has limited restrictions and no lockdown
are in place in the Czech Republic then we have `r = round((25 - 10)/10) + 1 = 3` with $\Omega_n = 49$ cells.

Suppose `rasterAgg = 5` (the aggregation factor we used for Belgium and $\lambda = \frac{10 km}{timestep}$
then we have `r = round((15 - 10)/10 + 0) + 1 = 1` with $\Omega_n = 25$.


