### Raster aggregation factor
This is the single number passed to `terra::aggregate`. It is the number of
cells horizontally and vertically, describing a square, which will be aggregated
into a single cell of lower resolution (a larger spatial domain in one cell
corresponds to a lower resolution, a higher resolution corresponds to a smaller
spatial domain).

The exact code used is given below. The vector `c(rasterAgg, rasterAgg)` has the
same effect as simply providing `fact = rasterAgg`.

```R
Aggregated <- aggregate(WorldPop, fact = c(rasterAgg, rasterAgg), fun = sum, na.rm = TRUE)
```

`WorldPop` is the raster data downloaded from the WorldPop project.

For more information read the
[`terra::aggregate`](https://rspatial.github.io/terra/reference/aggregate.html)
function documentation.
