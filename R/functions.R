options(conflicts.policy = list(warn = FALSE))

suppressPackageStartupMessages({
  library(Matrix)
  library(av)
  library(countrycode)
  library(cptcity)
  library(fasterize)
  library(geodata)
  library(here)
  library(httr2)
  library(lattice)
  library(lubridate)
  library(magick)
  library(rasterVis)
  library(readxl)
  library(rstudioapi)
  library(sf)
  library(sp)
  library(terra)
  library(tidyverse)
  library(writexl)
  library(magrittr)
})

##' Download 1 km aggregated UN-adjusted population count spatial data from 2020
##' through the WorldPop servers.
##'
##' This function only downloads the file if necessary (if it doesn't exist at
##' path).
##'
##' ppp is the code used by WorldPop to mean population estimate for population
##' counts; similar data, but which grids population density is coded _pd_
##' rather than _ppp_. 1km means 1 km aggregation of the data. The description
##' WorldPop gives for this data is «individual countries 2000-2020 UN adjusted
##' aggregated to 1km resolution using 100m resolution population count
##' datasets». See: https://hub.worldpop.org/doi/10.5258/SOTON/WP00671.
##' @title Download WorldPop 2020 data
##' @param countryISO3C The uppercase ISO three character code a recognized
##'   country.
##' @param folder The destination folder the downloaded data will be stored in
##' @return An absolute path where the data was downloaded, or the path at which
##'   the file already existed
##' @author Bryce
##' @examples
##' downloadWorldPopData("USA", here("data", "geotiff"))
downloadWorldPopData <- function(countryISO3C, folder = here("data", "geotiff")) {
  ## Construct the path to the data on data.worldpop.org
  urlPath <-
    c("GIS",
      "Population",
      "Global_2000_2020_1km_UNadj",
      "2020",
      countryISO3C,
      basename = sprintf("%s_ppp_2020_1km_Aggregated_UNadj.tif",
                         tolower(countryISO3C))) %>%
    httr2:::dots_to_path()
  url <- url_build(structure(list(scheme = "https",
                                  hostname = "data.worldpop.org",
                                  path = urlPath),
                             class = "httr2_url"))

  ## Download the GeoTIFF file if it doesn't already exist.
  if (!file.exists(here(folder, basename(url)))) {
    download.file(url, here(folder, basename(url)), mode = "wb")
  }

  here(folder, basename(url))
}

## DONE
##' Returns a SpatVector for the requested country on demand, either retrieving
##' the data from disk if it has been downloaded before, or downloading it for
##' the first time.
##'
##' The level one boundaries are the least granular administrative boundaries
##' that countries create to subdivide themselves.
##' @title Retrieve a SpatRaster of level 1 boundaries from local or remote disk
##' @param countryISO3C The uppercase ISO three character code a recognized
##'   country.
##' @return SpatVector
##' @author Bryce
lvl1AdminBorders <- function(ISO3C) {
  geodata::gadm(country = ISO3C,
                level = 1,
                path = here("data", "gadm",
                            sprintf("gadm36_%s_1_sp.rds", tolower(ISO3C))),
                version = "3.6",
                resolution = 1)
}

## DONE
##' Create a named RasterLayer object useful for spatiotemporal epidemic
##' compartmental modelling.
##' @title Create a Susceptible-component RasterLayer
##' @param countryISO3C The uppercase ISO three character code a recognized
##'   country.
##' @returns A RasterLayer of WorldPop population count data with the name
##'   Susceptible, with all NAs replaced by zeros.
##' @author Bryce Carson
##' @author Michael Myer
##' @author Ashok Krishnmaurthy
##' @examples
##' getCountryPopulation.SpatRaster("COD")
getCountryPopulation.SpatRaster <- function(countryISO3C) {
  rast(downloadWorldPopData(countryISO3C)) %>%
    replace(., is.na(.), 0) %>% # . is a magrittr placeholder for the piped data.
    `names<-`("Susceptible")
}

## DONE
##' Read an RDS file from the GADM folder for a given country's subregion.
##'
##' @title Get a country's GADM 36 raster
##' @param countryISO3C The uppercase ISO three character code a recognized
##'   country.
##' @param level1Region The subregions of the country to crop to
##' @param folder The folder which should be searched for the GADM data
##' @returns SpatVector for the specificed country.
##' @author Bryce Carson
##' @examples
##' getCountrySubregions.SpatVector("COD", c("Nord-Kivu", "Ituri"), here("data", "gadm"))
##' getCountrySubregions.SpatVector("CZE", "Prague")
##' getCountrySubregions.SpatVector("NGA", "Kwara")
getCountrySubregions.SpatVector <- function(countryISO3C = "COD",
                          level1Region = c("Nord-Kivu", "Ituri"),
                          folder = here("data", "gadm")) {
  stopifnot(countryISO3C %in% countrycode::codelist$iso3c)
  ## Read data from the gadm folder corresponding to the country. ## NOTE: 1_sp
  ## refers to the fact that this RDS file contains 1km aggregated spatial data.
  here(folder, sprintf("gadm36_%s_1_sp.rds", toupper(countryISO3C))) %>%
    readRDS() %>%
    subset(.$NAME_1 %in% level1Region) %>%
    vect()
}

## DONE
## NOTE: This function is related to cropping "base" rasters, and plotting them
## using the Haxby colour table. For more information on the Haxby colour table,
## see the entry for Haxby in the r.colors documentation of the GRASS GIS
## software suite: https://grass.osgeo.org/grass83/manuals/r.colors.html; this
## states "relative colors for bathymetry or topography".
##' Plot a raster cropped to a specific region(s)
##'
##' As a side effect, it writes a raster to file.
##' @title Plot a raster cropped to sub-region(s) of a country
##' @author Bryce Carson
##' @author gursDhaliwal
##' @author Ashok Krishnmaurthy
##' @author Michael Myer
##' @author Thomas White
##' @param subregions a SpatRaster object for subregions, created with
##'   getCountrySubregions.SpatVector
##' @param susceptible SpatRaster or SpatVector to be masked by GADM data
##'   of the country
##' @examples
##' subregions <- getCountrySubregions.SpatVector("COD",
##'                            c("Nord-Kivu", "Ituri"),
##'                            here("data", "gadm"))
##' susceptible <- getCountryPopulation.SpatRaster("COD")
##' result <- maskAndClassifySusceptibleSpatRaster(subregions = subregions,
##'                                                susceptible = susceptible)
##' plot(result)
##'
##' maskAndClassifySusceptibleSpatRaster(getCountrySubregions.SpatVector("COD", c("Nord-Kivu", "Ituri")),
##'                                      getCountryPopulation.SpatRaster("COD"))
##'
##' maskAndClassifySusceptibleSpatRaster(getSubregion("CZE", "Prague"),
##'                                      getCountryPopulation.SpatRaster("CZE"))
##'
##' maskAndClassifySusceptibleSpatRaster(getSubregion("NGA", "Lagos"),
##'                                      getCountryPopulation.SpatRaster("NGA"))
##'
##' maskAndClassifySusceptibleSpatRaster(getSubregion("COD", "Ituri"),
##'                                      getCountryPopulation.SpatRaster("COD"))
##'
##' maskAndClassifySusceptibleSpatRaster(getSubregion("COD", c("Nord-Kivu", "Ituri")),
##'                                      getCountryPopulation.SpatRaster("COD"))
maskAndClassifySusceptibleSpatRaster <- function(subregions, susceptible) {
  crs(subregions) <- crs(susceptible, proj = TRUE)

  ## NOTE: these two values were betwixt the call of rast and classify, which
  ## are only piped to remove the use of an unnecssary assignment whilst these
  ## interjecting values are unused and therefore don't need to be calculated.
  ## Rather than interjecting and elongating the pipe's line count, they are
  ## moved here (before the pipe), so only a relevant comment interjects in the
  ## pipeline.
  ##
  ## MAYBE FIXME: these values are unused.
  ## dlong = abs(xmax(susceptibleMaskedBySubregion) -
  ##             xmin(susceptibleMaskedBySubregion))
  ## ## MAYBE FIXME: shouldn't this be ymax - ymin?
  ## dlat = abs(ymax(susceptibleMaskedBySubregion) -
  ##            xmax(susceptibleMaskedBySubregion))

  ## Mask the susceptible SpatRaster by the subregions SpatRaster
  crop(susceptible, subregions, mask = TRUE) %>%
  ## NOTE: this is refactored, but untested. NOTE: this seems to classify the
  ## values as 1:7, if they have a value between the bin lower limits; that is,
  ## bin the values into classes delineated by the given vector of minimums for
  ## the bins.
  classify(c(0, 10, 25, 50, 100, 250, 1000, 10000)) %>%
    "levels<-"(levels(.)[[1]])
}

##' @title Create a RasterStack of SVEIRD model compartment raster data
##' @description Create a list of SpatRaster objects, one for each component in
##'   an SVEIRD epidemic model.
##' @details The SpatRaster objects for the VEIRD components are empty, while
##'   the Inhabited SpatRaster is a binary classification on habitation of land
##'   area.
##' @param subregions a SpatVector object of subregions used to crop the SVEIRD SpatRaster
##' @param susceptible a RasterLayer, pre-aggregated if that is wished.
##' @param aggregationFactor the number of cells in any direction to aggregate
##'   together into one, in the susceptible ratser, after masking with the
##'   subregions vector, before creating the list
##' @returns a SpatRaster, with layers for the SVERID components and an
##'   additional layer classifying the Inhabited status of a cell
##' @author Bryce Carson
##' @author Michael Myer
##' @author Ashok Krishnmaurthy
##' @examples
##' getSVEIRD.SpatRaster(getCountrySubregions.SpatVector("COD", c("Ituri", "Nord-Kivu")),
##'                      getCountryPopulation.SpatRaster("COD"),
##'                      aggregationFactor = 35)
getSVEIRD.SpatRaster <- function(subregions, susceptible, aggregationFactor = NULL) {
  susceptible <- maskAndClassifySusceptibleSpatRaster(subregions, susceptible)

  if (!is.null(aggregationFactor)) {
    susceptible <- aggregate(susceptible, aggregationFactor)
  }

  ## MAYBE FIXME: I'd be surprised if any of the values were negative, but it's
  ## okay to retain this.
  values(susceptible)[values(susceptible) < 0] <- 0

  Inhabited <- susceptible
  values(Inhabited)[values(Inhabited) > 0] <- 1
  values(Inhabited)[values(Inhabited) < 1] <- 0

  ## The values are EXACTLY equal, so I won't use this faster code.
  ## Inhabited <- classify(susceptible, matrix(c(c(-Inf, 1, 0), c(0, Inf, 1)), nrow = 2, byrow = TRUE))

  empty <- init(susceptible, fun = 0)
  c(susceptible, rep(empty, 5), Inhabited) %>%
    "names<-"(c("Susceptible",
                "Vaccinated",
                "Exposed",
                "Infected",
                "Recovered",
                "Dead",
                "Inhabited"))
}

##' @title Average Euclidean Distance
##' @description Measure the exposure influence upon susceptible individuals at
##'   spatial position (x, y) of the infectious individuals at a spatial
##'   position (u, v).
##' @details The mathematical modelling of human or non-human mobility patterns
##'   is non-trivial. This function is a limited implementation of the effective
##'   area for only human mobility, with distances travelled per day (λ)
##'   measured in kilometers.
##'
##'   NOTE: Our weight function does not take the same arguments as shown in the
##'   slideshow: (𝑤 x y u v); The term “effective area” comes from the fact that
##'   if the kernel is constant on a finite disk (and zero outside it), then the
##'   formula due to Bolker (1999) gives the area of the disk.
##'
##'   See the article titled *A clarification of transmission terms in
##'   host-microparasite models by Begon et al.* (2002).
##'
##'   The raster data of infection counts or disease incidence provided to the
##'   function which calls this one, transmissionLikelihoodWeightings, may be
##'   aggregated by a given factor. That factor must be passed to this function
##'   for parity, so the data is treated the same.
##' @param radius TODO: a fixed radius r > lambda; see details.
##' @param lambda movemenet distance (in kilometers) per day; see details.
##' @param aggregationFactor the degree of aggregation applied to the raster
##'   data mentioned in the function details.
##' @returns a matrix of the average Euclidean distances
##' @author Bryce Carson
##' @author Thomas White
avgEuclideanDistance <- function(radius, lambda, aggregationFactor = NULL) {
  ## NOTE: “I have a philosophical and geometric question about these
  ## identities: what do they imply about the dimension and magnitude of the
  ## raster which the weighted number sum will be used with? Can we rid the
  ## function of the radius argument and only accept the lambda argument, given
  ## these identities and the claim made about the calculation of radius in the
  ## slides?” — Bryce
  ## if (radius > lambda)
  ##   stopifnot(r == round((lambda - aggregationFactor) / aggregationFactor) + 1)
  ## else
  ##   stopifnot(r == lambda + aggregationFactor)

  len <- seq_len(1 + radius * 2)
  df <- tidyr::expand(tibble(i = len, j = len), i, j)
  avg.euc.dist <- function(i, j) {
    exp(-sqrt(sum((c(i, j) - c(radius + 1, radius + 1))^2)) / lambda)
  }
  mutate(df, avgEuclideanDistance = map2_dbl(i, j, avg.euc.dist)) %>%
    dplyr::select(avgEuclideanDistance) %>%
    unlist(use.names = FALSE) %>%
    matrix(byrow = TRUE, ncols = sqrt(length(.)))
}

##' @title Weighted Sums
##' @description Calculate a matrix of weights respecting human mobility
##'   patterns.
##' @details The pattern of human mobility used is described in a slideshow
##'   here:
##'   https://docs.google.com/presentation/d/1_gqcEh4d8yRy22tCZkU0MbGYSsGu3Djh/edit?usp=sharing&ouid=102231457806738400087&rtpof=true&sd=true.
##' @param infections a matrix of the count of infections per aggregate area in
##'   a raster of terrestrial data.
##' @param radius a constant; see details.
##' @param lambda movemenet distance (in kilometers) per day; see details.
##' @param aggregationFactor the degree of aggregation applied to the raster
##'   data mentioned in the function details.
##' @returns a matrix of weightings for the calculation of the proportion of
##'   exposed individuals who will become infectious.
##' @author Bryce Carson
##' @author Thomas White
##' @examples
##' terra::as.matrix(Infected, wide = TRUE) %>%
##'   transmissionLikelihoodWeightings(30, 15, 10)
transmissionLikelihoodWeightings <-
  function(infections, radius, lambda, aggregationFactor) {
    focal(infections, avgEuclideanDistance(radius, lambda, aggregationFactor))
  }

##' @title Linear (Forward) Interpolation Operator matrices for one or two state
##'   vectors
##' @details Create a linear forward interpolation operator matrix with as many
##'   columns as cells in the SpatRaster layers, and as many rows as health
##'   zones for which coordinates are provided (when creating a matrix for use
##'   with one state vector). The matrix is either one or two partitions in a
##'   sparse, block diagonal matrix.
##'
##' Each row of the partition
##'
##' When used to creat an interpolation matrix for two state vectors,
##'   the result is a block diagonal matrix of identical partitions, with each
##'   partition as described for one state vector.
##' @param layers The SpatRaster object with SVEIRD compartment layers, and a
##'   layer classifying habitation. Created with the getSVEIRD.SpatRaster
##'   function.
##' @param healthZoneCoordinates a table of values giving the latitude and
##'   longitude coordinates for health zones in the country of interest. See the
##'   description of the healthZoneCoordinates argument in the function
##'   SVEIRD.BayesianDataAssimilation for more details.
##' @param compartmentsReported either 1 or 2. Previously identified as
##'   states_observable, this is the count of compartments that are reported on
##'   and which will have data assimilated; if it is 2, the matrix is a block
##'   diagonal matrix.
##' @returns a linear forward interpolation operator matrix used in
##'   SVEIRD.BayesianDataAssimilation simulations.
##' @author Bryce Carson
##' @author Ashok Krishnmaurthy
##' @author Michael Myer
##' @examples
##' healthZonesCongo <- read.csv(here("data",
##'                                   "observed",
##'                                   "Ebola_Health_Zones_LatLon.csv"))
##' subregionsSpatRaster <-
##'   getCountrySubregions.SpatVector("COD", c("Ituri", "Nord-Kivu"))
##' linearInterpolationOperator(
##'   layers = getSVEIRD.SpatRaster(subregionsSpatRaster,
##'                                 getCountryPopulation.SpatRaster("COD"),
##'                                 aggregationFactor = 15),
##'   healthZoneCoordinates = healthZonesCongo,
##'   compartmentsReported = 1
##' )
linearInterpolationOperator <- function(layers, healthZoneCoordinates, compartmentsReported = 1) {
  stopifnot(ncol(layers) >= 5) # a second order neighbour can't be calculated
                               # with the current algorithm if the number fo
                               # columns is less than five.
  stopifnot(compartmentsReported %in% 1:2)

  queensNeighbours <- function(order, cell, ncols) {
    outer.offset <- order * ncols
    inner.offsets <- seq(0, order - 1) * ncols
    inner.offsets <- c(cell - inner.offsets,
                       cell + inner.offsets)
    c(cell - outer.offset - 2 : cell - outer.offset + 2,
      cell + outer.offset - 2 : cell + outer.offset + 2,
      inner.offsets + order,
      inner.offsets - order)
  }

  ## NOTE: cells contains the index into the rasters in layers (when converted
  ## to a matrix). The coordinates are re-ordered as longitude-latitude, rather
  ## than latitude-longitude as they are otherwise stored; the reason is due to
  ## the generation of NAs in the resulting matrix, otherwise, according to the
  ## previous implementation.
  cells <- cellFromXY(layers, as.matrix(healthZoneCoordinates[, 3:2]))
  if (anyDuplicated(cells) > 0)
    warning("Duplicate cell indices in cells vector derived from health zone coordinates.")

  ## NOTE: preallocate the linear forward interpolation matrix, with dimensions
  ## q ✕ p, where p is the number of cells in the SpatRaster layers (nrow *
  ## ncols), and q is the number of health zones; i.e., the dimensions of the
  ## matrix are n health zones ✕ m cells in the SpatRaster.
  H <- matrix(0, nrow(healthZoneCoordinates), ncell(layers))

  ## NOTE: these are the weightings used for the chess queen 🨁 zeroth, first,
  ## and second order neighbors. The zeroth order neighbor is the position of
  ## the queen itself. See
  ## https://www.paulamoraga.com/book-spatial/spatial-neighborhood-matrices.html#neighbors-of-order-k-based-on-contiguity
  ## for more information. The index into the vector is one more than the order
  ## of the neighourhood the value at that index the value applies to.
  neighbour.weights <- c(0.12, 0.8, 0.04) * 5 / 7

  for(index in seq_along(cells)) {
    neighbour.1st <- queensNeighbours(1, cells[index], ncol(layers))
    neighbour.2nd <- queensNeighbours(2, cells[index], ncol(layers))
    if(anyDuplicated(c(neighbour.1st, neighbour.2nd)) > 0)
      simpleError("Duplicate cell indices among neighbours of multiple localities.")
    H[index, cells[index]                     ] <- neighbour.weights[1]
    H[index, neighbour.1st[neighbour.1st >= 0 &
                           neighbour.1st <= ncell(layers)]] <-
      neighbour.weights[2]
    H[index, neighbour.2nd[neighbour.2nd >= 0 &
                           neighbour.2nd <= ncell(layers)]] <-
      neighbour.weights[3]
  }

  if (compartmentsReported == 2) H <- bdiag(H, H) # block diagonal matrix
  attr(H, "healthZoneCoordinates") <- healthZoneCoordinates
  attr(H, "layers") <- dim(layers)[3]
  attr(H, "compartmentsReported") <- compartmentsReported

  H
}

##' @description Q-matrix generation from various parameters.
##' @details TODO: write the details about the implementation of this function.
##' @title Create a Q-matrix
##' @param layers The SpatRaster object with SVEIRD compartment layers, and a
##'   layer classifying habitation. Created with the getSVEIRD.SpatRaster
##'   function.
##' @param variableCovarianceFunction TODO
##' @param Q.variance TODO
##' @param Q.correlationLength TODO
##' @param neighbourhood TODO
##' @param compartmentsReported TODO
##' @returns TODO
##' @author Bryce Carson
##' @author Ashok Krishnmaurthy
##' @author Michael Myer
##' @author Thomas White
##' @examples
##' variableCovarianceFunction <- "DBD"
##' Q.variance <- 2
##' Q.correlationLength <- 0.8
##' compartmentsReported <- 2
##'
##' layers <-
##'   getSVEIRD.SpatRaster(getCountrySubregions.SpatVector("COD", c("Ituri", "Nord-Kivu")),
##'                        getCountryPopulation.SpatRaster("COD"),
##'                        aggregationFactor = 35)
##'
##' nrow(layers)
##' ncols(layers)
##'
##' Qmat <- genQ(layers, "DBD", Q.variance = 1, Q.correlationLength = 0.8, neighbourhood = 4, compartmentsReported = 2)
##'
##' x <- 1:10
##' y <- 1:10
##'
##' X <- matrix(x, nrow = 10, ncols = 10, byrow = TRUE)
##' Y <- matrix(y, nrow = 10, ncols = 10, byrow = FALSE)
##'
##' library(plot3D)
##' persp3D(x = X, y = Y, z = Qmat$Q[1:10,1:10], theta = 90, expand = 0.5,
##'         xlab = "Columns", ylab = "Rows", scale = FALSE, clim = c(0, 1),
##'         colkey = list(side = 1))
genQ <- function(layers, variableCovarianceFunction, Q.variance, Q.correlationLength, neighbourhood, compartmentsReported = 2) {
  nrows <- nrow(layers)
  ncols <- ncols(layers)
  p <- nrows * ncols
  Q0 <- Q <- matrix(0, p, p)
  rows <- rep(1:(p / ncols), each = ncols)
  cols <- rep(1:ncols, times = (p / ncols))
  irow <- matrix(rep(rows, length(rows)), nrow = p, byrow = TRUE)
  icol <- matrix(rep(cols, length(cols)), nrow = p, byrow = TRUE)
  jrow <- t(irow)
  jcol <- t(icol)
  d <- sqrt((irow - jrow)^2 + (icol - jcol)^2)

  varCov.fun <- switch(variableCovarianceFunction,
         DBD = \(Q.variance) Q.variance * Q.correlationLength^d,
         Balgovind = \(Q.variance) Q.variance * (1 + (d / Q.correlationLength)) * exp(-d / Q.correlationLength),
         Exponential = \(Q.variance) Q.variance * exp(-d / Q.correlationLength),
         Guassian = \(Q.variance) Q.variance * exp(-d^2 / 2 * Q.correlationLength^2),
         Spherical = function(Q.variance) {
           ## NOTE: the Q.correlationLength actually refers to the radius of the
           ## spherical variance-covariance function.
           varMatrix <- Q.variance * ((3 * d) / (2 * Q.correlationLength) - d^3 / (2 * Q.correlationLength^3))
           varMatrix[d >= Q.correlationLength] <- 0
           varMatrix
         },
         stop(r"[Provided name of variableCovarianceFunction is invalid.
Valid function names are:
 - DBD
 - Balgovind
 - Exponential
 - Gaussian
 - Spherical]"))

  Q[d < neighbourhood] <- varCov.fun(Q.variance)[d < neighbourhood]
  diag(Q) <- ifelse(diag(Q) == 0, Q.variance, diag(Q))

  if (compartmentsReported == 2) {
    Qtop <- cbind(Q, Q0)
    Qbottom <- cbind(Q0, Q)
    QFull <- rbind(Qtop, Qbottom)
  }

  return(list("Q" = Q, "QFull" = QFull))
}

replaceInequalityWith <- function(f, w, x, y, z) {
  w[f(x, y)] <- z
  w
}

##' @description Run a SVEIRD compartmental model of an epidemic, optionally
##'   using Bayesian data assimilation.
##' @details TODO: DETAILS of the function.
##' @title SVEIRD compartmental model with optional Bayesian data assimilation
##' @param psi.diagonal TODO
##' @param layers The SpatRaster object with SVEIRD compartment layers, and a
##'   layer classifying habitation. Created with the getSVEIRD.SpatRaster
##'   function.
##' @param startDate The date (in YYYY-MM-DD format) the simulation begins.
##' @param countryISO3C The ISO three character code for a recognized country.
##' @param rasterAgg The number of adjacent cells in any one direction to
##'   aggregate into a single cell.
##' @param alpha The rate of vaccination (per day)
##' @param beta The rate of exposure (per day)
##' @param gamma The rate of becoming infectious (per day)
##' @param sigma The rate of recovery (per day)
##' @param delta The fatality rate (per day)
##' @param radius The distance, in kilometers, a given individual travels from
##'   their starting point (on average, per day)
##' @param lambda The probability that an individual will move the distance
##'   governed by radius
##' @param n.days The number of days the simulation will run for, beginning from
##'   the startDate.
##' @param seedData a dataframe like the following example; the compartment
##'   columns are the initial values.
##'
##'   \preformatted{ Location Latitude Longitude Vaccinated Exposed Infected
##'     Recovered Dead Beni 0.49113 29.47306 0 24 12 0 4 Butembo 0.140692
##'     29.335014 0 0 0 0 0 Mabalako 0.461257 29.210687 0 0 0 0 0 Mandima
##'     1.35551 29.08173 0 0 0 0 0 }
##' @param seedRadius The number of cells over which to average the seed data in
##'   a Moore neighbourhood for each locality.
##' @param simulationIsDeterministic Whether stochasticity is enabled or not; if
##'   the simulation is deterministic then no stochastic processes are used and
##'   the simulation is entirely deterministic.
##' @param dataAssimilationEnabled Whether Bayesian data assimilation will be
##'   used for state reporting data.
##' @param healthZoneCoordinates The coordinates of health zones in the country
##'   of interest, TODO: describe the use of the data so users understand why
##'   they must include it; e.g. (which will be used to group and summarize the
##'   compartmental model data at the end of the simmulation.)
##'
##'   \preformatted{
##'     HealthZone    Latitude    Longitude
##'     Alimbongo     -0.365515   29.1911818
##'     Beni          0.49113     29.47306
##'     Biena         0.57923     29.115633
##'     Butembo       0.140692    29.335014
##'     Goma          -1.658271   29.220132
##'     Kalunguta     0.323085    29.354677
##'     Katwa         0.116985    29.411838
##'     Kayna         -0.603936   29.174066
##'     Kyondo        -0.005622   29.408813
##'     Lubero        -0.15633    29.24057
##'     Mabalako      0.461257    29.210687
##'     Manguredjipa  0.353433    28.765479
##'     Masereka      -0.133333   29.333333
##'     Musienene     0.04022     29.26246
##'     Mutwanga      0.32893     29.74576
##'     Nyiragongo    -1.516667   29.25
##'     Oicha         0.698681    29.518834
##'     Pinga         -0.9830504  28.687911
##'     Vuhovi        0.1416      29.4075
##'     Ariwara       3.136873    30.706615
##'     Bunia         1.566667    30.25
##'     Komanda       1.367496    29.774322
##'     Lolwa         1.352969    29.498455
##'     Mambasa       1.359731    29.029226
##'     Mandima       1.35551     29.08173
##'     Nyakunde      1.431271    30.029283
##'     Rwampara      1.4053      30.3449
##'     Tchomia       1.4412      30.4845
##'   }
##' @param incidenceData A "situation report" dataframe. The first column
##'   provides the date of the officially reported, observed incidence of the
##'   disease, in ISO format (YYYY-MM-DD). MAYBE TODO: enforce the startDate
##'   parameter to be one week prior to the first observed data?
##'
##'   \preformatted{
##'     Date        Beni  Butembo  Mabalako  Mandima
##'     2018-08-05  34    34       34        34
##'     2018-08-12  2     0        11        1
##'     2018-08-20  1     0        37        6
##'     2018-08-26  5     0        3         0
##'     2018-08-02  8     0        1         1
##'     2018-08-09  5     2        1         1
##'   }
##' @param deathsAndDeadData TODO
##' @param variableCovarianceFunction Passed directly to [genQ()] to generate a Q matrix.
##' @param Q.variance TODO
##' @param Q.correlationLength TODO
##' @param neighbourhood TODO
##' @returns a summary dataframe for the simulation, showing changes in the
##'   compartment values over time, the daily values, and cumulative values.
##' @author Bryce Carson
##' @author Ashok Krishnmaurthy
##' @author Michael Myer
##' @examples
##' SVEIRD.BayesianDataAssimilation(
##'   ## Parameters
##'   alpha = 3.5e-5,
##'   beta = 7e-3,
##'   gamma = 1/7,
##'   sigma = 1/36,
##'   delta = 2/36,
##'   radius = 1,
##'   lambda = 15,
##'   ## Model runtime
##'   n.days = 31, # a month, permitting three assimilations of observed data
##'   ## Model data
##'   seedData = here("data", "seed", "COD_InitialSeedData.csv"),
##'   layers = createRasterList(getCountrySubregions.SpatVector("COD", c("Itrui", "Nord-Kivu")),
##'                             getCountryPopulation.SpatRaster("COD"),
##'                             35),
##'   startDate = "2018-08-05",
##'   countryISO3C = "COD",
##'   incidenceData = here("data", "observed", "Ebola_Incidence_Data.xlsx"),
##'   ## Model options
##'   simulationIsDeterministic = TRUE,
##'   dataAssimilationEnabled = TRUE,
##'   healthZoneCoordinates = here("data", "observed", "Ebola_Health_Zones_LatLon.csv"),
##'   variableCovarianceFunction = "DBD",
##'   ## Special parameters
##'   Q.variance = 0.55,
##'   Q.correlationLength = 6.75e-1,
##'   neighbourhood = 3,
##'   psi.diagonal = 1e-3
##' )
SVEIRD.BayesianDataAssimilation <-
  function(## Parameters
           alpha,
           beta,
           gamma,
           sigma,
           delta,
           radius,
           lambda,

           ## Model runtime
           n.days,

           ## Model data
           seedData,
           seedRadius = 0,
           rasterAgg,
           layers,
           startDate,
           countryISO3C,
           incidenceData,

           ## Model options
           simulationIsDeterministic,
           dataAssimilationEnabled = FALSE,
           healthZoneCoordinates,
           variableCovarianceFunction,

           ## Special parameters
           Q.variance,
           Q.correlationLength,
           neighbourhood,
           psi.diagonal,
           compartmentsReported = 1) {
    ## Preallocate a zeroed data frame with the following column names, and
    ## store it in a symbol named "summary".
    names <- c(## Population and epidemic compartments (states)
               "N", "S", "V", "E", "I", "R", "D",
               ## Daily values of new vaccinations, exposures, infections,
               ## recoveries, and deaths
               "newV", "newE", "newI", "newR","newD",
               ## Cumulative values of exposed or infected people through the
               ## simulation runtime
               "cumE", "cumI")

    summary <-
      data.frame(matrix(data = 0, ncols = length(names), nrow = n.days)) %>%
      "colnames<-"(names)

    ## NOTE: HOW ARE THESE USED?
    nrows <- nrow(layers)
    ncols <- ncols(layers)
    p <- nrows * ncols # What is the meaning of p?

    ## NOTE: cast the seed data from the initial infections equitably, in a
    ## Moore Neighborhood of cells.
    for (locationNumber in seq_along(nrow(seedData))) {
      ## Select only the epidemic compartment data
      data <- as.vector(seedData[locationNumber, -c(1:3)])
      ## NOTE: the numerator is the exposed and infected compartment; the
      ## denominator is the number of cells per region.
      data[2:3] <- data[2:3] / (2 * seedRadius + 1)^2

      ## Get a cell number from the latitude for this health region.
      row <- terra::rowFromY(layers, seedData[locationNumber, 2])
      rowRange <- seq(from = row - seedRadius,
                      to = row + seedRadius)

      ## Get a cell number from the longitude for this health region.
      col <- terra::colFromX(layers, seedData[locationNumber, 3])
      columnRange <- seq(from = col - seedRadius,
                         to = col + seedRadius)

      ## FIXME TODO: these layers are empty, so there's no need to summate the
      ## values. The values can simply be overlaid. MAYBE TODO: a terra method
      ## probably exists to assign the values in a vector to given cells in a
      ## vector of selected layers in a SpatRasterCollection.
      ##
      ## layers[2:6][row, col] <- data
      layers$Vaccinated[row, col]            %<>% sum(data[1])
      layers$Recovered[row, col]             %<>% sum(data[4])
      layers$Dead[row, col]                  %<>% sum(data[5])
      ## FIXME: the exposed and infected persons will be seeded in multiple
      ## cells, cloning them!
      layers$Exposed[rowRange, columnRange]  %<>% sum(data[2])
      layers$Infected[rowRange, columnRange] %<>% sum(data[3])
    }

    ## Calculate the proportion of people who will move from the susceptibile
    ## compartment to another compartment. NOTE: the names, exactly as they
    ## are, provide semantics for the components: e.g. accessing the
    ## proportion$vaccinated, or the proportion$exposed, is self-describing.
    proportion <-
      map_dbl(c(vaccinated = layers$Vaccinated,
                exposed = layers$Exposed,
                infected = layers$Infected,
                recovered = layers$Recovered,
                dead = layers$Dead),
              function(otherCompartment, susceptibleCompartment) {
                sum(values(otherCompartment)) / susceptibleCompartment
              },
              ## NOTE: Supplying the sum of the values of the susceptible compartment
              ## as an extra argument passed to the anonymous function ensures
              ## that its value is only calculated once.
              susceptibleCompartment = sum(values(layers$Susceptible)))

    ## Calculate the actual number of people that have moved to other compartments
    ## and subtract these from the original Susceptible compartment count.
    ## FIXME: Error in -proportion : invalid argument to unary operator
    values(layers$Susceptible) %<>% sum(as.matrix(layers$Susceptible) * -proportion)

    if (dataAssimilationEnabled) {
      ## MAYBE TODO: resurrect some, a lot, or all of the other print statements
      ## that once existed and convert them to messages, which are easily
      ## suppressed with the suppressMessages function. E.g.:
      ## message(sprintf("Dimension of Incidence Matrix: %s ✕ %s",
      ##                 dim(incidenceData)[1],
      ##                 dim(incidenceData)[2]))

      ## Generate the linear interpolation operator matrix (function works for
      ## two compartments, at most).
      linearInterpolationMatrix <-
        linearInterpolationOperator(layers,
                                    healthZoneCoordinates,
                                    compartmentsReported)


      ## NOTE: create the model error covariance matrix, which, given we are
      ## using an ensemble-type data assimilation process, is time invariant.
      QMat <- genQ(layers,
                   variableCovarianceFunction,
                   Q.variance,
                   Q.correlationLength,
                   neighbourhood,
                   compartmentsReported)

      Q <- QMat$Q

      ## NOTE: the determinant and dimensions of the matrices are unused, but
      ## are easily calculated; NOTE: Q and H are both block diagonal, sparse
      ## matrices (but not of class sparseMatrix:
      ## <https://stat.ethz.ch/R-manual/R-patched/library/Matrix/html/sparseMatrix-class.html>).
      QFull <- QMat$QFull
      QHt <- QFull %*% t(linearInterpolationMatrix)
      HQHt <- linearInterpolationMatrix %*% QHt

      ## MAYBE FIXME: there is no q anymore! Should there be? The sum of the
      ## Eigen values should be equal to q; is q the number of health zones (as
      ## in q ✕ p elsewhere?)
      ## stopifnot(sum(eigen(HQHt)$values) == q)
    }

    ## NOTE: preallocate the list which will hold a timeseries of RasterStack
    ## objects.
    layers.timeseries <- vector(mode = "list", length = n.days)

    ## FIXME: absolutely do not use this "datarow". There's a much better way.
    ## NOTE: datarow is a sentinel value used to prevent trying to assimilate
    ## more data than exists in the observed data dataframe. Seventy-six is a
    ## magic number, which is actually the number of rows of observed data plus
    ## one (for one-based indexing). The rows are weekly data; it may be easier
    ## to simply check if there is more data to assimilate, and whenever the
    ## simulation date is one day before or the same day as the observed data it
    ## is then assimilated.
    datarow <- 1 # pre-allocating the row from which we read the data to
                                        # assimilate each week
    for (t in seq(n.days)) {
      ## TODO: At this URL, StackOverflow user Roman provides a reprex for a
      ## waitress callback function to generate a progress bar.
      ## https://stackoverflow.com/a/77496657/14211497.
      print(paste("time = ", t)) # Print the date at each time step

      ## TODO: the entire column can be calculated one time and added to the
      ## summary data at the end of the function.
      ## summary[t, 1]  <- toString(as.Date(startDate) + n.days(t - 1))

      ## Set NSVEI counts in the summary table.
      with(proportion, {
        summary[t, 2] <- round(sum(susceptible, vaccinated, exposed, infected, recovered, dead))
        summary[t, 3] <- round(susceptible)
        summary[t, 4] <- round(vaccinated) # Absorbing state
        ## This is the prevalence (active exposed cases) at time t, NOT the
        ## cumulative sum.
        summary[t, 5] <- round(exposed)
        ## This is the prevalence (active infectious cases) at time t, NOT the
        ## cumulative sum.
        summary[t, 6] <- round(infected)

        layerWideMatrices <- lapply(c(Inhabited = inhabited,
                                      Susceptible = susceptible,
                                      Vaccinated = vaccinated,
                                      Exposed = exposed,
                                      Infected = infected,
                                      Recovered = recovered,
                                      Dead = dead),
                                    terra::as.matrix,
                                    wide = TRUE)
      })

      numberLiving <- sum(layerWideMatrices)
      transmissionLikelihoods <-
        transmissionLikelihoodWeightings(layerWideMatrices$Infected,
                                         radius,
                                         lambda,
                                         rasterAgg)

      ## Some susceptible people are going to be newly vaccinated
      with(layerWideMatrices, {
        newVaccinated <- (alpha * Susceptible) %>%
          replaceInequalityWith(f = `<`, Susceptible, 1, 0)
      })

      ## Some susceptible people who come in contact with nearby infected are
      ## going to be newly exposed
      proportionSusceptible <- layerWideMatrices$Susceptible / numberLiving
      proportionSusceptible[is.nan(proportionSusceptible)] <- 0

      growth <- beta * proportionSusceptible * transmissionLikelihoods
      newExposed <- growth %>%
        if(simulationIsDetermistic) growth else rpois(1, growth)

      newExposed[c(susceptibleMatrix < 1) || transmissionLikelihoods < 1] <- 0

      dailyExposed <- sum(newExposed)

      ## Some exposed people are going to become newly infectious
      valExposed <- Exposed + newExposed # MAYBE FIXME: what was valExposed before the refactor?
      newInfected <- gamma * valExposed
      newInfected[valExposed < 1] <- 0

      dailyInfected <- sum(newInfected)

      ## FIXME: this is awful practice; the cumulative infected should, ideally,
      ## be summarized from daily infected data later on, but cumulative
      ## infected is used later on in the simulation algorithm before it ends
      ## and before the mentioend summary values could be calculated in the
      ## mentioned alternative practice.
      if (!exists("cumInfected")) cumInfected <- 0
      cumInfected <- sum(cumInfected, newInfected)

      summary[t, 8] <- round(sum(Dead)) # Absorbing state
      summary[t, 15] <- round(cumInfected)

      ## Some infectious people are going to either recover or die
      newRecovered <- sigma*valInfected
      newRecovered[valInfected < 1] <- 0

      dailyRecovered <- sum(newRecovered)

      newDead <- delta*valInfected
      newDead[valInfected < 1] <- 0

      dailyDead <- sum(newDead)

      ## Store the next state of each cell
      nextCellValues <-
        tibble(Susceptible = layerWideMatrices$Susceptible -
                 newExposed -
                 newVaccinated,

               Vaccinated = layerWideMatrices$Vaccinated - newVaccinated,

               Exposed = layerWideMatrices$Exposed + newExposed - newInfected,

               Infected = layerWideMatrices$Infected +
                 newInfected -
                 newDead -
                 newRecovered,

               Recovered = layerWideMatrices$Recovered + newRecovered,

               Dead = layerWideMatrices$Dead + newDead) %>%
        mutate(across(Susceptible:Dead,
                      ## NOTE: this is a purrr-style lambda.
                      ~ replaceInequalityWith(`<`,
                                              .x,
                                              numberLiving,
                                              0,
                                              0))) %>%
        as.list() %>%
        lapply(rast) %>%
        lapply(function(component) {
          "ext<-"(component, RasterStack)
          "crs<-"(component, RasterStack)
        })

      with(nextCellValues, {
        layers$Susceptible <- Susceptible
        layers$Vaccinated <- Vaccinated
        layers$Exposed <- Exposed
        layers$Infected <- Infected
        layers$Recovered <- Recovered
        layers$Dead <- Dead
      })

      ## TODO: cbind the following on the row we're creating with the intent to
      ## rbind later, where this row is at time t. NOTE: this is a refactoring
      ## inclusion, not a refactoring of original behaviour. This is a possible
      ## alternative route.
      ## cbind(newVaccinated,
      ##       dailyExposed,
      ##       dailyInfected,
      ##       dailyRecovered,
      ##       dailyDead)

      ## NOTE: assimilate observed data weekly, not more or less frequently,
      ## while there is still data to assimilate.
      if (all(dataAssimlationEnabled,
              t %% 7 == 0,
              datarow < nrow(incidenceData))) {
        datarow <- datarow + 1

        ## NOTE: Optimal statistical inference: forecast state. NOTE: We track
        ## the compartments representing the states of being infectious or dead.
        Infected.prior <- terra::as.matrix(layers$Infected, wide = TRUE)

        Infected <- terra::as.matrix(layers$Infected, wide = TRUE)
        rat <- sum(terra::as.matrix(Exposed, wide = TRUE)) /
          (sum(Infected) + 1e-9) # FIXME: magic number

        ## MAYBE FIXME: this operation seems dubious. What was the motivation
        ## behind it?
        Xf.OSI <- Infected %>% t() %>% as.vector() %>% t() %>% t()

        HXf <- linearInterpolationMatrix %*% Xf.OSI

        ## Pick a row every 7 n.days, select third column through to the last
        ## column; measurement error covariance matrix.
        D.v <- as.vector(incidenceData[datarow, 1:nrow(healthZoneCoordinates) + 2]) %>%
          replaceInequalityWith(f = `<`, ., ., 1, psi.diagonal)

        ## NOTE: The gain matrix, Ke.OSI, determines how the observational data
        ## are assimilated.
        Ke.OSI <- QHt %*% matrix::solve(HQHt + diag(D.v))

        ## NOTE: Optimal statistical inference update step: analyze state? THEM:
        ## "OSI update step: analysis state".
        Xa.OSI <- Xf.OSI + Ke.OSI %*% (t(t(D.v)) - HXf) %>%
          replaceInequalityWith(f = `<`, 0, 0)

        ## MAYBE TODO: is subsetting even necessary? Is Xa.OSI larger than
        ## seq(p), requiring us to subset it so that I is not too large? NOTE:
        ## when restacking make sure byrow = T.
        I <- matrix(Xa.OSI[seq(p)],
                    nrow = nrows,
                    ncols = ncols,
                    byrow = TRUE)

        ## NOTE: if an area is uninhabitable replace its value with zero; it
        ## makes more sense to instead use NA values to prevent calculating
        ## values for uninhabitable areas. MAYBE TODO: a raster with
        ## uninhabitable areas which can mask the susceptible and any other
        ## layer with NAs would be better than this.
        I %<>% replaceInequalityWith(f = `==`, valInhabitable, 0, 0)
        cumInfected <- cumInfected +
          sum(I - Infected.prior)
        layers$Exposed <- rat * "values<-"(layers$Infected, I)
      }

      layers.timeseries[[t]] <- layers
    }

    summary[is.na(summary)] <- 0
    return(summary)
  }

##' @title Awful Plotting Code
##' @author al
awfulPlottingCode <- function() {
  ## TODO: refactor everything below this line.
  unlink("www/MP4", recursive = TRUE, force = TRUE) # Delete the MP4
  dir.create("www/MP4")               # Create empty MP4 folder before running new simulation
  dir.create("www/MP4/paper")         # Create paper folder before for plots without labels

  save(stack$raster.list[["Infected"]], file = "infectedRaster.RData")
  ## plot(layers.timeseries[[t]]$raster.list[["Infected"]],
  ##      col = pal(8)[-2],
  ##      axes = T,
  ##      cex.main = 1,
  ##      main = "Location of Initial Infections",
  ##      plg = list(title = expression(bold("Persons")),
  ##                 title.cex = 1,
  ##                 horiz = TRUE,
  ##                 x.intersp = 0.6,
  ##                 inset = c(0, -0.2),
  ##                 cex = 1.15),
  ##      pax = list(cex.axis = 1.15),
  ##      legend = TRUE,
  ##      mar = c(8.5, 3.5, 2.5, 2.5),
  ##      add = FALSE)
  ## plot(Level1Identifier, add = TRUE)

  ## Print a PNG for the infected variable
  rasterLayer <- "Infected"
  ## print(layers.timeseries[[1]]$raster.list[[rasterLayer]])
  maxRasterLayerVal <- 0

  for (t in 1:n.days) {
    tempMax <- minmax(layers.timeseries[[t]]$raster.list[[rasterLayer]])
    maxRasterLayerVal <- max(maxRasterLayerVal, tempMax)
  }

  ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC',
            '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF',
            '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE',
            '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767',
            '#FFBD56', '#FFA044', '#EE4F4D')
  pal <- colorRampPalette(ramp)

  for (t in 1:n.days) {
    ## OLD
    ## fname = paste0("MP4/", countryISO3C, "_", rasterLayer, "_", sprintf("%04d", t), ".png")

    ## From /home/bryce/Documents/src/r/spatialEpisim/R/plotting/rasterPlot.R
    printStackLayer(raster.list = layers.timeseries[[t]]$raster.list,
                    rasterLayer = rasterLayer,
                    directOutput = directOutput,
                    Level1Identifier = stack$Level1Identifier,
                    countryISO3C,
                    rasterAgg = rasterAgg,
                    fname = here("www", "MP4", "paper", sprintf("%s_%s_%04d.png",
                                                                countryISO3C,
                                                                rasterLayer,
                                                                t)),
                    maxVal = maxRasterLayerVal,
                    includeLabels = F,
                    isCropped)
  }

  ## MERGE THE PNGs TO A GET AN MP4 VIDEO
  setwd("www/MP4")
  videoDuration <- 15 # in seconds
  av::av_encode_video(list.files(pattern = ".png"),
                      framerate = n.days/videoDuration,
                      output = paste0(rasterLayer, "_MP4.mp4"))
  setwd("./../..")
}

##' @title Awful plotting code №2
##' @author al
awfulPlottingCode_two <- function() {
    ## ramp <- c('#FFFFFF', '#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC',
    ##           '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF',
    ##           '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE',
    ##           '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767',
    ##           '#FFBD56', '#FFA044', '#EE4F4D')
    ## pal <- colorRampPalette(ramp)
    ## par(mfrow = c(1, 2))
    ## plot(Infected, col = pal(8)[-2], axes = T, cex.main = 1,
    ##      main = "Location of Initial Infections",
    ##      xlab = expression(bold("Longitude")), ylab = expression(bold("Latitude")),
    ##      legend = TRUE, horizontal = TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
    ##
    ## plot(Level1Identifier, add = TRUE)
    ##
    ## plot(Dead, col = pal(8)[-2], axes = T, cex.main = 1,
    ##      main = "Location of Initial Deaths",
    ##      xlab = expression(bold("Longitude")), ylab = expression(bold("Latitude")),
    ##      legend = TRUE, horizontal = TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
    ##
    ## plot(Level1Identifier, add = TRUE)
    ##
    ## plot(log10(Susceptible), col = pal(8)[-2], axes = T, cex.main = 1, main = "Susceptible", legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
    ## plot(Level1Identifier, add = TRUE)
    ##
    ## plot(Inhabited, col = pal(8)[-2], axes = T, cex.main = 1, main = "Inhabited Cells", legend=TRUE, mar=c(8.5, 3.5, 2.5, 2.5))
    ## plot(Level1Identifier, add = TRUE)
    ## writeRaster(Infected, "seed.tif", overwrite = TRUE)
}
