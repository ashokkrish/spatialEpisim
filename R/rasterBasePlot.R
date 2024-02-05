library(countrycode)
library(terra)

createBasePlot <- function(selectedCountry, susceptible, directOutput) {

  inputISO <- countrycode(selectedCountry, origin = 'country.name', destination = 'iso3c') #Converts country name to ISO Alpha
  inputISOLower <- tolower(inputISO)

  #print(Susceptible)
  
  # Susceptible <- terra::rast(Susceptible) # Only a terra::rast() object can use the classify() function
  
  fname <- paste0(inputISO, "_PopulationCount.png")
  PNGFileName <<- paste0("www/", fname)
  
  if(!directOutput){png(PNGFileName, width = 1024, height = 768)} # output the plot to the www image folder

  x <- classify(susceptible, c(0, 10, 25, 50, 100, 250, 1000, 100000))
  
  # plot(x, col=pal(8)[-1], xlab = "Longitude", ylab = "Latitude")
  
  levs <- levels(x)[[1]]
  #levs[7] <- "> 1000"
  levels(x) <- levs

  #ramp <- c('#D0D8FB', '#BAC5F7', '#8FA1F1', '#617AEC', '#0027E0', '#1965F0', '#0C81F8', '#18AFFF', '#31BEFF', '#43CAFF', '#60E1F0', '#69EBE1', '#7BEBC8', '#8AECAE', '#ACF5A8', '#CDFFA2', '#DFF58D', '#F0EC78', '#F7D767', '#FFBD56', '#FFA044', '#EE4F4D')
  ramp <- c('#FFFFFF', 
            '#D0D8FB', 
            '#BAC5F7', 
            '#8FA1F1', 
            '#617AEC', 
            '#0027E0', 
            '#1965F0', 
            '#0C81F8', 
            '#18AFFF', 
            '#31BEFF', 
            '#43CAFF', 
            '#60E1F0', 
            '#69EBE1', 
            '#7BEBC8', 
            '#8AECAE', 
            '#ACF5A8', 
            '#CDFFA2', 
            '#DFF58D', 
            '#F0EC78', 
            '#F7D767', 
            '#FFBD56', 
            '#FFA044', 
            '#EE4F4D')
  pal <- colorRampPalette(ramp)
  
  if(selectedCountry == "Czech Republic"){
       aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for the ", 
                               selectedCountry, 
                               " (1 sq. km resolution)")
  }
  else if(selectedCountry == "Democratic Republic of Congo"){
      aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for the ", 
                              selectedCountry, 
                              " (1 sq. km resolution)")
  }
  else if(selectedCountry == "Gambia"){
    aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for the ", 
                            selectedCountry, 
                            " (1 sq. km resolution)")
  }
  else if(selectedCountry == "Netherlands"){
    aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for the ", 
                            selectedCountry, 
                            " (1 sq. km resolution)")
  }
  else{
       aggrPlotTitle <- paste0("2020 UN-Adjusted Population Count \n for ", 
                               selectedCountry, 
                               " (1 sq. km resolution)")  
  }

  terra::plot(x, 
              col = pal(8)[-1], 
              axes = TRUE,
              buffer = TRUE,
              box = TRUE,
              cex.main = 1.5,
              line.main = 1.25,
              main = aggrPlotTitle,
              xlab = expression(bold(Longitude)),
              ylab = expression(bold(Latitude)),
              line.lab = 2.25,
              cex.lab = 1.4,
              plg = list(title = expression(bold("Persons")),
                         title.cex = 1.25,
                         horiz = TRUE, 
                         loc = "bottom", 
                         yjust = 3.5, 
                         x.intersp = 0.6, 
                         inset = c(0, -0.2), 
                         cex = 1.25), 
              pax = list(cex.axis = 1.7), 
              mar = c(8.5, 3.5, 4, 2.5))  
  terra::north(type = 2, xy = "bottomleft", cex = 1)
  
  # if (selectedCountry == "Czech Republic"){
  #      ## CZE
  #     sbar(100, type="bar", below="km", cex=0.9, xy="bottomright")
  # }
  # else if (selectedCountry == "Nigeria")
  # {
  #      ## NGA
  #     sbar(300, type="bar", below="km", cex=0.9, xy="bottomright")
  # }

  # plot(x, col = pal(8)[-1], axes = TRUE, main = aggrPlotTitle, plg=list(legend=c("0-10", "10-25", "25-50", "50-100", "100-250", "250-1000", ">1000"), horiz = TRUE, x = "bottom", title ="Persons per sq. km"))
  
  # title(xlab = expression(bold(Longitude)), ylab = expression(bold(Latitude)), line = 2, cex.lab = 1.20)

  #---------------------------------------#
  # Source 2: From GADM: Level1Identifier #
  #---------------------------------------#
  
  gadmFileName <- paste0("gadm36_", toupper(inputISO), "_1_sp.rds")   # name of the .rds file 
  gadmFolder <- "gadm/"                                               # .rds files should be stored in local gadm/ folder
  
  # print(paste0(gadmFolder, gadmFileName))
  # print(getwd())
  
  Level1Identifier <- readRDS(paste0(gadmFolder, gadmFileName))
    
  plot(Level1Identifier, add = TRUE)

  
  if(!directOutput){dev.off()} # closes the file opened with png(PNGFileName)
} 

#------------------------#
# Example Function Calls #
#------------------------#
# # Set working directory to the root directory /spatialEpisim otherwise the below examples will not run
#
# createBasePlot(selectedCountry = "Czech Republic", susceptible = suscLayer, directOutput = T) 
# createBasePlot(selectedCountry = "Czech Republic", susceptible = suscLayer, directOutput = F)
#
# createBasePlot(selectedCountry = "Nigeria", susceptible = suscLayer, directOutput = T)
# createBasePlot(selectedCountry = "Nigeria", susceptible = suscLayer, directOutput = F)
#
# where "suscLayer" is a SpatRaster object containing the initial susceptible raster data created 
# using "createSusceptibleLayer()" from rasterWorldPop.R or some other source
