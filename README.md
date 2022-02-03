# spatialEpisim: Spatial Tracking of Infectious Disease Epidemics using Mathematical Models
 
## Overview

**spatialEpisim** is an open-source platform-independent browser-based interface for tracking the spatial spread of an infectious disease (ex: COVID-19, Ebola, Measles etc.). 

Run the app on Shinyapp.io server by clicking <http://spatialepisim.shinyapps.io/spatialepisim>

Alternatively you can send a pull request to download all the files in this repo and run the app by loading `app.R` and clicking `Run App`. Note that the ``spatialEpisim`` project is not on CRAN, just on github.

## Key features

- Run deterministic/stochastic compartmental models of epidemiology (ex: SEIR, SEIRD or SVEIRD)
- Cross-platform: It runs in a browser on Windows, Mac, and Linux
- Generate Spatiotemporal disease prevalence maps: either for an entire country or a smaller area (state(s)/province(s)) within a country.
- Context: Data and examples focus on mathematical modelling of infectious disease epidemics

## Packages you'll need

```R
# The absolute minimum
install.packages("lattice")
install.packages("latticeExtra")
install.packages("sp")
install.packages("sf")
install.packages("rgdal")
install.packages("shiny")
install.packages("shinyjs")
install.packages("shinyhelper")
install.packages("shinyWidgets")
install.packages("shinyalert")
install.packages("terra")
install.packages("raster")
install.packages("ggplot2")
install.packages("markdown")
install.packages("cptcity")
install.packages("rasterVis")
install.packages("purrr")
install.packages("stringr")
install.packages("xlsx")
install.packages("countrycode")
install.packages("av")
install.packages("dplyr")
install.packages("maps")
install.packages("magick")
install.packages("lubridate")
install.packages("rstudioapi")
install.packages("fasterize")
install.packages("Rcpp")
```

## Compartmental Models

<!-- ![SEIRD](https://github.com/ashokkrish/spatialEpisim/blob/main/www/SEIRD.png)

**The SEIRD Model**

<img src="https://github.com/ashokkrish/spatialEpisim/blob/main/www/SEIRD.png" width="400"/>  -->

**Schematic Diagram of the SVEIRD Model**

<img src="https://github.com/ashokkrish/spatialEpisim/blob/main/www/SVEIRD.png" width="400"/>

**Model Parameters**

| Parameter | Definition |
| --------- | ---------- |
| &alpha; | is the daily fraction that move from the susceptible compartment into the vaccinated compartment. (Vaccinated individuals are regarded as permanently immune.) |
| &beta; | is the daily fraction that move from the susceptible compartment into the exposed compartment. |
| &gamma; | is the daily fraction that move from the exposed compartment into the infectious compartment. |
| &sigma; | is the daily fraction that move from the infectious compartment into the recovered compartment. (Recovered individuals are regarded as permanently immune.) |
| &delta; | is the daily fraction that move from the infectious into the dead compartment (the mortality rate). |

<sub> Note: Setting &alpha; = 0 and &delta; = 0 would default to a SEIR model while setting only &alpha; = 0 would default to a SEIRD model.</sub>

## Mathematical Models



## Credits

This interactive [R Shiny](https://shiny.rstudio.com/) app would not be possible without the help from our team of research assistants [Gursimran Dhaliwal](https://github.com/dhaliwalgurs), [Crystal Wai](https://github.com/cwai097), [Jake Doody](https://github.com/jdoody1), and [Timothy Pulfer](https://github.com/TimPulfer). I thank them for their time and hardwork.

We acknowledge valuable inputs from [Dr. Bedrich Sousedik](https://github.com/sousedik/) and [Dr. Loren Cobb](http://www.aetheling.com/), [Dr. Agatha E. Ojimelukwe](https://www.linkedin.com/in/agatha-ojimelukwe/) and [Maya Mueller](https://www.linkedin.com/in/maya-mueller-842925198/). 

## References

## Feedback

The app is maintained by Dr. Ashok Krishnamurthy. We welcome questions, insights, and feedback.

Contact: [Ashok Krishnamurthy, Ph.D.](mailto:akrishnamurthy@mtroyal.ca)  
Website: <https://bit.ly/2YKrXjX>  

## Terms of use

-----
