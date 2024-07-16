# _spatialEpisim_: Spatial Tracking of Infectious Diseases using Mathematical Models
 
## Overview

**spatialEpisim** is an open-source platform-independent browser-based interface for tracking the spatial spread of infectious diseases (ex: COVID-19, Ebola, Measles etc.). 

Run the app on Shinyapp.io server by clicking <[[http://spatialepisim.shinyapps.io/spatialepisim](https://ashokkrish.shinyapps.io/spatialepisim/)](https://ashokkrish.shinyapps.io/spatialepisim/)>.

To run the application locally you'll need a copy of the software. Download an archived version of the repository or a release (if available) from the GitHub web interface, or clone the repository using Git or your IDE tools, then run the app locally. To run the Shiny application follow whatever procedure is correct for your environment (in RStudio you may use the "Run App" button) after loading `global.R`. 

Note that the *spatialEpisim* project is not available on CRAN, Bioconductor, or r-forge: it is only distributed through GitHub.

## Key features

- Run deterministic/stochastic compartmental models of epidemiology (ex: SEIR, SEIRD or SVEIRD)
- Cross-platform: It runs in a browser on Windows, Mac, and Linux
- Generate Spatiotemporal disease prevalence maps: either for an entire country or a smaller area (state(s)/province(s)) within a country.
- Context: Data and examples focus on mathematical modelling of infectious disease epidemics

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

## Directory structure
<!-- TODO: update this information and reorganize it. -->

    /
    |---R/
    |---gadm/
    |---misc/
    |---observeddata/
    |---seeddata/
    |---tif/
    |   |---cropped/
    |---www/
    |   |---MP4/
    |


- `gadm/` folder with .RDS files with a database of Global Administrative Areas.
- `misc/` folder with spreadsheets for default epidemic parameters and ISO3 Alpha codes.
- `R/` folder with R scripts sourced in app.R.
- `seeddata/` folder with seed data for selected countries.
- `tif/` folder with the 2020 UN-Adjusted Population Count rasters downloaded from [WorldPop](https://www.worldpop.org/) .
- `www/` is for static compartmental model flowcharts.
  - `MP4/` is where simulation MP4 animation and output images and are saved.

## Credits

This interactive [R Shiny](https://shiny.rstudio.com/) app would not be possible without the help from our team of research assistants [Michael Myer](https://github.com/m-myer), [Tobias Wondwossen](https://github.com/Toby-exe), [Khanh Le](https://github.com/kle6951), [Bryce Carson](https://github.com/bryce-carson), [Tom Bayliss White](https://github.com/tombaylisswhite), [Gursimran Dhaliwal](https://github.com/dhaliwalgurs), [Crystal Wai](https://github.com/cwai097), [Jake Doody](https://github.com/jdoody1), [Timothy Pulfer](https://github.com/TimPulfer),  Ryan Darby and [Jason Szeto](https://github.com/jason-szeto). I thank them for their time and hardwork.

We acknowledge valuable inputs from [Dr. Bedrich Sousedik](https://github.com/sousedik/) and [Dr. Loren Cobb](http://www.aetheling.com/).

<!--- [Dr. Agatha E. Ojimelukwe](https://www.linkedin.com/in/agatha-ojimelukwe/) and [Maya Mueller](https://www.linkedin.com/in/maya-mueller-842925198/). --->

## References

L. Cobb, A. Krishnamurthy, J. Mandel, and J. Beezley. Bayesian tracking of emerging epidemics using ensemble optimal statistical interpolation (EnOSI).Spatial and Spatio-temporal Epidemiology, 10:39–48, July 2014. <https://doi.org/10.1016/j.sste.2014.06.004>

## Feedback

The app is maintained by Dr. Ashok Krishnamurthy.

Contact: [Ashok Krishnamurthy, Ph.D.](mailto:akrishnamurthy@mtroyal.ca)  
Website: <https://bit.ly/2YKrXjX>  

We welcome questions, insights, and feedback. We accept contributions via pull request. You can also open an issue if you find a bug, or have a suggestion.
 
## License

See the LICENSE file for instructions on copying, redistribution, and your rights concerning the aas a user.
