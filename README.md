# spatialEpisim: Spatial Tracking of Infectious Disease Epidemics using Mathematical Models
 
## Overview

**spatialEpisim** is an open-source platform-independent browser-based interface for tracking the spatial spread of an infectious disease (ex: COVID-19, Ebola, Measles etc.). 

Run the app by loading `app.R` and clicking `Run App`.

## Key features

- Run deterministic/stochastic compartmental models of epidemiology
- Cross-platform: It runs in a browser on Windows, Mac, and Linux
- Generate Spatiotemporal disease prevalence maps: either for an entire country or a smaller area (state(s)/province(s)) within a country.
- Context: Data and examples focus on mathematical modelling of infectious disease epidemics

## Epidemic Models: Schematic Diagram

<!-- ![SEIRD](https://github.com/ashokkrish/spatialEpisim/blob/main/www/SEIRD.png) -->

**The SEIRD Model**

<img src="https://github.com/ashokkrish/spatialEpisim/blob/main/www/SEIRD.png" width="400"/>

**The SVEIRD Model**

<img src="https://github.com/ashokkrish/spatialEpisim/blob/main/www/SVEIRD.png" width="400"/>

![Model Equations](https://latex.codecogs.com/png.image?\dpi{110}%20\frac%20{\partial%20S(x,y,t)}%20{\partial%20t}%20%20=%20-%20\alpha%20S(x,y,t)%20-%20\frac{\beta}{N(x,y,t)}%20S(x,y,t)%20{\iint\limits_{%20\mathcal{B}}}%20w(x,y,u,v)%20%20I(u,v,t)%20\,du%20\,dv\\\frac%20{\partial%20V(x,y,t)}%20{\partial%20t}%20%20=%20\alpha%20S(x,y,t)\\\frac%20{\partial%20E(x,y,t)}%20{\partial%20t}%20=%20\frac{\beta}{N(x,y,t)}%20S(x,y,t)%20%20{\iint\limits_{%20\mathcal{B}}}%20w(x,y,u,v)%20%20I(u,v,t)%20\,du%20\,dv%20-%20\gamma%20E(x,y,t)\\\frac%20{\partial%20I(x,y,t)}%20{\partial%20t}%20=%20\gamma%20E(x,y,t)%20-%20\sigma%20I(x,y,t)%20-%20\delta%20I(x,y,t)\\\frac%20{\partial%20R(x,y,t)}%20{\partial%20t}%20=%20\sigma%20I(x,y,t)\\\frac%20{\partial%20D(x,y,t)}%20{\partial%20t}%20=%20\delta%20I(x,y,t))

## Credits

This interactive [R Shiny](https://shiny.rstudio.com/) app would not be possible without the help from our team of research assistants [Gursimran Dhaliwal](https://github.com/dhaliwalgurs), [Crystal Wai](https://github.com/cwai097), [Jake Doody](https://github.com/jdoody1), and [Timothy Pulfer](https://github.com/TimPulfer). I thank them for their time and hardwork. We acknowledge valuable inputs from [Dr. Bedrich Sousedik](https://github.com/sousedik/) and [Dr. Loren Cobb](http://www.aetheling.com/). 

## References

## Feedback

The app is maintained by Dr. Ashok Krishnamurthy. We welcome questions, insights, and feedback.

Contact: [Ashok Krishnamurthy, Ph.D.](mailto:akrishnamurthy@mtroyal.ca)  
Website: <https://bit.ly/2YKrXjX>  

-----
