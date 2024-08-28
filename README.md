<center><h1><em>spatialEpisim</em>: Spatial Tracking of Infectious Diseases using Mathematical Models<h1></center>

# Requirements (see the [INSTALL](INSTALL.md) file)

# Overview
*spatialEpisim* is an open-source, platform-independent, browser-based interface
for tracking the spatial spread of infectious diseases
(e.g. COVID-19, Ebola, Measles, etc.).

The applicaiton can be used without downloading it or running it locally by
using the deployed version on
[Shinyapps.io](http://spatialepisim.shinyapps.io/spatialepisim).

To run the application locally you'll need a copy of the software.
Download a ZIP or tarball of the software in its current,
possibly development state---or a released version if available---from the
GitHub web interface,
or clone the repository using Git,
then run the app locally.

Launch the Shiny application following whatever procedure is correct for your
environment
(in RStudio you may use the <kbd>Run App</kbd> button when viewing one of
global, server, or ui R files).

Note that the *spatialEpisim* project is not available on CRAN,
Bioconductor,
or r-forge:
it is only distributed through GitHub.

## Key features

- Run deterministic/stochastic compartmental models of epidemiology (ex: SEIR, SEIRD or SVEIRD)
- Cross-platform: It runs in a browser on Windows, Mac, and Linux
- Generate Spatiotemporal disease prevalence maps: either for an entire country or a smaller area (state(s)/province(s)) within a country.
- Context: Data and examples focus on mathematical modelling of infectious disease epidemics

## Credits
This interactive [R Shiny](https://shiny.rstudio.com/) app would not be possible
without the help from our team of research assistants:

- [Michael Myer](https://github.com/m-myer),
- [Tobias Wondwossen](https://github.com/Toby-exe),
- [Khanh Le](https://github.com/kle6951),
- [Bryce Carson](https://github.com/bryce-carson),
- [Tom Bayliss White](https://github.com/tombaylisswhite),
- [Gursimran Dhaliwal](https://github.com/dhaliwalgurs),
- [Crystal Wai](https://github.com/cwai097),
- [Jake Doody](https://github.com/jdoody1),
- [Timothy Pulfer](https://github.com/TimPulfer),
- Ryan Darby, and
- [Jason Szeto](https://github.com/jason-szeto).

I thank each of them for their time and hard work.

We acknowledge valuable inputs from
[Dr. Bedrich Sousedik](https://github.com/sousedik/) and
[Dr. Loren Cobb](http://www.aetheling.com/).

<!--- [Dr. Agatha E. Ojimelukwe](https://www.linkedin.com/in/agatha-ojimelukwe/) and [Maya Mueller](https://www.linkedin.com/in/maya-mueller-842925198/). --->

## References
L. Cobb, A. Krishnamurthy, J. Mandel, and J. Beezley.
Bayesian tracking of emerging epidemics using ensemble optimal statistical interpolation (EnOSI).
Spatial and Spatio-temporal Epidemiology, 10:39–48, July 2014.
<https://doi.org/10.1016/j.sste.2014.06.004>

## Feedback
The development and maintenance of the application is supervised by
[Dr. Ashok Krishnamurthy](https://bit.ly/2YKrXjX),
and maintained by him alone when no developers are contracted.

- Contact: [Ashok Krishnamurthy, Ph.D.](mailto:akrishnamurthy@mtroyal.ca)

We welcome questions, insights, and feedback.
We accept contributions via pull request.
You can also open an issue if you find a bug, or have a suggestion.

## License
See the LICENSE file for instructions on copying, redistribution, and your
rights as a user of the software.
