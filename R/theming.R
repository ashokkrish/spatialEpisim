## FIXME: it is debatable whether "the" should be prepended to any country name!
prependList <- c("Czech Republic",
                 "Democratic Republic of Congo",
                 "Gambia",
                 "Netherlands")

grey6 <- "#0f0f0f" # the common name for this hexadecimal RGB colour

cuts <- c(NA, 10^(0:6))
options(terra.plot = map.pal("haxby", length(cuts))[-1])

lineThickness <- 1.5
