## FIXME: it is debatable whether "the" should be prepended to any country name!
prependList <- c("Czech Republic",
                 "Democratic Republic of Congo",
                 "Gambia",
                 "Netherlands")

grey6 <- "#0f0f0f" # the common name for this hexadecimal RGB colour

colourPalette <-
  c(0, 5, 10, 25, 50, 100, 250, 1000, 10000) %>%
  colorBin(cptcity::cpt("jjg_misc_seminf_haxby", colorRampPalette = TRUE)(9)[-1],
           domain = .,
           bins = length(.))

lineThickness <- 1.5
