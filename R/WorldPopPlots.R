library(ggplot2)
library(readr)
library(readxl)

#List of countries that need "the" prepended to their name
prependList <- c("Czech Republic", 
                 "Democratic Republic of Congo",
                 "Gambia",
                 "Netherlands")

# =========================================================================== #
# plotLolliChart Function                                                     #
# ------------------------                                                    #
# Function for rendering a ggplot lollipop chart.                             #
#                                                                             #
# =========================================================================== #
# Usage                                                                       #
# ------                                                                      #
# plotLolliChart("Democratic Republic of Congo",                              #
#                "/observeddata/Ebola_Incidence_Data.xlsx")                   #
#                                                                             #
# =========================================================================== #
# Arguments                                                                   #
# ----------                                                                  #
# selectedCountry: the name of the country associated with the data           #
#                                                                             #
# filename:        the pathname for the file containing desired data          #
#                                                                             #
# =========================================================================== #
# Assumptions                                                                 #
# ------------                                                                #
# The datafile must contain data in a specific format where:                  #
#   - Columns 1 and 2  can contain misc. data related to organizing the data  #
#     (collection year, trip number, etc. They are unused for this plot).     #
#   - Column(s) 3+ contain gathered data for individual locations, with each  #
#     column representing individual locations.                               #
#                                                                             #
# =========================================================================== #
plotLolliChart <- function(selectedCountry, filename) {
  
  file <- as.data.frame(openFile(filename))
  data <- file[,3:ncol(file)]
  chartData <- data.frame(x = colnames(data),
                          y = colSums(data))
  
  plotTitle <- paste0("Cumulative Cases in ")
  if(selectedCountry %in% prependList) {
    plotTitle <- paste0(plotTitle, "the ")
  }
  plotTitle <- paste0(plotTitle, selectedCountry)
  
  
  plot <- ggplot(chartData, aes(x=x, y=y)) +
    # geom_hline(yintercept = 0) +
    # geom_vline(xintercept = colnames(data)[1]) +
    geom_segment(aes(x=reorder(x, y), xend=reorder(x, y), y=0, yend=y), color="black") +
    geom_point(color="#18536F", size=2.5) +
    labs(title = plotTitle,
         x = "Location", # x and y labels will be flipped
         y = "Cumulative Cases") +
    theme_light() +
    coord_flip() +
    theme(
      text = element_text(size = 16,
                          face = "bold"),
      plot.title = element_text(size = 24, 
                                face = "bold", 
                                hjust = 0.5,
                                margin = margin(0, 0, 25, 0)),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(3, 2, 2, 2),"cm"),
      axis.title.x = element_text(size = 16, 
                                  face = "bold",
                                  margin = margin(25, 0, 0, 0)),
      axis.title.y = element_text(size = 16, 
                                  face = "bold",
                                  margin = margin(0, 10, 0, 0)),
      axis.text.x.bottom = element_text(size = 16,
                                        face = "bold"),
      axis.text.y.left = element_text(size = 16,
                                      face = "bold"),
      axis.ticks.y = element_blank(),
      axis.line = element_line(linewidth = 0.5)
    ) +
    scale_y_continuous(n.breaks = 8, expand = c(0, 0), limits = c(0,(max(chartData[,"y"] * 1.1))))

  plot
}

# =========================================================================== #
# plotTimeSeries Function                                                     #
# ------------------------                                                    #
# Function for rendering a ggplot time-series plot.                           #
#                                                                             #
# =========================================================================== #
# Usage                                                                       #
# ------                                                                      #
# plotTimeSeries("/observeddata/Ebola_Incidence_Data.xlsx",                   #
#                "Democratic Republic of Congo")                              #
#                                                                             #
# =========================================================================== #
# Arguments                                                                   #
# ----------                                                                  #
# filename:        the pathname for the file containing desired data          #
#                                                                             #
# selectedCountry: the name of the country associated with the data           #
#                                                                             #
# =========================================================================== #
# Assumptions                                                                 #
# ------------                                                                #
# The datafile must contain data in a specific format where:                  #
#   - Column 1 can contain misc. data related to organizing the data          #
#     (collection year, trip number, etc. Column 1 is unused for this plot).  #
#   - Column 2 contains the date for the data gathered in each row            #
#   - Column(s) 3+ contain gathered data for individual locations, with each  #
#     column representing individual locations.                               #
#                                                                             #
#   Sample datafile format:                                                   #
#   --------------------------------------------------------                  #
##  | Misc. | Date  | Location 1 | Location 2 | Location 3 |                  #
#   --------------------------------------------------------                  #
##  | 1     | 04/01 | 12         | 6          | 10         |                  #
#   --------------------------------------------------------                  #
##  | ...   | ...   | ...        | ...        | ...        |                  #
#   --------------------------------------------------------                  #
#                                                                             #
# =========================================================================== #
plotTimeSeries <- function(filename, plotTitle, xlab, ylab, plotColour, plotStyle) {
  file <- as.data.frame(openFile(filename))
  
  if(tools::file_ext(filename) == "csv") {
    plotData <- data.frame(x = dmy(file[,2]),
                           y = rowSums(file[,3:ncol(file)]))
  } else {
    plotData <- data.frame(x = ymd(file[,2]),
                           y = rowSums(file[,3:ncol(file)]))
  }
  
  p = ggplot(plotData,
             aes(x = x, y = y)) +
    geom_line(color=plotColour, linewidth = 2) +
    geom_point(color=plotColour) +
    # geom_area(fill = "#22031F", alpha = 0.3) +
    labs(title = plotTitle, 
         x = xlab, 
         y = ylab) +
    scale_x_date(date_breaks = "6 months", date_labels = "%d %b %Y") +
    scale_y_continuous(minor_breaks = waiver()) +
    theme(
      plot.title = element_text(size = 18, 
                                face = "bold",
                                hjust = 0.5,
                                margin = margin(0, 0, 25, 0)),
      axis.title.x = element_text(size = 16, 
                                  face = "bold",
                                  margin = margin(25, 0, 0, 0)),
      axis.title.y = element_text(size = 16, 
                                  face = "bold",
                                  margin = margin(0, 25, 0, 0)),
      axis.text.x = element_text(angle = 90),
      axis.text.x.bottom = element_text(size = 14),
      axis.text.y.left = element_text(size = 14),
      axis.line = element_line(linewidth = 0.5),
      plot.margin = unit(c(3, 2, 2, 2),"cm")) 
  
  if(plotStyle == "Area"){
    p = p + geom_area(fill = plotColour, alpha = 0.3)
  }
  
  return(p)
}

# =========================================================================== #
# openDataFile Function                                                       #
# ------------------------                                                    #
# Function for read data from a file.                                         #
#                                                                             #
# =========================================================================== #
# Usage                                                                       #
# ------                                                                      #
# openDataFile("/observeddata/Ebola_Incidence_Data.xlsx")                     #
#                                                                             #
# =========================================================================== #
# Arguments                                                                   #
# ----------                                                                  #
# filename: the pathname for the file to be read.                             #
#                                                                             #
# =========================================================================== #
openFile <- function(filename) {
  ext <- tools::file_ext(filename)
  ext <- tolower(ext)
  
  switch(ext, 
         csv =  read_csv(filename, show_col_types = FALSE),
         xls =  read_xls(filename),
         xlsx = read_xlsx(filename),
         txt =  read_tsv(filename, show_col_types = FALSE),
  )
}

