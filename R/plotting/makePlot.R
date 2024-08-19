##' Make a line plot of multiple compartments with particular colours
##' @title Compartment data line plot
##' @param compartments The names of compartments to plot, which appear in the `input` argumenmt.
##' @param plotTitle The title of the plot.
##' @param xTitle The title of the horizontal axis.
##' @param yTitle The title of the vertical axis.
##' @param lineThickness the preferred line thickness.
##' @param input A data frame with a shape as described in the details.
##' @returns a ggplot2 object
##' @author Bryce Carson
##' @author Ashok Krishnamurthy
linePlotCompartmentValue <- function (compartments, plotTitle, xTitle, yTitle, lineThickness = 1.5, input) {
  compColors <- "names<-"(c("yellow", "blue", "orange", "red", "green", "black"),
                          c("S", "V", "E", "I", "R", "D"))

  plot <-
    plotData %<>%
    dplyr::mutate(Date = lubridate::ymd(input$Date)) %>%
    dplyr::pivot_longer(dplyr::all_of(compartments),
                        names_to = "Compartment",
                        values_to = "Count") %>%
    dplyr::mutate(Compartment = factor(Compartment, levels = compartments)) %>%
    ggplot2::ggplot(aes(x = Date, y = Count, color = Compartment)) +
    ggplot2::geom_line(linewidth = lineThickness) +
    ggplot2::labs(title = plotTitle,
         x = xTitle,
         y = yTitle,
         color = NULL) +
    ggplot2::scale_x_date(date_labels = "%d %b %Y") +
    ggplot2::scale_color_manual(
      values = c(
        "S" = "yellow",
        "V" = "blue",
        "E" = "orange",
        "I" = "red",
        "R" = "green",
        "D" = "black"
      )
    ) +
    ggplot2::coord_cartesian(clip = "off")

  ## When there is more than one compartment to plot, there should be a legend.
  if(length(compartments) == 1) {
    p <- p + ggplot2::theme(legend.position = 'none')
  }

  return(p)
}
