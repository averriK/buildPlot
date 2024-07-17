#' plot.ggplot2
#' @param ... extra arguments
#'
#' @return ggplot2 object
#' @export 
#' 
#' @examples
#' 
#' @import data.table
#' @import RColorBrewer 
#' @import ggplot2
#' @import epoxy
#' @import ggthemes 
#' @import scales
#'

buildPlot.ggplot2 <- function(data,...) {
  on.exit(expr = {rm(list = ls())}, add = TRUE)
  if (!all(c("ID", "X", "Y") %in% colnames(data))) {
    stop("data must contain columns named ID, X, and Y")
  }
  # Extract parameters from the list and assign them to the current environment
  
  params <- list(...)
  list2env(params, envir = environment())
  
  
  if(is.null(plot.theme)){
    plot.theme <- theme_flat()
  }
  
  
  if(is.null(line.style)){
    line.style <- "solid"
  }
  
  DATA <- as.data.table(data)[, c("ID", "X", "Y")]
  DATA[, ID := as.factor(ID)]  # Convert ID to factor
  
  
  if (is.null(plot.object)) {
    PLOT <- ggplot(
      data = DATA,
      aes(x = X, y = Y, group = ID, color = ID)
    )
  } else {
    PLOT <- plot.object
    PLOT <- PLOT + geom_blank() # Ensure it accepts additional layers
  }
  PLOT <- PLOT + plot.theme
  
  COLORS <- grDevices::hcl.colors(n = max(3, min(9,length(unique(DATA$ID)))), palette = color.palette)
  
  if (plot.type == "line") {
    PLOT <- PLOT +
      geom_line(aes(color = ID), size = line.size, linetype = line.style) +
      scale_color_manual(values = COLORS)
  }
  
  if (plot.type == "spline") {
    PLOT <- PLOT +
      geom_smooth(aes(color = ID), method = 'gam', formula = Y ~ s(X, bs = "cs"), size = line.size, linetype = line.style) +
      scale_color_manual(values = COLORS)
  }
  
  if (plot.type == "scatter") {
    PLOT <- PLOT +
      geom_point(aes(color = ID), size = point.size, shape = point.style) +
      scale_color_manual(values = COLORS)
  }
  
  if (print.max.abs) {
    max_values <- DATA[, .SD[which.max(abs(Y))], by = ID]
    PLOT <- PLOT +
      geom_point(data = max_values, aes(x = X, y = Y, fill = ID), size = point.size, shape = point.style, show.legend = FALSE)
  }
  
  if (!is.na(xAxis.min) || !is.na(xAxis.max)) {
    PLOT <- PLOT + xlim(c(xAxis.min, xAxis.max))
  }
  if (!is.na(yAxis.min) || !is.na(yAxis.max)) {
    PLOT <- PLOT + ylim(c(yAxis.min, yAxis.max))
  }
  
  if (yAxis.log == TRUE) {
    PLOT <- PLOT + scale_y_log10()
  }
  if (xAxis.log == TRUE) {
    PLOT <- PLOT + scale_x_log10()
  }
  
  if (yAxis.reverse == TRUE) {
    PLOT <- PLOT + scale_y_reverse()
  }
  
  if (xAxis.reverse == TRUE) {
    PLOT <- PLOT + scale_x_reverse()
  }
  
  # Ensure plot.title and plot.subtitle are not added if NA
  if (!is.na(plot.title)) {
    PLOT <- PLOT + ggtitle(plot.title)
  }
  
  if (!is.na(plot.subtitle)) {
    PLOT <- PLOT + labs(subtitle = plot.subtitle)
  }
  
  PLOT <- PLOT + xlab(xAxis.legend) + ylab(yAxis.legend)
  
  if (xAxis.label == FALSE) {
    PLOT <- PLOT + theme(axis.title.x = element_blank())
  }
  
  if (yAxis.label == FALSE) {
    PLOT <- PLOT + theme(axis.title.y = element_blank())
  }
  
  # Determine horizontal and vertical positions
  x_position <- switch(legend.align,
                       "left" = 0,
                       "center" = 0.5,
                       "right" = 1)
  
  y_position <- switch(legend.valign,
                       "top" = 1,
                       "middle" = 0.5,
                       "bottom" = 0)
  
  # Determine numeric justification values
  hjust_value <- switch(legend.align,
                        "left" = 0,
                        "center" = 0.5,
                        "right" = 1)
  
  vjust_value <- switch(legend.valign,
                        "top" = 1,
                        "middle" = 0.5,
                        "bottom" = 0)
  
  # Applying the legend settings to the plot
  if (legend.show) {
    PLOT <- PLOT + theme(
      legend.position = c(x_position, y_position),
      legend.direction = legend.layout,
      legend.justification = c(hjust_value, vjust_value)
    )
  } else {
    PLOT <- PLOT + theme(legend.position = "none")
  }
  
  # Extract major breaks from ggplot2 object
  build <- ggplot_build(PLOT)
  major_x_breaks <- build$layout$panel_params[[1]]$x$breaks
  major_y_breaks <- build$layout$panel_params[[1]]$y$breaks
  
  major_x_breaks <- major_x_breaks[is.finite(major_x_breaks)]
  major_y_breaks <- major_y_breaks[is.finite(major_y_breaks)]
  
  minor_x_breaks <- generate_minor_breaks(major_x_breaks, n = 5)
  minor_y_breaks <- generate_minor_breaks(major_y_breaks, n = 5)
  
  PLOT <- PLOT +
    scale_x_continuous(
      breaks = major_x_breaks,
      minor_breaks = minor_x_breaks
    ) +
    scale_y_continuous(
      breaks = major_y_breaks,
      minor_breaks = minor_y_breaks
    )
  
  # Add primary auxiliary dashed lines
  for (break_value in major_x_breaks) {
    PLOT <- PLOT + geom_vline(xintercept = break_value, linetype = 2, size=0.3,color = "#BDC3C7")
  }
  for (break_value in major_y_breaks) {
    PLOT <- PLOT + geom_hline(yintercept = break_value, linetype = 2, size=0.3, color = "#BDC3C7")
  }
  
  # Add secondary auxiliary dotted lines
  for (break_value in minor_x_breaks) {
    PLOT <- PLOT + geom_vline(xintercept = break_value, linetype = 3, size=0.1, color = "#BDC3C7")
  }
  for (break_value in minor_y_breaks) {
    PLOT <- PLOT + geom_hline(yintercept = break_value, linetype = 3, size=0.1, color = "#BDC3C7")
  }
  
  
  return(PLOT)
}

theme_flat <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#ECF0F1", color = NA),
      panel.background = element_rect(fill = "#ECF0F1", color = NA),
      panel.grid.major = element_line(color = "#BDC3C7",  size = 0.3),
      panel.grid.minor = element_line(color = "#BDC3C7", size = 0.1),
      axis.line.x = element_line(color = "#BDC3C7"),
      axis.line.y = element_line(color = "#BDC3C7"),
      axis.ticks.x = element_line(color = "#BDC3C7"),
      axis.ticks.y = element_line(color = "#BDC3C7"),
      axis.text = element_text(color = "#34495e"),
      axis.title = element_text(color = "#34495e", face = "bold"),
      plot.title = element_text(color = "#34495e", face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "#34495e", hjust = 0.5),
      legend.background = element_rect(fill = alpha("black", 0.1), color = NA),
      legend.key = element_rect(fill = alpha("black", 0.5), color = NA),
      legend.text = element_text(color = "#34495e"),
      legend.title = element_text(color = "#34495e", face = "bold")
    )
}

# Function to generate minor breaks
generate_minor_breaks <- function(major_breaks, n) {
  # Generate minor breaks between major breaks
  minor_breaks <- unlist(sapply(1:(length(major_breaks) - 1), function(i) {
    seq(major_breaks[i], major_breaks[i + 1], length.out = n + 2)[-c(1, n + 2)]
  }))
  
  # Generate minor breaks before the first and after the last major break
  if(length(major_breaks) > 1) {
    first_interval <- major_breaks[2] - major_breaks[1]
    last_interval <- major_breaks[length(major_breaks)] - major_breaks[length(major_breaks) - 1]
  } else {
    first_interval <- 1
    last_interval <- 1
  }
  
  first_minor_breaks <- seq(major_breaks[1] - first_interval, major_breaks[1], length.out = n + 2)[-c(1, n + 2)]
  last_minor_breaks <- seq(major_breaks[length(major_breaks)], major_breaks[length(major_breaks)] + last_interval, length.out = n + 2)[-c(1, n + 2)]
  
  # Combine all minor breaks
  c(first_minor_breaks, minor_breaks, last_minor_breaks)
}
