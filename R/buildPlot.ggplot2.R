#' plot.ggplot2
#' @param plot.object ggplot2 object
#' @param plot.title character
#' @param plot.subtitle character
#' @param data data.frame
#' @param plot.height numeric
#' @param plot.width numeric
#' @param legend.show boolean
#' @param xAxis.legend character
#' @param yAxis.legend character
#' @param group.legend character
#' @param color.palette character
#' @param plot.type character c("line","spline","point","column","bar")
#' @param xAxis.log boolean
#' @param yAxis.log boolean
#' @param xAxis.reverse boolean
#' @param yAxis.reverse boolean
#' @param line.size numeric
#' @param point.size numeric
#' @param xAxis.max numeric
#' @param yAxis.max numeric
#' @param xAxis.min numeric
#' @param yAxis.min numeric
#' @param xAxis.label boolean
#' @param yAxis.label boolean
#' @param legend.layout character
#' @param legend.align character
#' @param legend.valign character
#' @param line.style character c("solid","dashed","dotted","dotdash","longdash","twodash")
#' @param plot.theme ggplot2 theme
#' @param point.style character c("circle","square","diamond","triangle","triangle-down")
#' @param plot.save boolean
#' @param xAxis.legend.fontsize character
#' @param yAxis.legend.fontsize character
#' @param group.legend.fontsize character
#' @param plot.title.fontsize character
#' @param plot.subtitle.fontsize character
#' @param print.max.abs boolean
#'
#' @return ggplot2 object
#' @export 
#' 
#' @examples
#' data(iris)
#' DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
#' buildPlot.ggplot2(DT)
#' 
#' @import data.table
#' @import RColorBrewer 
#' @import ggplot2
#' @import epoxy
#' @import ggthemes 
#' @import scales
#'

buildPlot.ggplot2 <- function(
    data,
    plot.object = NULL,
    plot.title = NA,
    plot.subtitle = NA,
    plot.height = NA,
    plot.width = NA,
    xAxis.legend = "X",
    yAxis.legend = "Y",
    group.legend = "ID",
    color.palette = NULL,
    plot.type = "line", # c("line","spline","point","column","bar")
    line.style = "solid",
    point.style = 16, #3:cross 4:plus 5:asterisk 6:circle 7:disk 8:square 9:diamond 10:triangle 11:triangle-down
    line.size = 1,
    point.size = 3,
    xAxis.log = FALSE,
    yAxis.log = FALSE,
    xAxis.reverse = FALSE,
    yAxis.reverse = FALSE,
    xAxis.max = NA,
    yAxis.max = NA,
    xAxis.min = NA,
    yAxis.min = NA,
    xAxis.label = TRUE,
    yAxis.label = TRUE,
    legend.layout = "horizontal",
    legend.align = "right", # c("center", "left", "right")
    legend.valign = "top", # c("top", "middle", "bottom")
    legend.show = TRUE,
    plot.save = FALSE,
    plot.theme = theme_flat(),
    xAxis.legend.fontsize="14px",
    yAxis.legend.fontsize="14px",
    group.legend.fontsize="12px",
    plot.title.fontsize="24px",
    plot.subtitle.fontsize="18px",
    print.max.abs = TRUE  # New flag for printing max absolute values
){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  
  if (!all(c("ID", "X", "Y") %in% colnames(data))) {
    stop("data must contain columns named ID, X, and Y")
  }
  if(is.null(plot.theme)){
    plot.theme <- theme_light()
  }
  
  if(is.null(color.palette)){
    color.palette <- "Set 2" 
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
  
  
  COLORS <- grDevices::hcl.colors(n = max(3, min(9,length(unique(DATA$ID)))), palette = color.palette)
  
  PLOT <- PLOT + plot.theme
  
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
  
  if (!legend.show) {
    PLOT <- PLOT + theme(legend.position = "none")
  } else {
    PLOT <- PLOT + theme(legend.position = legend.align)
  }
  
  if (xAxis.label == FALSE) {
    PLOT <- PLOT + theme(axis.title.x = element_blank())
  }
  
  if (yAxis.label == FALSE) {
    PLOT <- PLOT + theme(axis.title.y = element_blank())
  }
  
  # Extract major breaks from ggplot2 object
  build <- ggplot_build(PLOT)
  major_x_breaks <- build$layout$panel_params[[1]]$x$breaks
  major_y_breaks <- build$layout$panel_params[[1]]$y$breaks
  
  # Ensure major breaks are finite
  major_x_breaks <- major_x_breaks[is.finite(major_x_breaks)]
  major_y_breaks <- major_y_breaks[is.finite(major_y_breaks)]
  
  # Rebuild the plot with minor breaks
  PLOT <- PLOT +
    scale_x_continuous(
      breaks = major_x_breaks,
      minor_breaks = generate_minor_breaks(major_x_breaks, n = 10)
    ) +
    scale_y_continuous(
      breaks = major_y_breaks,
      minor_breaks = generate_minor_breaks(major_y_breaks,n=5)
    )
  
  return(PLOT)
}

theme_flat <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#ECF0F1", color = NA),
      panel.background = element_rect(fill = "#ECF0F1", color = NA),
      panel.grid.major = element_line(color = "#BDC3C7", linetype = "dashed"),
      panel.grid.minor = element_line(color = "#BDC3C7", linetype = "dashed"),
      axis.line.x = element_line(color = "#BDC3C7"),
      axis.line.y = element_line(color = "#BDC3C7"),
      axis.ticks.x = element_line(color = "#BDC3C7"),
      axis.ticks.y = element_line(color = "#BDC3C7"),
      axis.text = element_text(color = "#34495e"),
      axis.title = element_text(color = "#34495e", face = "bold"),
      plot.title = element_text(color = "#34495e", face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "#34495e", hjust = 0.5),
      # legend.background = element_rect(fill = alpha("black", 0.5), color = NA),
      legend.key = element_rect(fill = alpha("black", 0.5), color = NA),
      legend.text = element_text(color = "#34495e"),
      legend.title = element_text(color = "#34495e", face = "bold")
    )
}

generate_minor_breaks <- function(major_breaks, n = 5) {
  major_breaks <- major_breaks[is.finite(major_breaks)]
  minor_breaks <- NULL
  for (i in 1:(length(major_breaks) - 1)) {
    minor_breaks <- c(minor_breaks, seq(major_breaks[i], major_breaks[i + 1], length.out = n + 1)[-c(1, n + 1)])
  }
  return(minor_breaks)
}

generate_minor_breaks <- function(major_breaks, n = 5) {
  major_breaks <- major_breaks[is.finite(major_breaks)]
  minor_breaks <- NULL
  for (i in 1:(length(major_breaks) - 1)) {
    minor_breaks <- c(minor_breaks, seq(major_breaks[i], major_breaks[i + 1], length.out = n + 1)[-c(1, n + 1)])
  }
  return(minor_breaks)
}