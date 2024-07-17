#' Title
#' @param ... extra arguments
#' 
#' @return highcharter object
#' @export 
#' 
#' @examples
#'
#' @import data.table data.table
#' @importFrom data.table as.data.table 
#' @importFrom grDevices hcl.colors 
#' @importFrom grDevices col2rgb
#' @importFrom highcharter hc_add_series
#' @importFrom highcharter hc_add_theme
#' @importFrom highcharter hc_colors
#' @importFrom highcharter highchart
#' @importFrom highcharter hc_legend
#' @importFrom highcharter hcaes
#' @importFrom highcharter hc_theme_hcrt
#' @importFrom highcharter hc_xAxis
#' @importFrom highcharter hc_yAxis
#' @importFrom highcharter hc_tooltip
#' @importFrom highcharter hc_chart
#' @importFrom highcharter hc_plotOptions
#' @importFrom highcharter hc_size
#' @importFrom highcharter hc_tooltip
#' @importFrom highcharter hc_title
#' @importFrom highcharter hc_subtitle
#' @importFrom epoxy epoxy_html
#'


buildPlot.highcharter <- function(data,...) {
  on.exit(expr = {rm(list = ls())}, add = TRUE)
  if (!all(c("ID", "X", "Y") %in% colnames(data))) {
    stop("data must contain columns named ID, X, and Y")
  }
  # Extract parameters from the list and assign them to the current environment
  
  params <- list(...)
  list2env(params, envir = environment())
  if(is.null(plot.theme)){
    plot.theme <- highcharter::hc_theme_flat()
  }
  if(is.null(color.palette)){
    color.palette <- grDevices::hcl.pals()[4]
  }
  #
  
  if(is.null(line.style)){
    line.style <- "Solid"
  }
  
  DATA <- as.data.table(data)[, c("ID", "X", "Y")]
  
  TIP <- epoxy::epoxy_html("{{group.legend}}:{point.series.name}<br> {{xAxis.legend}}={point.x}<br> {{yAxis.legend}}={point.y}")
  COLORS <- grDevices::hcl.colors(n = max(3, min(9,length(unique(DATA$ID)))), palette = color.palette)
  
  if (is.null(plot.object)) {
    PLOT <- highchart()
  } else {
    PLOT <- plot.object
  }
  
  # Prepare data for the shaded region
 
  if (fill.polygon==TRUE && length(unique(DATA$ID)) == 2) {
    DT1 <- DATA[ID == unique(DATA$ID)[1]]
    DT2 <- DATA[ID == unique(DATA$ID)[2]]
    
    # Create polygon points
    polygon_data <- rbind(DT1, DT2[nrow(DT2):1, ], fill=TRUE)
    polygon_data$ID <- fill.group
    
    PLOT <- PLOT |>
      hc_add_series(
        data = polygon_data,
        type = "polygon",
        hcaes(x = X, y = Y),
        name = fill.group,
        color = .hex_to_rgba(COLORS[1], 0.3),
        fillOpacity = 0.3
      )
  }
  
  # c("line","spline","point","column","bar")
  if (plot.type %in% c("line", "spline")) {
    PLOT <- PLOT |>
      hc_add_series(
        data = DATA, # main curve
        type = plot.type,
        dashStyle = line.style,
        lineWidth = line.size, # Apply line size here
        
        hcaes(x = X, y = Y, group = ID, color = ID)
      )
  }
  
  if (plot.type %in% c("scatter", "point")) {
    PLOT <- PLOT |>
      hc_add_series(
        data = DATA, # main curve
        type = "scatter",
        marker = list(symbol = point.style, radius = point.size), # Apply point size here
        hcaes(x = X, y = Y, group = ID)
      )
  }
  
  PLOT <- PLOT |>
    hc_add_theme(hc_thm = plot.theme) |>
    hc_yAxis(
      labels = list(enabled = yAxis.label),
      title = list(text = yAxis.legend, style = list(fontSize = yAxis.legend.fontsize)),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = FALSE,
      showLastLabel = TRUE
    ) |>
  
  
    hc_xAxis(
      labels = list(enabled = xAxis.label),
      title = list(text = xAxis.legend, style = list(fontSize = xAxis.legend.fontsize)),
      minorTickInterval = "auto",
      minorGridLineDashStyle = "Dot",
      showFirstLabel = TRUE,
      showLastLabel = TRUE
    ) |>
    hc_colors(colors = COLORS) |>
    hc_tooltip(
      sort = FALSE,
      split = FALSE,
      crosshairs = TRUE,
      pointFormat = TIP
    ) |>
    hc_legend(
      enabled = legend.show,
      align = legend.align,
      verticalAlign = legend.valign,
      layout = legend.layout,
      itemStyle = list(fontSize = group.legend.fontsize)
    ) |>
    hc_chart(style = list(fontFamily = "Helvetica"))
  
  if (!is.na(plot.title)) {
    PLOT <- PLOT |>
      hc_title(text = plot.title, fontSize = list(fontSize = plot.title.fontsize))
  }
  
  if (!is.na(plot.subtitle)) {
    PLOT <- PLOT |>
      hc_subtitle(text = plot.subtitle, fontSize = list(fontSize = plot.subtitle.fontsize))
  }
  
  if (!is.na(xAxis.max)) {
    PLOT <- PLOT |>
      hc_xAxis(max = xAxis.max)
  }
  if (!is.na(yAxis.max)) {
    PLOT <- PLOT |>
      hc_yAxis(max = yAxis.max)
  }
  
  if (!is.na(xAxis.min)) {
    PLOT <- PLOT |>
      hc_xAxis(min = xAxis.min)
  }
  if (!is.na(yAxis.min)) {
    PLOT <- PLOT |>
      hc_yAxis(min = yAxis.min)
  }
  if (yAxis.log == TRUE) {
    PLOT <- PLOT |>
      hc_yAxis(type = "logarithmic")
  }
  if (xAxis.log == TRUE) {
    PLOT <- PLOT |>
      hc_xAxis(type = "logarithmic")
  }
  
  if (xAxis.reverse == TRUE) {
    PLOT <- PLOT |>
      hc_xAxis(reversed = TRUE)
  }
  
  if (yAxis.reverse == TRUE) {
    PLOT <- PLOT |>
      hc_yAxis(reversed = TRUE)
  }
  
  if (!is.na(plot.height) & is.na(plot.width)) {
    PLOT <- PLOT |> hc_size(height = plot.height)
  }
  
  if (!is.na(plot.width) & is.na(plot.height)) {
    PLOT <- PLOT |> hc_size(width = plot.width)
  }
  
  if (!is.na(plot.width) & !is.na(plot.height)) {
    PLOT <- PLOT |> hc_size(width = plot.width, height = plot.height)
  }
  PLOT <- PLOT |> highcharter::hc_exporting(enabled = plot.save, filename = "hc_plot")
  return(PLOT)
}

.hex_to_rgba <- function(hex, alpha = 1) {
  rgb <- col2rgb(hex) / 255
  paste0("rgba(", paste(c(rgb, alpha), collapse = ","), ")")
}

