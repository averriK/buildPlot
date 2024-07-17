#' Title
#' @param plot.object highcharter object
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
#' @param legend.align character c("center", "left", "right")
#' @param legend.valign character c("top", "middle", "bottom")
#' @param line.style character c("Solid","Dashed","Dotted", "DashDot","LongDash","LongDashDot","LongDashDotDot")
#' @param plot.theme highcharter object
#' @param point.style character c("circle","square","diamond","triangle","triangle-down")
#' @param plot.save boolean
#' @param fill.polygon boolean
#' @param fill.group character
#' @param xAxis.legend.fontsize character
#' @param yAxis.legend.fontsize character
#' @param group.legend.fontsize character
#' @param plot.title.fontsize character
#' @param plot.subtitle.fontsize character


#' @return highcharter object
#' @export 
#' 
#' @examples
#' data(iris)
#' DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
#' buildPlot.highcharter(DT,plot.type="scatter")
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


buildPlot.highcharter <- function(
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
    line.style = "Solid",
    point.style = "circle",
    line.size = 1,
    point.size = 2,
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
    plot.theme = highcharter::hc_theme_flat(),
    plot.save = TRUE,
    fill.polygon= FALSE,
    fill.group="",
    xAxis.legend.fontsize="14px",
    yAxis.legend.fontsize="14px",
    group.legend.fontsize="12px",
    plot.title.fontsize="24px",
    plot.subtitle.fontsize="18px"
    
){
  on.exit(expr = {rm(list = ls())}, add = TRUE)
  #
  if (!all(c("ID", "X", "Y") %in% colnames(data))) {
    stop("data must contain columns named ID, X, and Y")
  }
  
  if(is.null(color.palette)){
    color.palette <- grDevices::hcl.pals()[4]
  }
  #
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

