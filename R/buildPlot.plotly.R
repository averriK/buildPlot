#' Title
#' @param plot.object plotly object
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
#'
#' @return plotly object
#' @export 
#' 
#' @examples
#' data(iris)
#' DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
#' buildPlot.plotly(DT)
#' 
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table 
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom plotly subplot



buildPlot.plotly <- function(
    data,
    plot.object=NA,
    
    plot.title=NA,
    plot.subtitle=NA,
    plot.height=NA,
    plot.width=NA,
    xAxis.legend="X",
    yAxis.legend="Y",
    group.legend="ID",
    color.palette="viridis",
    plot.type="line",#line
    line.style="solid",
    point.style="circle",
    xAxis.log=FALSE,
    yAxis.log=FALSE,
    xAxis.reverse=FALSE,
    yAxis.reverse=FALSE,
    line.size=1,
    point.size=2,
    xAxis.max=NA,
    yAxis.max=NA,
    xAxis.min=NA,
    yAxis.min=NA,
    xAxis.label=TRUE,
    yAxis.label=TRUE,
    legend.layout="horizontal",
    legend.align="right", #c("center", "left", "right")
    legend.valign="top", #c("top", "middle", "bottom")
    legend.show=TRUE,
    plot.save = FALSE,
    plot.theme=NA
    
) {
  if (!all(c(xAxis.legend, yAxis.legend, group.legend) %in% colnames(data))) {
    stop("data must contain columns specified in xAxis.legend, yAxis.legend, and group.legend")
  }
  
  # Mapear estilos de línea y puntos a plotly
  line.types <- c(solid = "solid", dashed = "dash", dotted = "dot", dotdash = "dashdot", longdash = "longdash")
  point.types <- c(circle = "circle", square = "square", diamond = "diamond", triangle = "triangle-up", "triangle-down" = "triangle-down")
  
  # Convertir estilos de línea y puntos a plotly
  DATA <- as.data.table(data)[,c("ID","X","Y")]
  line.style <- line.types[[line.style]]
  point.style <- point.types[[point.style]]
  
  # Inicializar la lista de trazas
  plotly_traces <- list()
  
  # Agrupar datos por group.legend
  data_groups <- split(DATA, DATA[[group.legend]])
  
  # Generar trazas para cada grupo
  for (group in names(data_groups)) {
    group_data <- data_groups[[group]]
    if (plot.type %in% c("line", "spline")) {
      trace <- plot_ly(x = group_data[[xAxis.legend]], y = group_data[[yAxis.legend]], type = 'scatter', mode = if (plot.type == "line") "lines" else "lines+markers",
                       line = list(dash = line.style), name = group)
    } else if (plot.type == "point") {
      trace <- plot_ly(x = group_data[[xAxis.legend]], y = group_data[[yAxis.legend]], type = 'scatter', mode = 'markers',
                       marker = list(symbol = point.style), name = group)
    } else if (plot.type == "bar") {
      trace <- plot_ly(x = group_data[[xAxis.legend]], y = group_data[[yAxis.legend]], type = 'bar', name = group)
    }
    plotly_traces[[length(plotly_traces) + 1]] <- trace
  }
  
  # Combinar todas las trazas en un solo gráfico
  PLOT <- subplot(plotly_traces, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
  
  # Configurar títulos y ejes
  layout(PLOT, title = plot.title, xaxis = list(title = xAxis.legend, type = if (xAxis.log) 'log' else 'linear', autorange = if (xAxis.reverse) TRUE else NA),
         yaxis = list(title = yAxis.legend, type = if (yAxis.log) 'log' else 'linear', autorange = if (yAxis.reverse) TRUE else NA),
         showlegend = legend.show)
  
  PLOT <- plot_ly()  # Uso de PLOT como nombre de variable unificado
  class(PLOT) <- c("plotly", class(PLOT))  # Asignar clase para identificar la función generadora
  
  if (plot.save) {
    plotly::export(PLOT, file = "plot_plotly.png")
  }
  
  return(PLOT)
}
