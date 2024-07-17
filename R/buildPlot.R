#' Title
#'
#' @param data data.frame
#' @param library character c("ggplot2","gg","highcharter","hc","plotly")
#' @param ... extra arguments
#'
#' @return plot object
#' @export buildPlot
#'
#' @examples
#' DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
#' buildPlot(data=DT)
#' 
#' buildPlot(data=DT,library="highcharter")
#' 
buildPlot <- function(data, library = "ggplot2", ...) {
  # Verificar que 'data' sea un data.frame
  if (!is.data.frame(data)) {
    stop(".x must be a dataframe data.frame")
  }
  
  if (library %in% c("ggplot2","gg")) {
    class(data) <- c("ggplot2", class(data))
    buildPlot.ggplot2(data=data, ...)
  } else if (library  %in% c("highcharter","highchart","hc")) {
    class(data) <- c("highcharter", class(data))
    buildPlot.highcharter(data=data, ...)
  } else if (library %in% c("py","plotly")) {
    class(data) <- c("plotly", class(data))
    buildPlot.plotly(data=data,...)
  } else {
    stop("Unknown library")
  }
}
