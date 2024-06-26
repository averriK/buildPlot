% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildPlot.ggplot2.R
\name{buildPlot.ggplot2}
\alias{buildPlot.ggplot2}
\title{plot.ggplot2}
\usage{
buildPlot.ggplot2(
  data,
  plot.object = NA,
  plot.title = NA,
  plot.subtitle = NA,
  plot.height = NA,
  plot.width = NA,
  xAxis.legend = "X",
  yAxis.legend = "Y",
  group.legend = "ID",
  color.palette = "viridis",
  plot.type = "line",
  line.style = "solid",
  line.size = 1,
  point.size = 2,
  point.style = "circle",
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
  legend.align = "right",
  legend.valign = "top",
  legend.show = TRUE,
  plot.save = FALSE,
  plot.theme = theme_bw()
)
}
\arguments{
\item{data}{data.frame}

\item{plot.object}{ggplot2 object}

\item{plot.title}{character}

\item{plot.subtitle}{character}

\item{plot.height}{numeric}

\item{plot.width}{numeric}

\item{xAxis.legend}{character}

\item{yAxis.legend}{character}

\item{group.legend}{character}

\item{color.palette}{character}

\item{plot.type}{character c("line","spline","point","column","bar")}

\item{line.style}{character c("solid","dashed","dotted","dotdash","longdash","twodash")}

\item{line.size}{numeric}

\item{point.size}{numeric}

\item{point.style}{character c("circle","square","diamond","triangle","triangle-down")}

\item{xAxis.log}{boolean}

\item{yAxis.log}{boolean}

\item{xAxis.reverse}{boolean}

\item{yAxis.reverse}{boolean}

\item{xAxis.max}{numeric}

\item{yAxis.max}{numeric}

\item{xAxis.min}{numeric}

\item{yAxis.min}{numeric}

\item{xAxis.label}{boolean}

\item{yAxis.label}{boolean}

\item{legend.layout}{character}

\item{legend.align}{character}

\item{legend.valign}{character}

\item{legend.show}{boolean}

\item{plot.save}{boolean}

\item{plot.theme}{ggplot2 theme}
}
\value{
ggplot2 object
}
\description{
plot.ggplot2
}
\examples{
data(iris)
DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
buildPlot.ggplot2(DT)

}
