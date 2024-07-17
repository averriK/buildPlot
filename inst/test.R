devtools::load_all()
DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
buildPlot(data=DT,library="ggplot2", legend.layout="horizontal",legend.align = "left", legend.valign = "center")
# buildPlot(data=DT,library="highcharter")
