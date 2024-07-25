devtools::load_all()
DT <- data.frame(ID=iris$Species,X=iris$Sepal.Length,Y=iris$Sepal.Width)
buildPlot(data=DT,library="highcharter",plot.type="scatter")
# buildPlot(data=DT,library="highcharter")
