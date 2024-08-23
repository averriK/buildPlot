devtools::load_all()
RMwTable <- readRDS("inst/RMwTable.Rds")
TR_TARGET <- 10000
Tn_TARGET <- 0
POE_TARGET <- (1 - exp(-50 / TR_TARGET)) |> round(digits = 5)
DT <- RMwTable[Tn == Tn_TARGET & POE == POE_TARGET, .(Mw, R, p=1000*p/0.01)]
# DT <- RMwTable[Tn == Tn_TARGET & POE == POE_TARGET, .(Mw, R, p)]

DATA <- DT[, .(Y = round(Mw,1), X = as.integer(R), Z = round(p,1))]


hist2D(data = DATA, 
       plot.type = "contour", #"heatmap","contour"
       yAxis.label = "Magnitude (Mw)", 
       nbins=20,
       xAxis.label = "Distance [km]", 
       yAxis.max = 8,
       xAxis.max = 200,
       axis.fontsize = "14px",
       legend.font = "Arial",
       plot.title = "3D Histogram of Seismic Data",
       title.fontsize = "24px",
       title.font = "Arial")
