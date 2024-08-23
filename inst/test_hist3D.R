devtools::load_all()
RMwTable <- readRDS("inst/RMwTable.Rds")
TR_TARGET <- 10000
Tn_TARGET <- 0
POE_TARGET <- (1 - exp(-50 / TR_TARGET)) |> round(digits = 5)
# DT <- RMwTable[Tn == Tn_TARGET & POE == POE_TARGET, .(Mw, R, p=1000*p/0.01)]
DT <- RMwTable[Tn == Tn_TARGET & POE == POE_TARGET, .(Mw, R, p)]

DATA <- DT[, .(Y = round(Mw,1), X = round(R/10), Z = p)]
hist3D(data = DATA, 
       nbins = 15, 
       bin.width = 0.4, 
       color.palette = hcl.pals()[112], 
       yAxis.label = "Magnitude (Mw)", 
       xAxis.label = "Distance [x10 km]", 
       yAxis.max = 8,
       xAxis.max = 20,
       aspect.ratio=list(x = 1, y = 1, z = 1.5),
       xAxis.legend = TRUE, 
       yAxis.legend = TRUE, 
       zAxis.legend = FALSE, 
       xAxis.tickangle = 45,
       yAxis.tickangle = -45,
       axis.fontsize = "14px",
       legend.font = "Arial",
       legend.valign = "top",
       caption = "This is a sample caption at the bottom of the plot",
       plot.title = "3D Histogram of Seismic Data",
       title.fontsize = "24px",
       title.font = "Arial")
