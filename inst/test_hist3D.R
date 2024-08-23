devtools::load_all()
RMwTable <- readRDS("inst/RMwTable.Rds")
TR_TARGET <- 10000
Tn_TARGET <- 0
POE_TARGET <- (1 - exp(-50 / TR_TARGET)) |> round(digits = 5)
DT <- RMwTable[Tn == Tn_TARGET & POE == POE_TARGET, .(Mw, R, p)]
# DT <- RMwTable[Tn == Tn_TARGET & POE == POE_TARGET, .(Mw, R, p)]

DATA <- DT[, .(Y = round(Mw,1), X = as.integer(R), Z = round(100000*p/0.01,0))]


hist3D(data = DATA, 
                  nbins = 10, 
                  bin.width = 0.4, 
                  yAxis.label = "Magnitude (Mw)", 
                  xAxis.label = "Distance [km]", 
                  zAxis.label = "1000 x poe [%]", 
                  yAxis.max = 8,
                  xAxis.max = 200,
                  xAxis.legend = TRUE, 
                  yAxis.legend = TRUE, 
                  xAxis.tickangle = -45,
                  yAxis.tickangle = 45,
                  axis.fontsize = "14px",
                  legend.font = "Arial",
                  legend.valign = "top",
                  caption = "This is a sample caption at the bottom of the plot",
                  plot.title = "3D Histogram of Seismic Data",
                  title.fontsize = "24px",
                  title.font = "Arial")