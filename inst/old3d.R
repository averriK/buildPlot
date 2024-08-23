 {
  
  X_bin <- Y_bin <- NULL
  
  # Create a copy of the data to avoid modifying the original data table
  DT <- copy(data)
  
  # Set default color palette if none is provided
  if(is.null(color.palette)){
    color.scale <- hcl.colors(6, palette = hcl.pals()[6])  
    
  }
  
 
  
  # Define the function to add 3D bars
  add_3Dbar <- function(p, x, y, z, width = bin.width) {
    w <- width
    add_trace(p, type = "mesh3d",
              x = c(x - w, x - w, x + w, x + w, x - w, x - w, x + w, x + w),
              y = c(y - w, y + w, y + w, y - w, y - w, y + w, y + w, y - w),
              z = c(0, 0, 0, 0, z, z, z, z),
              i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
              j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
              k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
              facecolor = rep(toRGB(color.scale), each = 2),

              hoverinfo = 'skip')  # Disable hover info to prevent showing non-real values
  }
  
  # Draw the 3D histogram
  fig <- plot_ly()
  for (k1 in 1:nrow(z_mtx)) {
    for (k2 in 1:ncol(z_mtx)) {
      if (z_mtx[k1, k2] > 0) {
        fig <- fig %>% add_3Dbar(k1, k2, z_mtx[k1, k2])
      }
    }
  }
  
  # Create a list for annotations
  annotations_list <- list()
  
  # Add title as an annotation if provided
  if (!is.null(plot.title)) {
    annotations_list <- append(annotations_list, list(
      list(
        text = plot.title,
        x = 0.5,
        y = 1.1,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = title.fontsize, family = title.font)
      )
    ))
  }
  
  # Add caption as an annotation if provided
  if (!is.null(caption)) {
    annotations_list <- append(annotations_list, list(
      list(
        text = caption,
        x = 0.5,
        y = -0.3,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = axis.fontsize, family = legend.font)
      )
    ))
  }
  # Determine decimal places for X and Y data
  x_decimals <- get_decimal_places(DT$X)
  y_decimals <- get_decimal_places(DT$Y)
  
  
  
  ticktext_x = seq(Xmin, Xmax, length.out = nbins) |> round(x_decimals)  # Label ticks with the correct X-axis values
  ticktext_y = seq(Ymin, Ymax, length.out = nbins) |> round(y_decimals)
  
  
  z_min <- min(z_mtx)
  z_max <- max(z_mtx)
  # Customize the layout for better visualization
  fig <- fig %>% layout(
    scene = list(
      xaxis = list(
        title = list(text = xAxis.label, font = list(size = axis.fontsize, family = legend.font), standoff = 10),  # Set the X axis title
        showticklabels = xAxis.legend,
        tickangle = xAxis.tickangle,  # Apply tick angle to X axis ticks
        zeroline = TRUE,
        tickvals = seq(0, nbins - 1),  # Use 0 to nbins-1 for internal tick positions
        ticktext = ticktext_x,
        gridcolor = "darkgrey",
        gridwidth = 0.5,
        titlefont = list(size = axis.fontsize, family = legend.font),  # Set font for X axis title
        titleangle = 0  # Set angle of X axis title to horizontal
      ),
      yaxis = list(
        title = list(text = yAxis.label, font = list(size = axis.fontsize, family = legend.font), standoff = 10),  # Set the Y axis title
        showticklabels = yAxis.legend,
        tickangle = yAxis.tickangle,  # Apply tick angle to Y axis ticks
        zeroline = TRUE,
        tickvals = seq(0, nbins - 1),  # Use 0 to nbins-1 for internal tick positions
        ticktext = ticktext_y,
        gridcolor = "darkgrey",
        gridwidth = 0.5,
        titlefont = list(size = axis.fontsize, family = legend.font),  # Set font for Y axis title
        titleangle = 0  # Set angle of Y axis title to horizontal
      ),
      zaxis = list(
        title = list(text = zAxis.label, font = list(size = axis.fontsize, family = legend.font), standoff = 10),  # Set the Z axis title
        showticklabels = zAxis.legend,
        tickangle = zAxis.tickangle,  # Apply tick angle to Z axis ticks
        zeroline = TRUE,
        tickvals = z_ticks |> prettyNum(),
        gridcolor = "darkgrey",
        gridwidth = 0.5,
        titlefont = list(size = axis.fontsize, family = legend.font),  # Set font for Z axis title
        titleangle = 45  # Set angle of Z axis title to horizontal
      ),
      aspectmode = "manual",
      aspectratio = list(x = 1, y = 1, z = 1)  # Ensure equal scaling
    ),
    annotations = annotations_list,
    margin = list(l = 0, r = 0, b = 100, t = 50)
  )
  
  # Display the plot
  return(fig)
}


get_decimal_places <- function(x) {
  if (all(x == round(x))) {
    return(0)
  } else {
    return(max(nchar(strsplit(as.character(x), "\\.")[[1]][2]), na.rm = TRUE))
  }
}