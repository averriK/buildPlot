
#' Title
#'
#' @param data data.table
#' @param nbins integer
#' @param bin.width numeric
#' @param xAxis.label string
#' @param yAxis.label string
#' @param zAxis.label string
#' @param xAxis.min numeric
#' @param xAxis.max numeric
#' @param yAxis.min numeric
#' @param yAxis.max numeric
#' @param xAxis.legend boolean
#' @param yAxis.legend boolean
#' @param zAxis.legend boolean
#' @param xAxis.tickangle numeric
#' @param yAxis.tickangle numeric
#' @param zAxis.tickangle numeric
#' @param axis.fontsize string
#' @param legend.font string
#' @param legend.valign string
#' @param color.palette palette
#' @param caption string
#' @param plot.title string
#' @param title.fontsize string
#' @param title.font string
#'
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom plotly toRGB
#' @import data.table
#' @import grDevices
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
hist3D <- function(data, 
                   nbins = 15, 
                   bin.width = 0.4, 
                   xAxis.label = "x", 
                   yAxis.label = "y", 
                   zAxis.label = "z", 
                   xAxis.min = NULL, 
                   xAxis.max = NULL, 
                   yAxis.min = NULL, 
                   yAxis.max = NULL, 
                   xAxis.legend = TRUE, 
                   yAxis.legend = TRUE, 
                   zAxis.legend = FALSE,
                   xAxis.tickangle = 0,
                   yAxis.tickangle = 0,
                   zAxis.tickangle = 0,
                   axis.fontsize = "14px",
                   legend.font = "Arial",
                   legend.valign = "top",
                   color.palette = "Viridis", 
                   caption = NULL,
                   plot.title = NULL,
                   title.fontsize = "24px",
                   title.font = "Arial") {
  
  X_bin <- Y_bin <- NULL
  
  # Create a copy of the data to avoid modifying the original data table
  DT <- copy(data)
  
  
  # Set axis limits based on the provided data or user input
  Xmin <- if (!is.null(xAxis.min)) xAxis.min else min(DT$X)
  Xmax <- if (!is.null(xAxis.max)) xAxis.max else max(DT$X)
  Ymin <- if (!is.null(yAxis.min)) yAxis.min else min(DT$Y)
  Ymax <- if (!is.null(yAxis.max)) yAxis.max else max(DT$Y)
  
  # Create bins for X and Y
  DT[, X_bin := cut(X, breaks = seq(Xmin, Xmax, length.out = nbins + 1), labels = FALSE, include.lowest = TRUE, right = FALSE)]
  DT[, Y_bin := cut(Y, breaks = seq(Ymin, Ymax, length.out = nbins + 1), labels = FALSE, include.lowest = TRUE, right = FALSE)]
  
  
  # Create a matrix for Z values (accumulated probabilities)
  z_mtx <- matrix(0, nrow = nbins, ncol = nbins)
  for (i in 1:nrow(DT)) {
    z_mtx[DT$Y_bin[i], DT$X_bin[i]] <- z_mtx[DT$Y_bin[i], DT$X_bin[i]] + DT$Z[i]
  }
  
  # Define the Z-axis tick values
  Zmin <- min(z_mtx)
  Zmax <- max(z_mtx)
  z_ticks <- seq(Zmin, Zmax, length.out = nbins)
  
  # Draw the 3D histogram
  fig <- plot_ly()

  # Draw the 3D histogram with the updated function
  fig <- plot_ly()
  for (k1 in 1:nrow(z_mtx)) {
    for (k2 in 1:ncol(z_mtx)) {
      if (z_mtx[k1, k2] > 0) {
        fig <- add_3Dbar(
          p = fig,
          x = k1,
          y = k2,
          z = z_mtx[k1, k2],
          bin.width = bin.width,
          z_min = Zmin,
          z_max = Zmax,
          fixed_color_scale = fixed_color_scale
        )
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
  
  
  
  ticktext_x = seq(Xmin, Xmax, length.out = nbins) |> round(x_decimals)  
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

# Generate a fixed set of six colors
fixed_color_scale <- hcl.colors(6, palette = "Viridis")

# Function to map z values to one of the six colors
get_color_for_z <- function(z, z_min, z_max, fixed_color_scale) {
  # Normalize z to a 0-5 range
  normalized_z <- (z - z_min) / (z_max - z_min) * 5
  color_index <- round(normalized_z) + 1  # Map to an index between 1 and 6
  return(plotly::toRGB(fixed_color_scale[color_index]))  # Convert to RGB
}

# Modified add_3Dbar function
add_3Dbar <- function(p, x, y, z, bin.width, z_min, z_max, fixed_color_scale) {
  w <- bin.width
  
  # Get the color for the current z value, converted to RGB
  column_color <- get_color_for_z(z, z_min, z_max, fixed_color_scale)
  
  # Create the mesh3d for each column with facecolor in RGB format
  fig <- plotly::add_trace(p, type = "mesh3d",
                           x = c(x - w, x - w, x + w, x + w, x - w, x - w, x + w, x + w),
                           y = c(y - w, y + w, y + w, y - w, y - w, y + w, y + w, y - w),
                           z = c(0, 0, 0, 0, z, z, z, z),
                           i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
                           j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
                           k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
                           facecolor = rep(column_color, each = 2),  # Apply color to all faces in RGB
                           hoverinfo = 'skip')  # Disable hover info to prevent showing non-real values
  
  return(fig)
}

get_decimal_places <- function(x) {
  if (all(x == round(x))) {
    return(0)
  } else {
    return(max(nchar(strsplit(as.character(x), "\\.")[[1]][2]), na.rm = TRUE))
  }
}