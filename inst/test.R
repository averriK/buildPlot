devtools::load_all()
library(ggplot2)
library(scales)
custom_colors <- c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c", "#34495e", 
                   "#3498db", "#1abc9c", "#f39c12", "#d35400")

scale_color_flat <- function(...) {
  scale_color_manual(values = custom_colors, ...)
}

scale_fill_flat <- function(...) {
  scale_fill_manual(values = custom_colors, ...)
}

# Example of applying the custom theme and color scale

ggplot(mpg, aes(displ, hwy, color = class)) +
  geom_point(size = 3) +
  labs(title = "Fuel Efficiency by Engine Size",
       subtitle = "Data from the ggplot2 package",
       x = "Engine displacement (liters)",
       y = "Highway miles per gallon",
       color = "Car class") +
  theme_flat() +
  scale_color_flat()