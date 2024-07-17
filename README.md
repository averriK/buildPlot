
# buildPlot

## Overview
`buildPlot` is an R package designed to facilitate the creation of various types of plots using `ggplot2`, `highcharter`, or `plotly`. The package provides a single function, `buildPlot()`, which allows users to specify the desired plotting library and customize their plots with a wide range of parameters.

## Installation
To install the development version of `buildPlot` from GitHub, use the following commands:

```R
# Install devtools if you haven't already
install.packages("devtools")

# Install buildPlot from GitHub
devtools::install_github("averriK/buildPlot")
```

## Function

### buildPlot
Creates customized plots using `ggplot2`, `highcharter`, or `plotly` based on the provided parameters.

#### Usage:
```R
buildPlot(data, library = "ggplot2", ...)
```
- `data`: A data frame containing the variables to plot.
- `library`: The plotting library to use (one of `"ggplot2"`, `"highcharter"`, `"plotly"`).
- `...`: Additional arguments specific to the plot type and library.

#### Example:
```R
library(buildPlot)

# Create a plot using ggplot2
DT <- data.frame(ID = iris$Species, X = iris$Sepal.Length, Y = iris$Sepal.Width)
buildPlot(data = DT, library = "ggplot2", plot.type = "line", plot.title = "Sepal Dimensions")

# Create a plot using highcharter
buildPlot(data = DT, library = "highcharter", plot.type = "scatter", plot.title = "Sepal Dimensions")

# Create a plot using plotly
buildPlot(data = DT, library = "plotly", plot.type = "bar", plot.title = "Sepal Dimensions")
```

### Plot Customization
The `buildPlot` function allows extensive customization through additional parameters, such as:

- `plot.title`: Title of the plot.
- `plot.subtitle`: Subtitle of the plot.
- `plot.height`, `plot.width`: Dimensions of the plot.
- `xAxis.legend`, `yAxis.legend`: Labels for the x and y axes.
- `plot.type`: Type of plot (e.g., `"line"`, `"scatter"`, `"bar"`).
- `color.palette`: Color palette for the plot.
- `line.style`, `point.style`: Styles for lines and points.
- `line.size`: Size of lines in the plot.
- `point.size`: Size of points in the plot.
- `xAxis.log`, `yAxis.log`: Use logarithmic scale for x and y axes.
- `xAxis.reverse`, `yAxis.reverse`: Reverse the x and y axes.
- `xAxis.max`, `yAxis.max`: Maximum values for the x and y axes.
- `xAxis.min`, `yAxis.min`: Minimum values for the x and y axes.
- `xAxis.label`, `yAxis.label`: Show or hide labels for the x and y axes.
- `legend.layout`: Layout of the legend (e.g., `"horizontal"`, `"vertical"`).
- `legend.align`: Alignment of the legend (e.g., `"center"`, `"left"`, `"right"`).
- `legend.valign`: Vertical alignment of the legend (e.g., `"top"`, `"middle"`, `"bottom"`).
- `legend.show`: Show or hide the legend.
- `plot.theme`: Theme for the plot.
- `plot.save`: Save the plot.

Each plotting library (`ggplot2`, `highcharter`, `plotly`) has specific options and features that can be customized through these parameters.

## License
This package is licensed under the GPL 3.0 License.

## Contributing
Contributions are welcome! If you would like to contribute to this package, please fork the repository, create a new branch, and submit a pull request with your changes. For major changes, please open an issue first to discuss what you would like to change.

## Contact
For any questions or issues, please contact the maintainer:
- Name: Alejandro Verri Kozlowski
- Email: averri@fi.uba.ar
