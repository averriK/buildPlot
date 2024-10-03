library(devtools)
library(roxygen2)
library(usethis)
file.remove("NAMESPACE") |> suppressWarnings()
# usethis::use_author(
#   given="Alejandro",
#   family = "Verri Kozlowski",
#   role = c("aut", "cre"),
#   email= "averri@fi.uba.ar",
#   comment = c(ORCID = "0000-0002-8535-1170")
# )
usethis::use_gpl_license(version = 3, include_future = TRUE)
devtools::check(document=TRUE)
remove.packages("buildPlot")
## Commit Push
devtools::install()
remotes::install_github("averriK/buildPlot")

