devtools::install_github("r-lib/devtools")

getwd()
library(devtools)
find_rtools()

library(roxygen2)

devtools::build()
devtools::load_all()
devtools::document()
devtools::test()
devtools::build_vignettes()

usethis::use_testthat()

usethis::use_vignette("detailed_example")

usethis::create_package("./koboloops")
system("defaults write org.R-project.R force.LANG en_US.UTF-8")
