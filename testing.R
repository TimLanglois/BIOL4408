library(RSQLite)
library(devtools)
library(bigrquery)
library(ggmap)

devtools::install_github("r-dbi/bigrquery")


devtools::install_github("dkahle/ggmap")

devtools::install_github("dkahle/ggmap", ref = "tidyup",force = TRUE)
