### install EBImage, if necessary ###

# EBImage is installed via the Bioconductor package manager (BiocManager)

if (!require("EBImage", character.only = TRUE)) {
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

  BiocManager::install("EBImage")
}

require("EBImage", character.only = TRUE)

### install other package dependencies, if necessary ###

package_dependencies <- c(
  "tidyverse",
  "FactoMineR",
  "Rtsne",
  "dbscan",
  "grid",
  "png",
  "foreach",
  "doParallel",
  "magrittr",
  "dplyr",
  "igraph"
  )

for (package in package_dependencies){
  if (!require(package, character.only = TRUE)) install.packages(package)
  require(package, character.only = TRUE)
}

### load the helper functions ###

source("analysis/code/00_helper_functions/import_functions.R")
source("analysis/code/00_helper_functions/analytical_functions.R")
source("analysis/code/00_helper_functions/plot_functions.R")

### cleaning up ###

rm(package_dependencies, package)
