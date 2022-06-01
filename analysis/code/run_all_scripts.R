purrr::walk(
  c("analysis/code/00_load_functions_and_packages.R",
    "analysis/code/01_run_data_preparation.R",
    "analysis/code/02_iterative_clustering.R",
    "analysis/code/03_visualisation.R"),
  function(y) {
    message("\n###### ", y, " ######\n")
    source(y)
    rm(list = ls())
  }
)
