purrr::walk(
  list.files("analysis/code/01_data_preparation", full.names = T),
  function(y) {
    message("\n###### ", y, " ######\n")
    source(y)
    rm(list = ls())
  }
)
