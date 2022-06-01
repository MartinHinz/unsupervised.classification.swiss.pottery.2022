purrr::walk(
  list.files("analysis/code/03_visualisation", full.names = T),
  function(y) {
    message("\n###### ", y, " ######\n")
    source(y)
    rm(list = ls())
  }
)
