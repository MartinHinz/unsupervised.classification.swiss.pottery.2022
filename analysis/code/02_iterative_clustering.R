purrr::walk(
  list.files("analysis/code/02_iterative_clustering", full.names = T),
  function(y) {
    message("\n###### ", y, " ######\n")
    source(y)
    rm(list = ls())
  }
)
