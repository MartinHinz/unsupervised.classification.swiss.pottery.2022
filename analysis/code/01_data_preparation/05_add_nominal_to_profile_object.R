### add nominal data ###

# adding the nominal data from the total data to the profile object
# the list of variables to include is loaded from a csv file

nominal_data <-
  unlist(read.csv(
    paste0(
      object_data_directory,
      "nominal_variables_for_clustering.csv"
    ),
    row.names = 1,
    stringsAsFactors = F
  ))

# subsetting the total data to the columns containing the nominal data

nominal_for_clustering <-
  total_data[, nominal_data]

# copying the profile object and adding the nominal data

profile_object_with_nominal <- profile_object

profile_object_with_nominal$data <-
  merge(profile_object_with_nominal$data,
        nominal_for_clustering,
        by = "row.names")

row.names(profile_object_with_nominal$data) <-
  profile_object_with_nominal$data$Row.names

# removing superfluous column Row.names

profile_object_with_nominal$data <-
  profile_object_with_nominal$data[, -1]

### cleaning up ###

rm(nominal_for_clustering, nominal_data)
