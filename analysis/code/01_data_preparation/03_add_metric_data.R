### add metric data ###

# adding the metric data from the total data to the profile object

metric_for_clustering <-
  total_data[, c("keramik_H_max", "dm_1", "dm_2")]
profile_object$data <-
  merge(profile_object$data, metric_for_clustering, by = "row.names")
row.names(profile_object$data) <- profile_object$data$Row.names
profile_object$data <- profile_object$data[, -1]

### cleaning up ###

rm(metric_for_clustering)
