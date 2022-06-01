### sub clustering ###

# The clusters found in the first clustering will be
# iteratively further subdivided. The nominal characteristics
# are now taken into account. For this purpose, a file system hierarchy
# is created that represents the clustering.

# Read the directory structure that was created when the first cluster was generated.
sub_dirs <- dir(output_directory,
                include.dirs = T,
                full.names = T)
sub_dirs <- sub_dirs[file.info(sub_dirs)$isdir]

# For each subdirectory (=subcluster), the data stored there
# (containing the information for the associated objects) is loaded,
# the profile object is limited to these objects and then
# the cluster process is triggered again,
# for 2 further hierarchy levels (by the parameter it = 2).
for (i in 1:length(sub_dirs)) {
  this_dir <- normalizePath(file.path(sub_dirs[i]))

  this_clust_data <- readRDS(file.path(this_dir, "this_data.rds"))

  this_vessels <- row.names(this_clust_data$data)

  this_profile_object <- profile_object_with_nominal

  this_profile_object$profiles <-
    this_profile_object$profiles[this_vessels, ]

  this_profile_object$weights <- NULL

  this_profile_object$auswertungseinheiten <-
    this_profile_object$auswertungseinheiten[this_vessels, ]

  this_profile_object$data <-
    this_profile_object$data[this_vessels, ]

  this_profile_object$profile_images_path <-
    this_profile_object$profile_images_path[this_vessels]

  this_profile_object$original_images_path <-
    this_profile_object$original_images_path[this_vessels]

  iterative_subcluster(
    profile_object = this_profile_object,
    it = 2,
    path = this_dir,
    method = method,
    n_rep = n_rep
  )
}

### cleaning up ###

rm(sub_dirs, this_vessels, this_dir, n_rep, method, i)
