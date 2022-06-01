### Initial first clustering ###

# according to the research design, clustering takes place
# at the highest level without the nominal data,
# based solely on metric data (height, diameter 1 & 2 and profile shape)

# setting the number of repetitions of the clustering (101)
# for the consensus solution
# in any case should this number be odd,
# so that a majority voting can be achieved
n_rep <- 101

# starting from scratch be deleting the output directory
unlink(output_directory, recursive = T)
dir.create(output_directory)

# using the profile object data (without nominal variables)
this_clust_data <- profile_object

# save the data for later use
saveRDS(this_clust_data, file = paste0(output_directory, "this_data.rds"))

# defining the method
# options are
# "tsne" for tsne-hdbscan
# "hcpc" for hierarchical clustering on pca
method <- "tsne"

# initiating the subcluster routine on the top hierarchical level
iterative_subcluster(
  #profile_object = profile_object,
  profile_object = profile_object_with_nominal,
  path = output_directory,
  it = 1,
  method = method,
  n_rep = n_rep
)
