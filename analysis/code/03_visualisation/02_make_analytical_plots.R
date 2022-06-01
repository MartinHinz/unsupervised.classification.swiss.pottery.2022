### make the analytical plots ###

# The following code serves for the visualization of the characteristic data of
# the resulting clusters and their subdivision into subclusters. The directory
# hierarchy of the operating system is used for the cluster structure. This is
# initially loaded, then the variables are defined which will be used as
# categorical data for the individual plots.
#
# For each subdirectory, the data is imported, the original images belonging to
# the cluster are copied to a 'vessels' directory, and the following information
# is then visualized:
#
# * Boxplot and histogram of the absolute heights
# * Representation of the 'typical' profile line with confidence intervals
#   (1 standard deviation and 90% interval)
# * Representation of all profile lines of vessels, colored according
#   to the given categorical data
# * Representation of the number of vessels according to categorical data
#   as a bar chart
#
# Furthermore, the division into the respective immediate subcluster is
# visualized. The following representations are generated after the
# corresponding data has been collected:
#
# * Barplots of the distribution of the categorical data to the subclusters,
#   incl. and excl. the category NA
# * Distribution of the subcluster to the individual sites
# * Distribution of the absolute height to the individual
#   sub-clusters as a box plot
#
# All plots are exported as PDF files to the respective directory.

dirs <- list.dirs(output_directory, recursive = T)

# defining categorical data to be used

relevant_info <- c(
  "sites",
  "stilgruppe",
  "bodenform",
  "grundform",
  "form"
)

# columns to be selected for the subcluster data
selected_columns <- c(relevant_info, "ind_nr", "profile", "keramik_H_max")

# for all (sub)directories
for (this_directory in dirs) {

  # read in the data of the cluster
  this_directory <- normalizePath(this_directory)
  this_cluster_data <-
    readRDS(file.path(this_directory, "this_data.rds"))

  # copy the original images
  gather_original_images(this_directory,
                         this_cluster_data$original_images_path)

  # subsetting the total data to the ones included in the cluster
  this_total_data <-
    total_data[rownames(this_cluster_data$data), ]

  # if dataset is empty, skip further processing
  if (!(nrow(this_total_data) > 0))
    next

  # adding meta data
  this_total_data <-
    add_metadata(this_total_data,
                 this_cluster_data)

  # Boxplot and histogram of the absolute heights
  create_absolute_height_plots(this_total_data,
                               this_directory)

  # Representation of the 'typical' profile line with confidence intervals
  # (1 standard deviation and 90% interval)
  create_mean_plot(this_total_data,
                   this_directory,
                   plot_options)

  # Representation of all profile lines of vessels, colored according
  # to the given categorical data
  create_profile_set_plots(this_total_data,
                           relevant_info,
                           this_directory,
                           plot_options)

  # Representation of the number of vessels according to categorical data
  # as a bar chart
  create_bar_plots(this_total_data,
                   relevant_info,
                   this_directory,
                   plot_options)

  ## visualising subcluster distribution

  # gather subclusters from directory hierarchy
  this_cluster_subcluster_directories <-
    get_subcluster_directories(this_directory)

  # if no subcluster, skip further processing
  if (!(length(this_cluster_subcluster_directories) > 0))
    next

  # gather all informations from the subclusters
  this_subclusters_total_data <-
    gather_subcluster_informations(this_cluster_subcluster_directories,
                                   this_total_data,
                                   selected_columns)

  # create an annotation object for ggplot, so that the images of the
  # 'typical' vessel is used as icon for the subcluster
  subcluster_annote_object <-
    create_subcluster_annote_object(this_cluster_subcluster_directories,
                                    this_subclusters_total_data)

  # Barplots of the distribution of the categorical data to the subclusters
  # incl. and excl. the category NA
  create_subcluster_bar_plots(
    this_subclusters_total_data,
    relevant_info,
    this_directory,
    plot_options,
    subcluster_annote_object
  )

  create_subcluster_bar_plots_exclude_na(
    this_subclusters_total_data,
    relevant_info,
    this_directory,
    plot_options,
    subcluster_annote_object
  )

  # Distribution of the subcluster to the individual sites
  create_wide_cluster_sites_plot(
    this_subclusters_total_data,
    this_directory,
    plot_options,
    subcluster_annote_object
  )

  # Distribution of the absolute height to the individual
  # sub-clusters as a box plot
  create_height_per_subcluster_plot(
    this_subclusters_total_data,
    this_directory,
    plot_options,
    subcluster_annote_object
  )
}
