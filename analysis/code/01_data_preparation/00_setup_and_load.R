#### setting up and loading ####

# setting some directories

data_directory <- "analysis/data/input_data/"
object_data_directory <- paste0(data_directory, "object_data/")
image_directory <- paste0(data_directory, "object_images/")
profile_image_directory <-
  paste0(image_directory, "profile_images/")
original_image_directory <-
  paste0(image_directory, "original_images/")
output_directory <- "analysis/data/output_data/"

# load the object data

total_data <-
  read.csv(file = paste0(object_data_directory, "object_data.csv"),
           row.names = 1)

plot_height <- 7
plot_width <- 7
plot_dpi <- 300

plot_options <- list(height = plot_height,
                     width = plot_width,
                     dpi = plot_dpi)
