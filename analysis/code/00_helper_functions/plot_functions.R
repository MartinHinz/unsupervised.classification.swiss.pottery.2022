### actual plot functions ###

# Boxplot of the absolute heights
create_absolute_height_boxplot <-
  function(this_total_data, this_directory) {
    ggplot(data.frame(height = this_total_data$keramik_H_max)) +
      geom_boxplot(aes(x = "vessels", y = height))
    
    ggsave(file.path(this_directory,
                     "absolute_height_boxplot.pdf"))
    
  }

# Histogram of the absolute heights
create_absolute_height_hist_density_plot <-
  function(this_total_data, this_directory) {
    ggplot(data.frame(height = this_total_data$keramik_H_max),
           aes(x = height)) +
      geom_histogram(aes(y = ..density..), bins = 30) +
      geom_density(aes(y = ..density..))
    
    ggsave(file.path(this_directory, "absolute_height_hist_density.pdf"))
  }

# Boxplot and histogram of the absolute heights
create_absolute_height_plots <-
  function(this_total_data, this_directory) {
    create_absolute_height_boxplot(this_total_data, this_directory)
    
    create_absolute_height_hist_density_plot(this_total_data, this_directory)
  }

# Representation of the 'typical' profile line with confidence intervals
# (1 standard deviation and 90% interval)
create_mean_plot <-
  function(this_total_data,
           this_directory,
           plot_options) {
    this_profile_stats <- this_total_data %>%
      select(profile) %>%
      unnest() %>%
      group_by(y) %>%
      summarize(
        mean = mean(x),
        sd = sd(x),
        quantile1 = quantile(x, probs = 0.05),
        quantile2 = quantile(x, probs = 0.95)
      )
    
    mean_plot <- ggplot(data = this_profile_stats) +
      geom_path(aes(y = y, x = mean)) +
      geom_path(aes(x = mean - sd, y = y), lty = 2) +
      geom_path(aes(x = mean + sd, y = y), lty = 2) +
      geom_path(aes(x = quantile1, y = y), lty = 2, alpha = 0.5) +
      geom_path(aes(x = quantile2, y = y), lty = 2, alpha = 0.5) +
      coord_fixed()  + ylab("height") + xlab("width")
    
    ggsave(
      file.path(this_directory, "mean_plot.pdf"),
      plot = mean_plot,
      height = plot_options$height,
      width = plot_options$width,
      dpi = plot_options$dpi
    )
  }

profile_set_plot <- function(this_data, category = NA) {
  if (is.na(category)) {
    my_aes <- aes_string(x = "x", y = "y", group = "ind_nr")
  }
  else {
    my_aes <- aes_string(
      x = "x",
      y = "y",
      group = "ind_nr",
      color = category
    )
  }
  if (is.na(category) || !(all(is.na(this_total_data[category]))))
    my_plot <- ggplot(this_data, my_aes) + geom_path() +
      coord_fixed() + ylab("width") + xlab("height")
  
  return(my_plot)
}

# Representation of all profile lines of vessels, colored according
# to the given categorical data
create_profile_set_plots <-
  function(this_total_data,
           relevant_info,
           this_directory,
           plot_options) {
    lapply(c(NA, relevant_info), function(category) {
      this_profile_set <-
        profile_set_plot(unnest(this_total_data), category) + ylab("height") + xlab("width")
      
      ggsave(
        file.path(
          this_directory,
          paste("profile_set_plot_colored_", category, ".pdf", sep = "")
        ),
        plot = this_profile_set,
        height = plot_options$height,
        width = plot_options$width,
        dpi = plot_options$dpi
      )
    })
    
  }

# Representation of the number of vessels according to categorical data
# as a bar chart
create_bar_plots <-
  function(this_total_data,
           relevant_info,
           this_directory,
           plot_options) {
    lapply(c(NA, relevant_info), function(category) {
      this_barplot <- ggplot(this_total_data) +
        geom_bar(aes_string(x = category)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggsave(
        file.path(this_directory,
                  paste(category,
                        "_count.pdf",
                        sep = "")),
        plot = this_barplot,
        height = plot_options$height,
        width = plot_options$width,
        dpi = plot_options$dpi
      )
    })
  }

annotate_cluster_images <- function(x) {
  annotation_custom(
    x[[1]],
    xmin = x[[2]],
    ymin = x[[3]],
    xmax = x[[4]],
    ymax = x[[5]]
  )
}

# create an annotation object for ggplot, so that the images of the
# 'typical' vessel is used as icon for the subcluster
create_subcluster_annote_object <-
  function(this_cluster_subcluster_directories,
           this_subclusters_total_data)
  {
    typical_images <-
      file.path(this_cluster_subcluster_directories, "node.png")
    names(typical_images) <-
      basename(this_cluster_subcluster_directories)
    
    cluster_annote <- list()
    
    cluster_levels <-
      as.character(unique(this_subclusters_total_data$cluster))
    
    for (t in 1:length(cluster_levels)) {
      this_image_path <- normalizePath(typical_images[cluster_levels[t]])
      this_image <-
        rasterGrob(readPNG(this_image_path, native = T), interpolate = FALSE)
      this_positions <- c(t - .25,
                          0.2,
                          t + .25,
                          0.8)
      cluster_annote[[t]] <- list(
        this_image,
        xmin = this_positions[1],
        ymin = this_positions[2],
        xmax = this_positions[3],
        ymax = this_positions[4]
      )
    }
    
    cluster_annote_object <-
      lapply(cluster_annote, annotate_cluster_images)
    return(cluster_annote_object)
  }

# Barplots of the distribution of the categorical data to the subclusters
# (incl. NA)
create_subcluster_bar_plots <- function(this_subclusters_total_data,
                                        relevant_info,
                                        this_directory,
                                        plot_options,
                                        subcluster_annote_object,
                                        filename_suffix = "_bar_stacked.pdf")
{
  lapply(relevant_info, function(category) {
    this_stacked_baseplot <- ggplot(this_subclusters_total_data) +
      geom_bar(aes_string(x = "cluster", fill = category),
               position = "fill") +
      subcluster_annote_object
    
    ggsave(
      file.path(
        this_directory,
        paste("cluster_", category, "_bar_stacked_incl_na.pdf", sep =
                "")
      ),
      plot = this_stacked_baseplot,
      height = plot_options$height,
      width = plot_options$width,
      dpi = plot_options$dpi
    )
  })
}

# Barplots of the distribution of the categorical data to the subclusters
# (excl. the category NA)
create_subcluster_bar_plots_exclude_na <-
  function(this_subclusters_total_data,
           relevant_info,
           this_directory,
           plot_options,
           subcluster_annote_object,
           filename_suffix = "_bar_stacked.pdf")
  {
    lapply(relevant_info, function(category) {
      this_stacked_baseplot <-
        ggplot(subset(
          this_subclusters_total_data,
          !is.na(this_subclusters_total_data[, category])
        )) +
        geom_bar(aes_string(x = "cluster", fill = category),
                 position = "fill") +
        subcluster_annote_object
      
      ggsave(
        file.path(
          this_directory,
          paste("cluster_", category, "_bar_stacked_excl_na.pdf", sep =
                  "")
        ),
        plot = this_stacked_baseplot,
        height = plot_options$height,
        width = plot_options$width,
        dpi = plot_options$dpi
      )
    })
  }

# Distribution of the subcluster to the individual sites
create_wide_cluster_sites_plot <-
  function(this_subclusters_total_data,
           this_directory,
           plot_options,
           subcluster_annote_object)
  {
    wide_cluster_sites_plot <- ggplot(this_subclusters_total_data) +
      geom_bar(aes_string(x = "cluster", fill = "sites"),
               position = "fill") +
      subcluster_annote_object
    
    ggsave(
      file.path(this_directory, "wide_cluster_sites_plot.pdf"),
      plot = wide_cluster_sites_plot,
      height = plot_options$height,
      width = plot_options$width * 4,
      dpi = plot_options$dpi
    )
  }

# Distribution of the absolute height to the individual
# sub-clusters as a box plot
create_height_per_subcluster_plot <-
  function(this_subclusters_total_data,
           this_directory,
           plot_options,
           subcluster_annote_object) {
    height_per_subcluster_plot <- ggplot(this_subclusters_total_data) +
      geom_boxplot(aes(x = cluster, y = keramik_H_max)) +
      subcluster_annote_object
    
    ggsave(
      file.path(this_directory, "height_per_subcluster_plot.pdf"),
      plot = height_per_subcluster_plot,
      height = plot_options$height,
      width = plot_options$width,
      dpi = plot_options$dpi
    )
  }

### plot helper functions ###

# gather subclusters from directory hierarchy
get_subcluster_directories <- function(this_directory) {
  exclude_directories <- c("vessels", "report")
  this_cluster_subcluster_directories <- list.dirs(this_directory,
                                                   recursive = F,
                                                   full.names = T)
  select_directories <-
    !(basename(this_cluster_subcluster_directories) %in% exclude_directories)
  this_cluster_subcluster_directories <-
    this_cluster_subcluster_directories[select_directories]
  return(this_cluster_subcluster_directories)
}

gather_original_images <- function(this_directory,
                                               original_images_path) {
  dir.create(file.path(this_directory, "vessels"))
  all_copied <-
    file.copy(original_images_path, file.path(this_directory, "vessels"))
  if (!all(all_copied)) {
    stop(paste0(
      "not all original images could be copied for directory ",
      this_directory
    ))
  }
}

add_metadata <- function(this_total_data, this_cluster_data) {
  this_cluster_data_names <- rownames(this_cluster_data$data)
  
  this_total_data$region <-
    this_cluster_data$metadata[this_cluster_data_names, "Kleinregion"]
  this_total_data$zeitfenster_fein <-
    this_cluster_data$metadata[this_cluster_data_names, "Zeitfenster_fein"]
  this_total_data$zeitfenster_grob <-
    this_cluster_data$metadata[this_cluster_data_names, "Zeitfenster_grob"]
  
  this_total_data$profile <-
    lapply(1:nrow(this_total_data), function(x) {
      tibble(y = ncol(this_cluster_data$profiles):1,
             x = this_cluster_data$profiles[x, ])
    })
  
  return(this_total_data)
}

# gather all informations from the subclusters
gather_subcluster_informations <-
  function(this_cluster_subcluster_directories,
           this_total_data,
           selected_columns) {
    this_subclusters_total_data <- tibble()
    
    this_subclusters_total_data <-
      this_cluster_subcluster_directories %>%
      lapply(
        . %>% normalizePath %>%
          file.path("this_data.rds") %>%
          readRDS %$% profiles %>%
          row.names %>%
          this_total_data[., selected_columns]
      ) %>%
      bind_rows(.id = "cluster")
    
    tmp_data <-
      left_join(this_subclusters_total_data, this_total_data, by = "ind_nr")
    
    this_subclusters_total_data$cluster <-
      factor(basename(this_cluster_subcluster_directories)[as.numeric(this_subclusters_total_data$cluster)])
    
    return(this_subclusters_total_data)
  }
