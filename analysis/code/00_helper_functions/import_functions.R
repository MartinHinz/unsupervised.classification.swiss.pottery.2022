# read an profile image using the EBImage library
readProfileImage <- function(x) {
  out <- EBImage::readImage(x)
  out <- EBImage::channel(out,"gray")
  out <- t(EBImage::imageData(out))
  out[out<1] <- 0
  out
}

# get a named vector of image paths
get_image_path_vector <- function(images_directory, names, basenames) {
  images_path <- normalizePath(file.path(images_directory, names))
  
  names(images_path) <- basenames
  
  return(images_path)
}

# get a matrix of shape measurements from images in path
get_shapes_matrix <- function(profile_images_path) {
  n_images <- length(profile_images_path)
  
  first_image <- readProfileImage(profile_images_path[1])
  
  image_rows <- nrow(first_image)
  
  shapes<-matrix(ncol=image_rows,nrow=n_images) # collection matrix for the shapes
  
  for (i in 1:n_images) # for each image
  {
    # add the individual shape vector to the collection matrix
    shapes[i,]<-get_individual_shape_vector(profile_images_path[i],
                                            image_rows)
  }
  
  return(shapes)
}

# get the profile shape representation of an individual image
get_individual_shape_vector <- function(this_file, image_rows) {
  image<-readProfileImage(this_file) # read current picture
  
  imagemat <- cbind(rep(1,times=image_rows),image)
  
  res <- dim(imagemat)[1:2]
  shape<-res[2]-apply(imagemat,1,function(x){max(which(x>0))}) # for each image row getz the first profile point
  return(shape)
}

#' Imports profile images for further analysis
#'
#' Imports profile images for further analysis.
#'
#' @param profile_images_directory the directory where the profile images are stored
#' @param original_images_directory the directory where the original images are stored
#' @param profile_images_suffix the suffix of the profile images if there is one
#'
#' @return A list with the profile informations of the images and the paths to the
#' profile and the original images
#' @importFrom png readPNG
#' @export

import_profile_data <- function(profile_images_directory,
                                original_images_directory = NA,
                                profile_images_suffix = "") {
  if(missing(original_images_directory)) {original_images_directory <- profile_images_directory}

  profile_images_directory <- normalizePath(profile_images_directory)
  original_images_directory <- normalizePath(original_images_directory)

  files_profile_path <- dir(profile_images_directory)

  profile_extension <- paste(profile_images_suffix,".png", sep = "")

  profile_names <- files_profile_path[grepl(profile_extension, files_profile_path)]

  profiles_basenames <- sub(profile_extension,"",profile_names)

  orig_names <- paste(profiles_basenames,".png", sep="")

  profile_images_path <- get_image_path_vector(profile_images_directory, profile_names, profiles_basenames)

  original_images_path <- get_image_path_vector(original_images_directory, orig_names, profiles_basenames)

  shapes <- get_shapes_matrix(profile_images_path)
  rownames(shapes) <- profiles_basenames #rownames equals basename
  
  profile_object <- list(profiles = shapes,
                         data = shapes,
                         profile_images_path = profile_images_path,
                         original_images_path = original_images_path)
  
  return(profile_object)
}
