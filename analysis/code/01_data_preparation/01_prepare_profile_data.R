### prepare profile data ###

# using the function ´import_profile_data´ from the import_functions.R
# it transforms the scanned and pretreated profile images
# into vectors of profile coordinates

profile_object <-
  import_profile_data(
    profile_images_directory = profile_image_directory,
    original_images_directory = original_image_directory,
    profile_images_suffix = "_sh"
  )
