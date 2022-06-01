### consolidating data ###

# to make sure that profile objects contain only those data congruent with those were
# data, all individual datasets in the profile object are subsetted those in the total_data
# based on the row names

for (element in c("profiles", "data", "metadata")) {
  profile_object[[element]] <-
    profile_object[[element]][rownames(total_data), ]
}

for (element in c("profile_images_path", "original_images_path")) {
  profile_object[[element]] <-
    profile_object[[element]][rownames(total_data)]
}

### cleaning up ###

rm(element)
