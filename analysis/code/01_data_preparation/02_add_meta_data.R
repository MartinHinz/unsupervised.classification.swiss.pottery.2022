### adding meta data ###

# adding data on the objects that includes spatial and temporal informations

metadata <- read.csv2(paste0(object_data_directory, "metadata.csv"))

profile_object$metadata <-
  total_data %>% merge(metadata,
                       by.x = "keramik_id",
                       by.y = "Id",
                       sort = F) %>% select("Schicht_Einheit",
                                            "Kleinregion",
                                            "Zeitfenster_fein",
                                            "Zeitfenster_grob")

rownames(profile_object$metadata) <- rownames(total_data)

### cleaning up ###

rm(metadata)
