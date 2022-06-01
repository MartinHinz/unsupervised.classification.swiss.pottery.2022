### calculating weightings for the variables ###

# the weight for every group of variables equals to 1
# for every variable in the group the individual weight is
# equal to the 1 / number of variables in the group

variables <-
  read.csv(paste0(object_data_directory, "variable_names_groups.csv"),
           row.names = 1)

variables %<>% group_by(variable_group) %>%
  mutate(weight = 1 / n()) %>%
  as_tibble() %>%
  select(variable, weight)

variables <- data.frame(variables)
rownames(variables) <- variables$variable

profile_object_with_nominal$weights <-
  variables[colnames(profile_object_with_nominal$data), "weight"]

### cleaning up ###

rm(variables)
