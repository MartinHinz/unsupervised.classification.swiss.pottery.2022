get_majority_cluster_solution <- function(co_assoc_matrix) {
  threshold <- 0.5
  co_assoc_matrix_processed <- co_assoc_matrix
  new_cluster_level <- 0
  new_cluster_solution <- vector(length = nrow(co_assoc_matrix))
  names(new_cluster_solution) <- rownames(co_assoc_matrix)

  co_assoc_above_threshold <- any(co_assoc_matrix_processed > threshold)

  while(co_assoc_above_threshold) {
    new_cluster_level <- new_cluster_level + 1
    first_incidence_row <- which(co_assoc_matrix_processed > threshold, arr.ind = T)[1,][1]

    if (is.na(first_incidence_row)) {
      co_assoc_above_threshold = FALSE
      break
    }

    first_incidence <- co_assoc_matrix_processed[first_incidence_row,]

    same_cluster <- first_incidence > threshold
    new_cluster_solution[names(same_cluster)] <- new_cluster_level

    co_assoc_matrix_processed <- co_assoc_matrix_processed[!same_cluster,!same_cluster]
    co_assoc_matrix_processed <- matrix(co_assoc_matrix_processed, nrow = sum(!same_cluster))

    colnames(co_assoc_matrix_processed) <- names(same_cluster)[!same_cluster]
    rownames(co_assoc_matrix_processed) <- names(same_cluster)[!same_cluster]

    co_assoc_above_threshold <- any(co_assoc_matrix_processed > threshold)
  }
  return(new_cluster_solution)
}

create_co_association_matrix <- function(co_clust_matrix) {

  co_assoc_matrix <- empty_co_assoc_matrix(co_clust_matrix)

  for (column in 1:ncol(co_clust_matrix)) {
    this_cluster_solution <- co_clust_matrix[,column]
    cluster_levels <- unique(this_cluster_solution)
    for(cluster in 1:length(cluster_levels)) {
      this_cluster_level <- cluster_levels[cluster]
      if (this_cluster_level == "0") {
        next
      }
      this_members <- this_cluster_solution == this_cluster_level
      co_assoc_matrix[this_members,this_members] <- co_assoc_matrix[this_members,this_members] + 1
    }
  }

  co_assoc_matrix <- co_assoc_matrix / ncol(co_clust_matrix)
  diag(co_assoc_matrix) <- 1
  return(co_assoc_matrix)
}

empty_co_assoc_matrix <- function(co_clust_matrix) {
  co_assoc_matrix <- matrix(0, nrow = nrow(co_clust_matrix),ncol = nrow(co_clust_matrix))
  co_clust_names <- rownames(co_clust_matrix)
  rownames(co_assoc_matrix) <- co_clust_names
  colnames(co_assoc_matrix) <- co_clust_names
  return(co_assoc_matrix)
}

get_tsne_hdbscan_result <- function(shapes, weights) {
  this_perplexity <- 30

  if (nrow(shapes) - 1 < 3 * this_perplexity) {this_perplexity <- floor((nrow(shapes) - 1)/3)}

  pca_weighted <- PCA(as.matrix(shapes), graph=F, col.w = weights)

  tsne_model_1 <- Rtsne(pca_weighted$ind$coord, pca = FALSE,
                        check_duplicates=FALSE,
                        perplexity=this_perplexity,
                        dims=2,
                        theta = 0)

  this_res_sne <- tsne_model_1$Y
  rownames(this_res_sne) <- rownames(shapes)

  hc <- hdbscan(this_res_sne, minPts = 5)
  hc$x <- this_res_sne[,1]
  hc$y <- this_res_sne[,2]
  return(hc)
}

get_hcpc_results <- function(shapes, ncp=8) {
  res.pca <- PCA(shapes, graph=FALSE, ncp = ncp)
  hc <- HCPC(res.pca, graph=F,nb.clust=-1, consol=F)
  return(hc)
}

get_cluster_assignments <- function(x) {
  object_class <- class(x)
  if(object_class=="HCPC") {
    RVA <- x$data.clust$clust
  } else if(object_class=="hdbscan") {
    RVA <- x$cluster
  }
  else {
    stop("wrong class of object")
  }
  return(RVA)
}

verify_clustering_method <- function(method) {
  METHODS <- c("hcpc","tsne-hdbscan")
  i.meth <- pmatch(method, METHODS)
  if (is.na(i.meth))
    stop("invalid clustering method", paste("", method))
  if (i.meth == -1)
    stop("ambiguous clustering method", paste("", method))
  method <- METHODS[i.meth]
  return(method)
}

get_cluster_results <- function(method, shapes, ncp, weights) {
  if(method=="hcpc") {
    hc <- get_hcpc_results(shapes,ncp=ncp)
  } else if(method=="tsne-hdbscan") {
    hc <- get_tsne_hdbscan_result(shapes, weights)
  } else {
    stop("unknown method")
  }
  return(hc)
}

empty_co_clust_matrix <- function(shapes, n_rep) {
  co_clust_matrix <- matrix(nrow = nrow(shapes), ncol=n_rep)
  rownames(co_clust_matrix) <- rownames(shapes)
  return(co_clust_matrix)
}

get_weights <- function(profile_object) {
  if("weights" %in% names(profile_object)) {
    weights <- profile_object$weights
  } else {
    weights <- rep(1,ncol(profile_object$data))
  }

  return(weights)
}

get_consensus <- function(co_clust_matrix) {

  # get the majority voting
  co_assoc_matrix <- create_co_association_matrix(co_clust_matrix)
  cluster_values <- get_majority_cluster_solution(co_assoc_matrix)

  # noise filter
  # whatever is below min_n_objects (default 5) will be send to cluster 0 (noise)

  min_n_objects <- 5
  table_cluster_values <- table(cluster_values)
  noise_cluster <- as.numeric(
    names(table_cluster_values)[table_cluster_values<min_n_objects]
    )

  cluster_values[cluster_values %in% noise_cluster] <- 0

  return(cluster_values)
}

select_from_profile_object <- function(object, selector) {
  this_clust_data <- object$data[selector,]
  this_clust_profiles <- object$profiles[selector,]
  this_profile_images_path <-
    object$profile_images_path[selector]
  this_original_images_path <-
    object$original_images_path[selector]

  RVA  <- list(
    data = this_clust_data,
    profiles = this_clust_profiles,
    profile_images_path = this_profile_images_path,
    original_images_path = this_original_images_path
  )

  return(RVA)

}

iterative_subcluster <- function(profile_object, path, it, ncp = 8, method="hcpc", n_rep = 11)
{
  # if this is the last iteration, than exit recursion
  if(it==0){return()}

  # distinguish between methods
  method <- verify_clustering_method(method)

  # go to the next iteration
  this_it <- it-1

  # get the data
  shapes <- profile_object$data

  # get or set default weights
  weights <- get_weights(profile_object)

  # if there are less then 8 objects in this cluster, exit recursion
  if(nrow(shapes)<8){return()}

  # use the following if you can not use parallel computing with fork clusters
  #
  # # create empty collector matrices
  # co_clust_matrix <- empty_co_clust_matrix(shapes, n_rep)
  #
  # # loop for n_rep to get the cluster results unparallel
  # for (repetion in 1:n_rep) {
  #  cluster_result_object <- get_cluster_results(method, shapes, ncp, weights)
  #  cluster_values <- get_cluster_assignments(cluster_result_object)
  #  co_clust_matrix[,repetion] <- cluster_values
  # }

  # loop for n_rep to get the cluster results using parallel computing

  # setup parallel backend to use many processors
  cores <- detectCores()
  cl <- makeForkCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)

  # setting some random seeds randomly
  # otherwise all parallel streams will have the same result
  r_seeds <- sample(1:1000,n_rep,replace=T)

  co_clust_matrix <-foreach(i = 1:n_rep, .combine = cbind) %dopar% {
   set.seed(r_seeds[i])
   cluster_result_object <- get_cluster_results(method, shapes, ncp, weights)
   cluster_values <- get_cluster_assignments(cluster_result_object)
   cluster_values
  }

  rownames(co_clust_matrix) <- rownames(shapes)

  stopCluster(cl)

  # using one result as structure for return object
  cluster_result_object <- get_cluster_results(method, shapes, ncp, weights)
  cluster_result_object$image_names <- row.names(shapes)
  cluster_result_object$data <- shapes
  cluster_result_object$cluster <- get_consensus(co_clust_matrix)

  # get the unique cluster classes
  classes <- unique(cluster_result_object$cluster)

  # identify the 'typical' vessel
  typical_images <- gettypicalimages(cluster_result_object, cluster_result_object$cluster)

  # consolidate the given file path
  current_path <- normalizePath(path)

  # Plot the cluster solution at the given path
  plotclustersolution(cluster_result_object, current_path)

  for(this_class in classes)
  {
    # skip the noise class
    if(this_class==0) {next}

    # create folder for class plots and data
    this_path <- file.path(current_path,this_class)
    dir.create(this_path)

    # copy the typical image to folder as 'node.png'
    this_typical_image <-
      typical_images[as.character(this_class)]

    this_typical_image_origin <-
      profile_object$original_images_path[this_typical_image]

    file.copy(this_typical_image_origin,
              paste(this_path, "node.png", sep = "/"))

    # filter profile object for class members
    this_clust_data <-
      select_from_profile_object(profile_object,
                                 cluster_result_object$cluster == this_class)

    # save class data to class folder
    saveRDS(this_clust_data,
            file = paste(this_path,
                         "this_data.rds",
                         sep = "/"))

    # recursive conduct procedure on subclass
    iterative_subcluster(this_clust_data,
                         path=this_path,
                         it=this_it,
                         method=method)
  }
}

make_graphviz <-function(this_dir){
  this_content <- dir(this_dir, recursive=T,include.dirs=T,full.names = T)
  this_content <- this_content[file.info(this_content)$isdir]
  this_content <- this_content[!endsWith(this_content,"vessels")]

  images <- sapply(this_content, function(x){
    readPNG(normalizePath(file.path(x, "node.png.out.png")))
  })

  nodes <- data.frame(id=this_content)
  edges <- data.frame(from=NA,to=NA)

  for(i in 1:length(this_content)) {
    this <- this_content[i]
    this_dir_content <- dir( path = this, recursive=F,include.dirs=T,full.names = T)
    this_dir_content <- as.vector(na.omit(this_dir_content[file.info(this_dir_content)$isdir]))
    this_dir_content <- this_dir_content[!endsWith(this_dir_content,"vessels")]
    if(length(this_dir_content)>0){
      edges <- rbind(edges, data.frame(from=this, to=this_dir_content))
    }
  }
  edges<-na.omit(edges)
  edges$from <- as.character(edges$from)
  edges$to <- as.character(edges$to)
  nodes$id <- as.character(nodes$id)
  edges$from <- gsub("//", "/", edges$from)
  edges$to <- gsub("//", "/", edges$to)
  nodes$id <- gsub("//", "/", nodes$id)

  g <- igraph::graph.data.frame(edges, directed=TRUE, vertices=nodes)

  igraph::V(g)$raster <- images

  pdf(normalizePath(file.path(this_dir,"graph.pdf"), mustWork = F), width = 29.7/2.54, height = 21/2.54)
  plot(g, layout=igraph::layout.fruchterman.reingold, vertex.shape = "raster", vertex.label = NA,
       vertex.size=10,vertex.size2=10, edge.arrow.size=0.5)
  dev.off()
 }

gettypicalimages <- function(hc, predefined_cluster_solution = NA) {
  object_class <- class(hc)
  if(object_class=="HCPC") {
    RVA <- gettypicalimages_hcpc(hc, predefined_cluster_solution)
  } else if (object_class=="hdbscan") {
    RVA <- gettypicalimages_hdbscan(hc, predefined_cluster_solution)
  }
  else {
    stop("wrong class of object")
  }
  return(RVA)
}

gettypicalimages_predefined <- function(hc, predefined_cluster_solution) {
  this_pca_solution <- PCA(hc$data, ncp = Inf, graph = FALSE)
  typical_images <- by(this_pca_solution$ind$coord, predefined_cluster_solution, gettypicalimages_predefined_per_cluster)
  cluster_names <- names(typical_images)
  typical_images <- t(sapply(typical_images, I))
  typical_images <- as.vector(unlist(typical_images))
  names(typical_images) <- cluster_names
  return(typical_images)
}

gettypicalimages_predefined_per_cluster <- function(pca_solution) {
  center <- colMeans(pca_solution)
  data_incl_center <- rbind(pca_solution, center)
  distance <- as.matrix(dist(data_incl_center))
  distance <- distance[(nrow(pca_solution) + 1):nrow(distance),
                       -((nrow(pca_solution) + 1):ncol(distance))]
  distance <- as.vector(distance)
  names(distance) <- rownames(pca_solution)
  distance <- sort(distance, decreasing = FALSE)
  return(names(distance)[1])
}

gettypicalimages_hcpc <- function(hc, predefined_cluster_solution = NA) {
  if (!all(is.na(predefined_cluster_solution))) {
    RVA <- gettypicalimages_predefined(hc, predefined_cluster_solution)
  } else {
    RVA <- unlist(lapply(hc$desc.ind$para, function(x) names(x[1])))
  }
  return(RVA)
}

gettypicalimages_hdbscan <- function(hc, predefined_cluster_solution = NA) {
  if (!all(is.na(predefined_cluster_solution))) {
    RVA <- gettypicalimages_predefined(hc, predefined_cluster_solution)
  } else {
    this_selection_object <- data.frame(probs = hc$membership_prob, image_names = hc$image_names, cluster = hc$cluster)
    this_selection_object <- this_selection_object[this_selection_object$cluster!=0,]
    this_clusters <- unique(this_selection_object$cluster)
    RVA <- vector()
    for (i in 1:length(this_clusters)) {
      this_cluster <- this_clusters[i]
      this_selection <- subset(this_selection_object, cluster == this_cluster)
      RVA[as.character(this_cluster)] <- as.character(this_selection$image_names[which.max(this_selection$probs)])
    }
  }
  return(RVA)
}

plotclustersolution <- function(hc, path) {
  object_class <- class(hc)
  if(object_class=="HCPC") {
    plotclustersolution_hcpc(hc)
  } else if(object_class=="hdbscan") {
    plotclustersolution_hdbscan(hc, path)
  }
  else {
    stop("wrong class of object")
  }
}

plotclustersolution_hcpc <- function(hc) {
  png(filename = "cluster_solution")
  plot(hc)
  dev.off()
}

plotclustersolution_hdbscan <- function(hc, path) {
  plotchoice <- data.frame(hc[c("x","y")])
  this_plot <- ggplot(data.frame(plotchoice, cluster = hc$cluster), aes(x=x,y=y, color=factor(cluster))) + geom_point()
  ggsave(this_plot, path = path, filename = "cluster_solution.png")
}
