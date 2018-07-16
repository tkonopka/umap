## package umap
## calculations via the umap python package
##
## part of this implementation was inspired by https://github.com/ropenscilabs/umapr



##' Create a umap embedding using a python package
##'
##' @keywords internal
##' @param d data object
##' @param config list with settings
##'
##' @return list, one element of which is matrix with embedding coordinates
umap.learn = function(d, config) {

  ## abort if python package is not available
  if (is.null(python.umap)) {
    umap.error("python package umap is not available")
  }

  ## get an update config object that includes a vector of arguments for umap-learn
  config = detect.umap.learn(config)

  message.w.date(paste0("calling umap-learn (v", config$umap_learn_version, ")"),
                 config$verbose)
  message.w.date(paste0("setting arguments: ", length(config$umap_learn_args)),
                 config$verbose)
  
  ## adjust values in config to please python type checkers
  if (is.na(config$random_state)) {
    config$random_state= as.integer(stats::runif(1, 0, 2^30))
  }
  config$verbose = 0+as.logical(config$verbose)

  ## construct python object and create embedding
  UMAP = do.call(python.umap$UMAP, config[config$umap_learn_args])
  embedding = UMAP$fit_transform(d)
  message.w.date("done", config$verbose)

  list(layout=embedding, config=config)
}




##' adjust config depending on umap-learn version
##'
##' @keywords internal
##' @param config list with settings
##'
##' @return config list with set umap_learn_args
detect.umap.learn = function(config) {
  
  if (!identical(config$umap_learn_args, NA)) {
    return (config)
  }
  
  learn.version = as.character(python.umap$"__version__")
  config$umap_learn_version = learn.version
  ## get a main version number from the detail version
  main.version = paste(strsplit(learn.version, "\\.")[[1]][1:2], collapse=".")
  
  args.version = list(
    "0.2" = c("n_neighbors", "n_components","metric", "n_epochs", "alpha",
                "init", "spread", "min_dist", "set_op_mix_ratio",
                "local_connectivity", "bandwidth", "gamma", "negative_sample_rate",
                "a", "b", "random_state", "verbose"),
    "0.3" = c("n_neighbors", "n_components", "metric", "n_epochs", "learning_rate",
                "init", "min_dist", "spread", "set_op_mix_ratio",
                "local_connectivity", "repulsion_strength", "negative_sample_rate",
                "transform_queue_size", "a", "b", "random_state",
                "angular_rp_forest", "target_n_neighbors", "target_metric",
                "target_weight", "transform_seed", "verbose")
  )
  args.version$latest = args.version[["0.3"]]

  if (main.version %in% names(args.version)) {    
    config$umap_learn_args = args.version[[main.version]]
  } else {
    warning(paste0("cannot recognize umap-learn version ", learn.version,
                   "; set umap_learn_args manually"))
    config$umap_learn_args = args.version$latest
  }
  config$umap_learn_args = intersect(config$umap_learn_args, names(config))
  
  config
}

