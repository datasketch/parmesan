
parmesan_load <- function(path = "parmesan"){

  if(!dir.exists(path)){
    stop("Parmesan folder not found")
  }

  inputs <- yaml::read_yaml(file.path(path, "inputs.yaml"))
  if(file.exists(file.path(path, "layout.yaml"))){
    layout <- yaml::read_yaml(file.path(path, "layout.yaml"))
  }else{
    layout <- list(section = names(inputs))
  }
  parmesan <- layout
  section_ids <- names(layout)
  parmesan <- lapply(seq_along(layout), function(j){
    # x <- layout[[2]]
    x <- list(id = section_ids[j])
    x <- c(x, layout[[j]])
    inputs <- inputs[x$inputs]
    x$inputs <- lapply(seq_along(inputs), function(i){
      input <- list(id = x$inputs[i])
      input <- c(input, inputs[[i]])
      input
    })
    x
  })
  names(parmesan) <- section_ids
  # parmesan$env <- new.env(parent = parent.frame(5))
  # parmesan$env <- new.env(parent = .GlobalEnv)
  # parmesan$env <- new.env(parent = environment())
  parmesan
}

