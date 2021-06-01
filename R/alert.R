#' @export
parmesan_alert <- function(parmesan = NULL,
                           r = NULL,
                           env = parent.frame(),
                           panic = FALSE){

  # Alerts do not work with reactive parmesan/presets
  if(shiny::is.reactive(parmesan))
    return()

  if(is.null(r)){
    # message("IN ALERT")
    # fenv(env)
    funs_in_env <- as.vector(lsf.str(envir = env))
    # funs_in_env used for tests, list env funs
    # may be used later for non-reactive funs defined in server
    reacts_in_env <- which_shiny_reactives(env = env) %||% funs_in_env
  } else {
    reacts_in_env <- names(isolate(reactiveValuesToList(r)))
  }

  which_parmesan_reactives <- which_parmesan_reactives(parmesan)
  if(!all(which_parmesan_reactives %in% reacts_in_env)){
    msg <- paste0(
      "Reactives not found in parmesan environment: ",
      paste0(which_parmesan_reactives[!which_parmesan_reactives %in% reacts_in_env],
             collapse = ", "),"\n",
      "  Reactives in parmesan: ",
      paste0(which_parmesan_reactives,collapse = ", "),"\n",
      "  Reactives in App: ",
      paste0(reacts_in_env,collapse = ", "),"\n"
    )
    if(panic) stop(msg)
    message(msg)
  }
}


which_shiny_reactives <- function(env = parent.frame()){
  # message("which shiny reactives env: ", capture.output(print(env)))
  # message("IN WHICH SHINY REACTIVE")
  # fenv(env)
  x <- ls(env, all = TRUE)
  # message("ls which shiny reactives: ",paste(x, collapse = ", "))
  x <- Filter(function(x) "reactive" %in% class(get(x, envir = env)),x)
  # message("ls Filter reactives: ",paste(x, collapse = ", "))
  # x <- x[!grepl("parmesan_input", x)]
  as.vector(x)
}

fenv <- function(env = parent.frame(), msg = "", silent = FALSE){
  ls(env, all.names = TRUE)
  if(silent) return()
  message(msg, "\n",
          "Current environment", capture.output(print(env)),"\n",
          paste0(ls(env, all.names = TRUE), collapse = ", "),"\n",
          "Parent environment", capture.output(print(parent.env(env))),"\n",
          paste0(ls(parent.env(env), all.names = TRUE), collapse = ", "),"\n"
  )
}

#' Show all reactive elements of parmesan object
#'
#' Returns all reactive elements from parmesan object,
#' including parameters, conditions (in show_if statements),
#' and reactive info tooltip text.
#'
#' @param parmesan Parmesan object
#'
#' @return Character vector of all reactive strings.
#' @export
which_parmesan_reactives <- function(parmesan){
  if(is.null(parmesan)){
    parmesan <- parmesan_load()
  }
  # inputs <- yaml::read_yaml(file.path(path, "inputs.yaml"))
  inputs <- parmesan
  inps <- unlist(inputs)
  reactives <- unname(inps[grepl("\\(\\)",unname(inps))])
  nms <- unlist(strsplit(names(inps), "\\."))
  reactives <- c(reactives, nms[grepl("\\(\\)",unname(nms))])
  gsub("\\(\\)", "", reactives)
}
