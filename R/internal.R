#' Check rho value
#' @param rho : expected correlation of the difference in cost (dc) and effect (de)
#' @noRd
check_rho <- function(rho){
  if (rho < -1 | rho > 1){
    stop("rho is out of bounds.")
  }
  return (T)
}

#' Check if the length of each variable is one
#' @param ... : a list of variables
#' @noRd
#' @return TRUE if all variable are length one ; else return an error
check_1 <- function(...){
  if (!is.list(...)){
    stop("Arguments must be passed in a list")
  }
  liste <- c (...)
  tests <- lapply (liste, function(x){
    if(length(x)== 0 ) {
      nom <- deparse(substitute(x))
      return ("is not set")
    }
    if(length(x)!= 1 ) {
      nom <- deparse(substitute(x))
      return("length is not equal to 1")
    }
    return(T)
  })

  tests <- paste (names(tests), unlist(tests), sep=" ")

  bool <- grepl("TRUE",tests)
  if (all(bool)){
    return(T)
  } else {
    tests <- tests[!bool]
    tests <- paste (tests, collapse="\n")
    stop (tests)
  }
}

#' Check if each varaible is positif
#' @param ... : une liste de variables
#' @noRd
#' @return TRUE if all variables passed as arguments are positives ; else return an error
check_positif <- function(...){
  if (!is.list(...)){
    stop("Arguments must be passed in a list")
  }
  liste <- c (...)
  positif <- lapply (liste, function(x){
    return (x >= 0)
  })
  positif <- unlist(positif)
  if (all(positif)){
    return(T)
  }
  ## si c'est faux :
  bool <- positif == F
  messages <- paste (names(positif[bool]), "must be positif", collapse = "\n")
  stop(messages)
}

#' Check if each variable is integer
#' @param ... : une liste de variables
#' @noRd
#' @return TRUE if all variables passed as arguments are integers ; else return an error
check_entier <- function(...){
  if (!is.list(...)){
    stop("Arguments must be passed in a list")
  }
  liste <- c (...)
  positif <- lapply (liste, function(x){
    return (x %% 1 == 0)
  })
  positif <- unlist(positif)
  if (all(positif)){
    return(T)
  }
  ## si c'est faux :
  bool <- positif == F
  messages <- paste (names(positif[bool]), "is not an integer", collapse = "\n")
  stop(messages)
}


#' Check if an object inherits another object
#' @param objet : the object to check
#' @param object_name : the name of the object it should inherit
#' @noRd
#' @return TRUE if it inherits ; else return an error
check_heritage <- function(objet, object_name){
  if (!inherits(objet,object_name)){
    stop(deparse(substitute(objet)), " is not an instance of the ", object_name, " class")
  } else {
    return(T)
  }
}
