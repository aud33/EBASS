#' @title A Reference Class to represent the lambda value
#' @description Lambda is known as the willingness to pay. That is the ceiling cost-effectiveness ratio or the maximum acceptable cost of a unit of effectiveness.
#' @name Lambda
## @description Lambda is known as the willingness to pay. That is the ceiling cost-effectiveness ratio or the maximum acceptable cost of a unit of effectiveness.
## It must be coherent with the criteria of effectiveness used in the analysis (year of life, QALY, life saved, or a criteria related to morbidity).
#' @field lambda : Lambda is a monetary value. For example, the value of lambda is usually between 20 000 and 40 000 pounds/QALY in UK.
#' @section Methods:
#'  \describe{
#'  \item{set_lambda(lambda):}{sets the lambda value of this Lambda object}
#'  }
#' @seealso \link{create_object_lambda} the constructor
## @noRd
setRefClass(
  # Nom de la classe
  "Lambda",
  # Attributs
  fields =  c(
    lambda = "numeric"),
  # Fonctions
  methods= list(
    ### constructeur :
    initialize = function(lambda){
      set_lambda (lambda)
    },
    get_lambda = function(){
      return(lambda)
    },

    ## setter
    set_lambda = function(lambda){
      check_1 (list(lambda=lambda))
      lambda <<- lambda
    }
  )
)

#' @title Create an object lambda
#' @description Lambda is known as the willingness to pay. That is the ceiling cost-effectiveness ratio or the maximum acceptable cost of a unit of effectiveness.
#' It must be coherent with the criteria of effectiveness used in the analysis (year of life, QALY, life saved, or a criteria related to morbidity).
#' @param lambda : Lambda is a monetary value. For example, the value of lambda is usually between 20 000 and 40 000 pounds/QALY in UK.
#' @return create_object_lambda returns an object of class \link{Lambda}
#' @examples
#' ## Create an object lambda
#' object_lambda <- create_object_lambda (20000)
#' ## retrieve the lambda value from the object
#' object_lambda$get_lambda()
#' @export

create_object_lambda <- function(lambda){
  object_lambda <- methods::new (Class="Lambda",lambda = lambda)
}
