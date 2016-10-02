#' @title A Reference Class to represent the theoretical standard deviation of the expected INB
#' @description When absolutely no data regarding the variability of costs and effectiveness are available, it is possible to directly provide a value for the variance of the Incremental Net Monetary Benefit in this object.
#' @name VAR_INMB_DIRECT
## @description When absolutely no data regarding the variability of costs and effectiveness are available, it is possible to directly provide a value for the variance of the Incremental Net Monetary Benefit in this object.
#' @field var_inmb : variance of the Incremental Net Monetary Benefit
#' @section Methods:
#'  \describe{
#'  \item{set_var_inmb(inmb):}{sets the var_inmb for this VAR_INMB_DIRECT object}
#'  }
#' @seealso \link{create_object_var_inmb_diff} to calculate the theoretical standard deviation of the expected INB with different standard deviation in the reference and the experimental group
#' @seealso \link{create_object_var_inmb} to calculate the theoretical standard deviation of the expected INB with the same standard deviation in the reference and the experimental group
#' @seealso \link{create_object_var_inmb_direct} the constructor
#' @examples
#' ## Create a var_inmb object :
#' object_var_inmb <- create_object_var_inmb_direct(var_inmb = 18324000)
#' ## retrieve the inmb value from the object
#' object_var_inmb$get_var_inmb()
## @noRd

setRefClass(
  # the name for the class
  "VAR_INMB_DIRECT",

  # Attributs
  fields =  c(
    var_inmb = "numeric"
  ),

  methods= list (
    ### constructeur
    initialize = function(var_inmb){
      set_var_inmb(var_inmb)
    },

    ### getter
    get_var_inmb = function(){
      return(var_inmb)
    },

    ### var_inmb_exp = var_inmb_ref = var_inmb / 2 quand ecart-type des couts et efficacite communs
    get_var_inmb_exp = function(){
      return (var_inmb/2)
    },

    get_var_inmb_ref = function(){
      return (var_inmb/2)
    },

    ### setter
    set_var_inmb = function(var_inmb){
      check_1(list(var_inmb=var_inmb))
      check_positif(list(var_inmb=var_inmb))
      var_inmb <<- var_inmb
    }
  )
)

#' @title Create an object var_inmb_direct
#' @description When absolutely no data regarding the variability of costs and effectiveness are available, it is possible to directly provide a value for the variance of the Incremental Net Monetary Benefit in this object.
#' @param var_inmb : variance of the Incremental Net Monetary Benefit
#' @seealso \link{create_object_var_inmb_diff} to calculate the theoretical standard deviation of the expected INB with different standard deviation in the reference and the experimental group
#' @seealso \link{create_object_var_inmb} to calculate the theoretical standard deviation of the expected INB with the same standard deviation in the reference and the experimental group
#' @examples
#' ## Create a var_inmb object :
#' object_var_inmb <- create_object_var_inmb_direct(var_inmb = 18324000)
#' ## retrieve the inmb value from the object
#' object_var_inmb$get_var_inmb()
#' @return create_object_var_inmb_direct returns an object of class \link{VAR_INMB_DIRECT}
#' @export

create_object_var_inmb_direct <- function(var_inmb){
  var_inmb <- methods::new (Class="VAR_INMB_DIRECT",var_inmb = var_inmb)
}
