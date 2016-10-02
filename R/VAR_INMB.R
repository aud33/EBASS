#' @title A Reference Class to represent the Hypothetical variance of the Incremental Net Monetary Benefit
#' @description Hypothetical variance of the Incremental Net Monetary Benefit.
#' @name VAR_INMB
## @description Hypothetical variance of the Incremental Net Monetary Benefit.
## If data are available this variance can be calculated based of the common standard deviation of costs in each group (sdc),
## the common standard deviation of effectiveness in each group (sde), lambda (\link{create_object_lambda}),
## and the coefficient of correlation (rho) between the difference in costs (dc) and the difference in effectiveness (de)
#' @field sdc : common standard deviation of costs in each group
#' @field sde : common standard deviation of effectiveness in each group
#' @field rho : coefficient of correlation between the difference in costs (dc) and the difference in effectiveness (de)
#' @field object_lambda : an object lambda.
#' Create one with \link{create_object_lambda}.
#' It contains lambda : the ceiling cost-effectiveness ratio or maximum acceptable cost of a unit of effectiveness
#' @section Methods:
#'  \describe{
#'  \item{set_sdc(sdc):}{Sets the common standard deviation of costs in each group for this VAR_INMB object}
#'  \item{set_sde(sde):}{Sets the common standard deviation of effectiveness in each group for this VAR_INMB object}
#'  \item{set_rho(rho):}{Sets the coefficient of correlation between the difference in costs (dc) and the difference in effectiveness (de)}
#'  \item{set_object_lambda(object_lambda):}{Sets the object_lambda of this VAR_INMB object}
#'  \item{get_var_inmb():}{Return the calculated hypothetical variance of the Incremental Net Monetary Benefit (INMB)}

#'  }
#' @seealso \link{create_object_var_inmb_direct} to directly provide a value for the variance of the Incremental Net Monetary Benefit
#' @seealso \link{create_object_var_inmb_diff} to calculate the theoretical standard deviation of the expected INB with different standard deviation in the reference and the experimental group
#' @seealso \link{create_object_var_inmb} the constructor
#' @include VAR_INMB_DIRECT.R
#' @examples
#' ## First, create a lambda object
#' object_lambda <- create_object_lambda (20000)
#' ## Then, create a var_inmb object
#' var_inmb <- create_object_var_inmb(sde=0.12, sdc=2100, rho=0.1, object_lambda=object_lambda)
#' var_inmb$get_var_inmb()
## @noRd

setRefClass(
  # Nom de la classe
  "VAR_INMB",
  # Attributs
  fields =  c(
    sde = "numeric",
    sdc = "numeric",
    rho="numeric",
    object_lambda = "ANY"
  ),

  # Fonctions :
  methods=list(
    ### Constructeur
    initialize = function(sde,sdc,rho,object_lambda){
      check_heritage (object_lambda, "Lambda")
      object_lambda <<- object_lambda
      check_1(list(sde=sde,sdc=sdc,rho=rho))
      check_positif(list(sde=sde, sdc=sdc))
      check_rho(rho)
      sde <<- sde
      sdc <<- sdc
      rho <<- rho
      set_var_inmb()
    },

    ### recalcul la variance du bni
    set_var_inmb = function(){
      var_inmb <<- 2 * (object_lambda$get_lambda()^2 * sde^2 + sdc^2 -
                                   2*object_lambda$get_lambda() * rho * sde * sdc)
    },

    set_sdc = function(sdc){
      check_1(list(sdc=sdc))
      check_positif(list(sdc=sdc))
      sdc <<- sdc
      set_var_inmb()
    },

    set_sde = function(sde){
      check_1(list(sde=sde))
      check_positif(list(sde=sde))
      sde <<- sde
      set_var_inmb()
    },

    set_rho = function(rho){
      check_1(list(rho=rho))
      check_rho(rho)
      rho <<- rho
      set_var_inmb()
    },

    ### var_inmb est calculee avant d'etre envoyee
    get_var_inmb = function(){
      set_var_inmb()
      return(var_inmb)
    },

    ### var_inmb_exp = var_inmb_ref = var_inmb / 2 quand ecart-type des couts et efficacite communs
    get_var_inmb_exp = function(){
      set_var_inmb()
      return (var_inmb/2)
    },

    get_var_inmb_ref = function(){
      set_var_inmb()
      return (var_inmb/2)
    },

    set_object_lambda = function(object_lambda){
      check_heritage (object_lambda, "Lambda")
      object_lambda <<- object_lambda
      set_var_inmb()
    }
  ),
  # Set the inheritance for this class
  contains = "VAR_INMB_DIRECT"
)

#' @title Create an object var_inmb
#' @description Hypothetical variance of the Incremental Net Monetary Benefit.
#' If data are available this variance can be calculated based of the common standard deviation of costs in each group (sdc),
#' the common standard deviation of effectiveness in each group (sde), lambda (\link{create_object_lambda}),
#' and the coefficient of correlation (rho) between the difference in costs (dc) and the difference in effectiveness (de)
#' @param sdc : common standard deviation of costs in each group
#' @param sde : common standard deviation of effectiveness in each group
#' @param rho : coefficient of correlation between the difference in costs (dc) and the difference in effectiveness (de)
#' @param object_lambda : an object lambda.
#' Create one with \link{create_object_lambda}.
#' It contains lambda : the ceiling cost-effectiveness ratio or maximum acceptable cost of a unit of effectiveness
#' @seealso \link{create_object_var_inmb_direct} to directly provide a value for the variance of the Incremental Net Monetary Benefit
#' @seealso \link{create_object_var_inmb_diff} to calculate the theoretical standard deviation of the expected INB with different standard deviation in the reference and the experimental group
#' @return create_object_var_inmb returns an object of class \link{VAR_INMB} which inherits from the class \link{VAR_INMB_DIRECT}
#' @examples
#' ## First, create a lambda object
#' object_lambda <- create_object_lambda (20000)
#' ## Then, create a var_inmb object
#' var_inmb <- create_object_var_inmb(sde=0.12, sdc=2100, rho=0.1, object_lambda=object_lambda)
#' var_inmb$get_var_inmb()
#' @export

create_object_var_inmb <- function(sdc, sde, rho, object_lambda){
  var_inmb <- methods::new(Class="VAR_INMB", sdc=sdc, sde = sde, rho = rho,
                 object_lambda = object_lambda)
}
