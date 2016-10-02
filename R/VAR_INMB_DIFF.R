#' @title A Reference Class to represent the variance of the Incremental Net Monetary Benefit (INMB) when the standard deviation of costs and effectiveness in each group differ.
#' @name VAR_INMB_DIFF
#' @description The variance of the Incremental Net Monetary Benefit may also be calculated in a hypothetical situation when the standard deviation of costs and effectiveness in each group differ.
#' @name VAR_INMB_DIFF
## @description Theoretical standard deviation of the expected INB is calculated with
## different standard deviation in the reference and the experimental group

#' @field sdc_ref : standard deviation of costs in the reference group
#' @field sdc_exp : standard deviation of costs in the experimental group
#' @field sde_exp : standard deviation of effectiveness in the experimental group
#' @field sde_ref : standard deviation of effectiveness in the reference group
#' @field rho : coefficient of correlation between the difference in costs (dc) and the difference in effectiveness (de)
#' @field object_lambda : object containing the ceiling cost-effectiveness ratio or maximum acceptable cost of a unit of effectiveness. See \link{create_object_lambda}
#' @seealso \link{create_object_var_inmb_direct} to directly provide a value for the variance of the Incremental Net Monetary Benefit
#' @seealso \link{create_object_var_inmb} to calculate the theoretical standard deviation of the expected INB with the same standard deviation in the reference and the experimental group
#' @seealso \link{create_object_var_inmb_diff} the constructor
#'  \describe{
#'  \item{set_sdc_ref(sdc_ref):}{sets the standard deviation of costs in the reference group of this VAR_INMB_DIFF object}
#'  \item{set_sdc_exp(sdc_exp):}{sets the standard deviation of costs in the experimental group of this VAR_INMB_DIFF object}
#'  \item{set_sde_exp(sde_exp):}{sets the standard deviation of effectiveness in the experimental group of this VAR_INMB_DIFF object}
#'  \item{set_sde_ref(sde_ref):}{sets the standard deviation of effectiveness in the reference group of this VAR_INMB_DIFF object}
#'  \item{set_rho(rho):}{Sets the coefficient of correlation between the difference in costs (dc) and the difference in effectiveness (de) of this VAR_INMB_DIFF object}
#'  \item{set_lambda(lambda):}{sets the lambda value of this VAR_INMB_DIFF object}
#'  \item{get_var_inmb():}{Return the calculated variance of the Incremental Net Monetary Benefit when the standard deviation of costs and effectiveness in each group differ.}
#'  }
#' @include VAR_INMB_DIRECT.R
#' @examples
#' ## First, create a lambda object
#' object_lambda <- create_object_lambda (20000)
#' ## Then, create a var_inmb_diff object
#' var_inmb_diff <- create_object_var_inmb_diff(sdc_ref=2100, sdc_exp=2100, sde_ref = 0.12,
#' sde_exp = 0.12, rho = 0.1,object_lambda = object_lambda)
## @noRd

setRefClass(
  # Nom de la classe
  "VAR_INMB_DIFF",
  # Attributs
  fields =  c(
    sde_ref = "numeric",
    sde_exp = "numeric",
    sdc_ref = "numeric",
    sdc_exp = "numeric",
    rho="numeric",
    var_inmb_exp = "numeric",
    var_inmb_ref = "numeric",
    object_lambda = "ANY"
  ),

  # Fonctions :
  methods=list(
    ### Constructeur
    initialize = function(sde_ref, sde_exp, sdc_ref, sdc_exp,rho,object_lambda){
      check_heritage (object_lambda, "Lambda")
      object_lambda <<- object_lambda
      check_1(list(sde_ref=sde_ref,sde_exp=sde_exp,
                   sdc_ref=sdc_ref,sdc_exp=sdc_exp,rho=rho))
      check_positif(list(sde_ref=sde_ref, sdc_ref=sdc_ref,
                         sde_exp=sde_exp, sdc_exp=sdc_exp))
      check_rho(rho)
      sde_ref <<- sde_ref
      sdc_ref <<- sdc_ref
      sde_exp <<- sde_exp
      sdc_exp <<- sdc_exp
      rho <<- rho
      set_var_inmb_exp()
      set_var_inmb_ref()
      set_var_inmb()
    },

    ### calcul de la variance theorique
    set_var_inmb = function(){
      var_inmb <<- sdc_exp^2 + sdc_ref^2 + object_lambda$get_lambda()^2 *
        (sde_exp^2 + sde_ref^2) - (2 * object_lambda$get_lambda() * rho *
          sqrt ((sdc_exp^2 + sdc_ref^2) * (sde_exp^2 + sde_ref^2)))
    },

    set_sdc_ref = function(sdc_ref){
      check_1(list(sdc_ref=sdc_ref))
      check_positif(list(sdc_ref=sdc_ref))
      sdc_ref <<- sdc_ref
      set_var_inmb_ref()
      set_var_inmb()
    },

    set_sdc_exp = function(sdc_exp){
      check_1(list(sdc_exp=sdc_exp))
      check_positif(list(sdc_exp=sdc_exp))
      sdc_exp <<- sdc_exp
      set_var_inmb_exp()
      set_var_inmb()
    },

    set_sde_exp = function(sde_exp){
      check_1(list(sde_exp=sde_exp))
      check_positif(list(sde_exp=sde_exp))
      sde_exp <<- sde_exp
      set_var_inmb_exp()
      set_var_inmb()
    },

    set_sde_ref = function(sde_ref){
      check_1(list(sde_ref=sde_ref))
      check_positif(list(sde_ref=sde_ref))
      sde_ref <<- sde_ref
      set_var_inmb_ref()
      set_var_inmb()
    },

    set_rho = function(rho){
      check_1(list(rho=rho))
      check_rho(rho)
      rho <<- rho
      set_var_inmb_ref()
      set_var_inmb_exp()
      set_var_inmb()
    },

    set_object_lambda = function(object_lambda){
      check_heritage (object_lambda, "Lambda")
      object_lambda <<- object_lambda
      set_var_inmb_ref()
      set_var_inmb_exp()
      set_var_inmb()
    },


    set_var_inmb_exp = function(){
      var_inmb_exp <<- object_lambda$get_lambda()^2 * sde_exp^2 + sdc_exp^2 -
        2*object_lambda$get_lambda() * rho * sde_exp * sdc_exp
    },

    set_var_inmb_ref = function(){
      var_inmb_ref <<- object_lambda$get_lambda()^2 * sde_ref^2 + sdc_ref^2 -
        2*object_lambda$get_lambda() * rho * sde_ref * sdc_ref
    },

    ############ GETTER
    ### var_inmb est calculee avant d'etre envoyee
    get_var_inmb = function(){
      set_var_inmb()
      return(var_inmb)
    },

  get_var_inmb_exp = function(){
    set_var_inmb_exp()
    return (var_inmb_exp)
  },

  get_var_inmb_ref = function(){
    set_var_inmb_ref()
    return (var_inmb_ref)
  })
  ,
  # Set the inheritance for this class
  contains = "VAR_INMB_DIRECT"
)



#' @title Create an object var_inmb_diff
#' @description The variance of the Incremental Net Monetary Benefit may also be calculated in a hypothetical situation when the standard deviation of costs and effectiveness in each group differ.
#' @param sdc_ref : standard deviation of costs in the reference group
#' @param sdc_exp : standard deviation of costs in the experimental group
#' @param sde_exp : standard deviation of effectiveness in the experimental group
#' @param sde_ref : standard deviation of effectiveness in the reference group
#' @param rho : coefficient of correlation between the difference in costs (dc) and the difference in effectiveness (de)
#' @param object_lambda : object containing the ceiling cost-effectiveness ratio or maximum acceptable cost of a unit of effectiveness. See \link{create_object_lambda}
#' @seealso \link{create_object_var_inmb_direct} to directly provide a value for the variance of the Incremental Net Monetary Benefit
#' @seealso \link{create_object_var_inmb} to calculate the theoretical standard deviation of the expected INB with the same standard deviation in the reference and the experimental group
#' @return create_object_var_inmb_diff returns an object of class \link{VAR_INMB_DIFF} which inherits from the class \link{VAR_INMB_DIRECT}
#' @examples
#' ## First, create a lambda object
#' object_lambda <- create_object_lambda (20000)
#' ## Then, create a var_inmb_diff object
#' var_inmb_diff <- create_object_var_inmb_diff(sdc_ref=2100, sdc_exp=2100, sde_ref = 0.12,
#' sde_exp = 0.12, rho = 0.1,object_lambda = object_lambda)
#' @export

create_object_var_inmb_diff <- function(sdc_ref,sdc_exp, sde_ref,sde_exp,
                                rho, object_lambda){
  var_inmb <- methods::new(Class="VAR_INMB_DIFF", sdc_ref=sdc_ref,
                 sdc_exp = sdc_exp, sde_ref = sde_ref,sde_exp=sde_exp,
                 rho=rho, object_lambda = object_lambda)
}
