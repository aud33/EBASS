#' @title A Reference Class to represent the EVPI
#' @description An object that combines three others objects : object_inmb, object_pop, object_var_inmb.
#' @name EVPI_DECREASE
#' @field object_inmb : an instance that inherits the \link{INMB_DIRECT}
#' @field object_var_inmb : an instance that inherits the \link{VAR_INMB_DIRECT}
#' @field object_pop : an instance that inherits the \link{POP}
#' @field step_ref (default=1) : to define the ratio (step_ref/step_exp) for group allocation during the study
#' @field step_exp (default=1) : to define the ratio (step_ref/step_exp) for group allocation during the study
#' @section Methods:
#'  \describe{
#'  \item{get_N():}{return the estimated optimal sample size for the study}
#'  \item{get_N_exp():}{return the estimated number of individuals in the experimental group}
#'  \item{get_N_ref():}{return the estimated number of participants to include in the reference group}
#'  \item{get_k(N_exp):}{return the ratio (step_ref/step_exp) for group allocation}
#'  \item{set_N_ref(N_ref):}{sets the number of individuals in the reference group (N_exp will be automatically calculated according to the ratio)}
#'  \item{set_N_exp(N_exp):}{sets the number of individuals in the experimental group (N_ref will be automatically calculated according to the ratio)}
#'  \item{set_object_inmb(object_inmb):}{sets object_inmb for this EVPI_DECREASE object}
#'  \item{set_object_var_inmb(object_var_inmb):}{sets object_var_inmb for this EVPI_DECREASE object}
#'  \item{set_object_pop(object_pop):}{sets object_pop for this EVPI_DECREASE object}
#'  }
## get_var_inmb_expected : return the expected standard deviation of the expected INB and considering a given sample size
## get_evpis : return the estimate of the Expected Value of Perfect Information (EVPI) for a given total sample size and the POP object

#' @include VAR_INMB_DIRECT.R
#' @include INMB_DIRECT.R
#' @include POP.R
## @noRd
setRefClass(
  # Nom de la classe
  "EVPI_DECREASE",
  # Attributs
  fields =  c(
    object_inmb = "ANY",
    object_var_inmb = "ANY",
    object_pop = "ANY",
    ## nombre de sujets :
    k = "numeric",
    # deduit avec k :
    step_exp = "numeric",
    step_ref = "numeric",

    N_exp = "numeric",
    N_ref = "numeric",
    ## Attribues calcules :
    var_inmb_expected = "numeric",
    evpis = "numeric",
    evpi = "numeric"
  ),

  # Fonctions :
  methods=list(
    ### Constructeur
    initialize = function(object_inmb,object_var_inmb, object_pop,step_ref=1,
                          step_exp=1){
      ## object BNI
      check_heritage (object_inmb, "INMB_DIRECT")
      object_inmb <<- object_inmb
      ## object_var_inmb
      check_heritage (object_var_inmb, "VAR_INMB_DIRECT")
      object_var_inmb <<- object_var_inmb
      ## object pop
      check_heritage (object_pop, "POP")
      object_pop <<- object_pop

      ## k
      check_entier(list(step_ref = step_ref, step_exp=step_exp))
      step_ref <<- step_ref
      step_exp <<- step_exp
      set_k()
    },

    ### setter public :
    set_k = function(){
      k <<- step_ref / step_exp
    },

    set_N_exp = function(N_exp){
      check_positif(list(N_exp=N_exp))
      N_exp <<- N_exp
      N_ref <<- N_exp * k
#        if (any(N_ref%%1 != 0)){
#          warning("N_ref n'est pas un nombre entier")
#        }
      set_var_inmb_expected()
      set_evpis()
      set_evpi()
    },

    set_N_ref = function(N_ref){
      check_positif(list(N_ref=N_ref))
      N_ref <<- N_ref
      N_exp <<- N_ref/k
#       if (any(N_exp%%1 != 0)){
#         warning("N_exp n'est pas un nombre entier")
#       }
      set_var_inmb_expected()
      set_evpis()
      set_evpi()
    },

    #### setter prive :
    set_var_inmb_expected=function(){
      var_inmb_expected <<- (object_var_inmb$get_var_inmb_exp() / N_exp) +
        (object_var_inmb$get_var_inmb_ref() / N_ref)
    },

    set_evpis = function(){
      inmb <- object_inmb$get_inmb()
      evpis <<- sapply(var_inmb_expected, function(varinmb_expected){
        EVPIi <- fonction_D(inmb, varinmb_expected)
        return (EVPIi)
      })
    },

    ### evpis pour tous les individus :
    set_evpi = function(){
      evpi <<- evpis * object_pop$get_iota()
    },

    ## setter des objects :
    set_object_var_inmb = function(object_var_inmb){
      check_heritage(object_var_inmb, "VAR_INMB_DIRECT")
      object_var_inmb <<- object_var_inmb
      if (length(N_exp) != 0){
        set_var_inmb_expected()
      }
    },

    set_object_inmb = function(object_inmb){
      check_heritage(object_inmb, "INMB_DIRECT")
      object_inmb <<- object_inmb
      if (length(N_exp) != 0){
        set_var_inmb_expected()
      }
    },

    set_object_pop = function(object_pop){
      check_heritage(object_pop, "POP")
      object_pop <<- object_pop
      if (length(N_exp) != 0){
        set_var_inmb_expected()
      }
    },

    ### get_N
    get_N = function(){
      N <- N_exp + N_ref
      return (N)
    },

    get_N_ref = function(){
      return (N_ref)
    },

    get_N_exp = function(){
      return(N_exp)
    },

    get_k = function(){
      return(k)
    },
    get_step_exp = function(){
      return(step_exp)
    },
    get_step_ref = function(){
      return(step_ref)
    },

    ### getter
    get_var_inmb_expected = function(){
      set_var_inmb_expected()
      return(var_inmb_expected)
    },

    get_evpis = function(){
      set_var_inmb_expected()
      set_evpis ()
      return(evpis)
    },

    get_evpi = function(){
      set_var_inmb_expected()
      set_evpis ()
      set_evpi()
      return(evpi)
    },

    ## Analytic method to integrate the desire function (Willan article )
    fonction_D = function(b0,v0){
      indic <- ifelse (b0 < 0, 1, 0)
      output <- ((v0/(2*pi))^0.5)*
        exp ((-b0^2)/(2*v0))-
        b0*(pnorm(-b0/(v0^0.5), 0,1) - indic)
      return(output)
    }
  )
)




#' @title Create an object evpi_decrease
#' @description An object that combines three others objects : object_inmb, object_pop, object_var_inmb.
#' It contains methods to compute the value of perfect information (EVPI) that would remain after a study of n participants (EVPIn).
#' For each additional individual included, the EVPI decreases. So EVPIn is a decreasing vector. It is used to determine the optimal sample size.
#' @param object_inmb : an object that represents the INMB (Incremental Net Monetary Benefit)
#' Create an object with one of these functions : \link{create_object_inmb_direct}, \link{create_object_inmb}
#' @param object_pop : an object that represents the size of the targeted population.
#' Create an object with \link{create_object_pop}
#' @param object_var_inmb : an object that represents the variance of INMB. The variance of INMB can be directly hypothesized,
#' or calculated through sdc, sde, rho and lambda, or calculated through sdc_ref, sde_ref, sdc_exp, sde_exp, rho and lambda.
#' Create an object with one of these functions : \link{create_object_var_inmb_direct}, \link{create_object_var_inmb},\link{create_object_var_inmb_diff}
#' @param step_ref (default=1) : the minimal number of individuals to be included in the reference group to respect the allocation ratio. If the allocation ratio is 2:1 in favor of the reference group, step_ref=2 and step_exp=1.
#' @param step_exp (default=1) : the minimal number of individuals to be included in the experimental group to respect the allocation ratio. If the allocation ratio is 2:1 in favor of the experimental group, step_exp=2 and step_ref=1.
#' @return create_object_evpi_decrease returns an object of class \link{EVPI_DECREASE}
#' @examples
#' ## First, create 3 objects : inmb, pop and var_inmb, then create the evpi_decrease object
#' object_lambda <- create_object_lambda (20000)
#' object_inmb <- create_object_inmb(de = 0.04, dc=-168, object_lambda = object_lambda)
#' object_var_inmb <- create_object_var_inmb(sde=0.12, sdc=2100, rho=0.1, object_lambda=object_lambda)
#' object_pop <- create_object_pop(horizon = 20, discount=0.04, N_year = 52000)
#' object_evpi_decrease <- create_object_evpi_decrease(object_inmb, object_pop, object_var_inmb)
#' @export
create_object_evpi_decrease <- function(object_inmb, object_pop, object_var_inmb,
                              step_exp = 1, step_ref=1){
  evpi <- methods::new (Class="EVPI_DECREASE",object_inmb = object_inmb, object_pop = object_pop,
               object_var_inmb=object_var_inmb, step_exp = step_exp, step_ref = step_ref)
}



