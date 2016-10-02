#' @title A Reference Class to represent the target population
#' @description The expected value of perfect information (EVPI) is estimated for the entire population targeted by the evaluated intervention.
#' @name POP
## @description The expected value of perfect information is estimated for the entire population targeted by the evaluated intervention.
## This objects represents this target population. The size of the target population (POP) can be estimated through prevalence and incidence data from registries,
## large cohort studies, medico-administrative databases, or surveillance systems.
## POP has to be calculated over the entire time horizon used for the estimation of the EVPI.
## It is usually easier to gather data on the annual number of individual susceptible to benefit for the new intervention.
## If this number is expected to be constant over the time horizon, POP is the product of this time horizon (in years) and the annual number of individual.
## If the time horizon is longer than one year, POP has to be discounted.
#' @field horizon : Time horizon in years considered in the estimation of the EVPI.
#' Finite time horizons are recommended in order to control for the complex and uncertain process of future changes.
#' Furthermore, because of discounting, the impact of a time horizon over 15 or 20 years on the estimation of EVPI is insignificant.
#' @field discount : Annual discount rate considered in the estimation of the EVPI. The annual discount rate is defined in each country, usually within 3 to 6\%.
#' @field N_year : Number of individuals likely to be targeted by the evaluated intervention each year
#' @section Methods:
#'  \describe{
#'  \item{set_discount(discount):}{sets the discount for this POP object}
#'  \item{set_N_year(N_year):}{sets the N_year of this POP object}
#'  \item{set_horizon(horizon):}{sets the horizon of this POP object}
#'  }
#' @seealso \link{create_object_pop} the constructor
#' @examples
#' object_pop <- create_object_pop(horizon = 20, discount=0.04, N_year = 52000)
## @noRd
setRefClass(
  # Nom de la classe
  "POP",
  # Attributs
  fields =  c(
    horizon = "numeric",
    discount = "numeric",
    N_year = "numeric",
    iota = "numeric"
  ),

  # Fonctions :
  methods=list(
    ### Constructeur
    initialize = function(horizon,discount,N_year){
      check_1(list(horizon=horizon, discount = discount, N_year = N_year))
      check_positif(list(horizon=horizon, discount = discount, N_year = N_year))
      check_horizon (horizon)
      check_discount (discount)
      horizon <<- horizon
      discount <<- discount
      N_year <<- N_year
      set_iota ()
    },

    check_horizon = function(horizon){
      if (horizon == 0){
        stop("Time horizon can not be equal to 0")
      }
      if (!horizon%%1==0){
        stop ("Time horizon must be an integer")
      }
      return(T)
    },

    check_discount = function(discount){
      if (discount > 1){
        stop("Annual discount rate must be less than 1")
      }
      return(T)
    },

    set_horizon = function(horizon){
      horizon <<- horizon
      set_iota()
    },

    set_discount = function(discount){
      discount <<- discount
      set_iota()
    },

    set_N_year = function(N_year){
      N_year <<- N_year
      set_iota()
    },

    set_iota = function(){
      iota_temp <- 0
      horizon_temp <- 1:horizon - 1
      for (k in horizon_temp){
        une_annee <- (1/(1+discount)^k) * N_year
        iota_temp <- sum(iota_temp, une_annee)
      }
      iota <<- iota_temp
    },

    ### getter :
    get_iota = function(){
      set_iota()
      return(iota)
    },

    get_N_year = function(){
      return(N_year)
    },

    get_discount = function(){
      return(discount)
    },

    get_horizon = function(){
      return(horizon)
    }
)
)


#' @title Create an object POP
#' @description The expected value of perfect information (EVPI) is estimated for the entire population targeted by the evaluated intervention.
#' This object represents this target population. The size of the target population (POP) can be estimated through prevalence and incidence data from registries,
#' large cohort studies, medico-administrative databases, or surveillance systems.
#' POP has to be calculated over the entire time horizon used for the estimation of the EVPI.
#' It is usually easier to gather data on the annual number of individual susceptible to benefit for the new intervention.
#' If this number is expected to be constant over the time horizon, POP is the product of this time horizon (in years) and the annual number of individual.
#' If the time horizon is longer than one year, POP has to be discounted.
#' @param horizon : Time horizon in years considered in the estimation of the EVPI.
#' Finite time horizons are recommended in order to control for the complex and uncertain process of future changes.
#' Furthermore, because of discounting, the impact of a time horizon over 15 or 20 years on the estimation of EVPI is insignificant.
#' @param discount : Annual discount rate considered in the estimation of the EVPI. The annual discount rate is defined in each country, usually within 3 to 6\%.
#' @param N_year : Number of individuals likely to be targeted by the evaluated intervention each year
#' @return create_object_pop returns an object of class \link{POP}
#' @examples
#' object_pop <- create_object_pop(horizon = 20, discount=0.04, N_year = 52000)
#' @export

create_object_pop <- function(horizon, discount, N_year){
  pop <- methods::new (Class="POP",horizon = horizon, discount=discount, N_year = N_year)
}
