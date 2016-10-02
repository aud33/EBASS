#' @title A Reference Class to represent the INMB (Incremental Net Monetary Benefit)
#' @description The net monetary benefit (NMB) of an intervention is given by E x Lambda - C,
#' where E and C are the effectiveness and cost of this intervention, and Lambda is the threshold value for a unit of effectiveness,
#' the ceiling incremental cost-effectiveness ratio. When the NMB is positive, the value of the intervention's effectiveness overpasses its cost.
#' When evaluating the cost-effectiveness of a new intervention in comparison with the reference, one can estimate the difference between the net monetary benefit of the new or experimental intervention
#' (NMBn) and the net monetary benefit of the reference (NMBr). This difference is known as the incremental net monetary benefit (INMB), which is given by: INMB = NMBn - NMBr = de x lambda - dc.
#' The new intervention is cost-effective if INMB is positive.
#' @name INMB
#' @field de : Expected point estimate of the difference in mean effectiveness (effectiveness in the experimental group minus effectiveness in the reference group)
#' @field dc : Expected point estimate of the difference in mean cost (cost in the experimental group minus cost in the reference group)
#' @field object_lambda : object containing the ceiling cost-effectiveness ratio or maximum acceptable cost of a unit of effectiveness. See \link{create_object_lambda}
#' @section Methods:
#'  \describe{
#'  \item{get_inmb():}{Returns the calculated Incremental Net Monetary Benefit (inmb)}
#'  \item{set_dc(dc):}{sets the dc of this INMB object}
#'  \item{set_de(de):}{sets the de of this INMB object}
#'  \item{set_object_lambda(object_lambda):}{sets the object_lambda of this INMB object}
#'  }
#' @seealso \link{INMB_DIRECT} the parent class
#' @seealso \link{create_object_inmb} the constructor
#' @include INMB_DIRECT.R
#' @include internal.R
#' @examples
#' ## First, create a lambda object
#' object_lambda <- create_object_lambda (20000)
#' ## Then, create an inmb object
#' object_inmb <- create_object_inmb(de = 0.04, dc=-168, object_lambda = object_lambda)
#' ## inmb is calculated by methods inside the object. Retrieve the inmb :
#' object_inmb$get_inmb()
## @noRd

setRefClass(
  # Nom de la classe
  "INMB",
  # Attributs
  fields =  c(
    de = "numeric",
    dc = "numeric",
    object_lambda = "ANY"
  ),

  # Fonctions :
  methods=list(
    get_inmb = function(){
      #"Returns the inmb"
      set_inmb()
      return(inmb)
    },

    ### Constructeur
    initialize = function(de,dc,object_lambda){
      #"Initialize the object"
      check_heritage (object_lambda, "Lambda")
      object_lambda <<- object_lambda

      check_1(list(de=de,dc=dc))
      de <<- de
      dc <<- dc
    },



    ### recalcul le inmb
    set_inmb = function(){
      #"Private function to calculate the inmb"
      inmb <<- de*object_lambda$get_lambda() - dc
    },

    set_dc = function(dc){
      #"sets the dc of this INMB"
      check_1(list(dc=dc))
      dc <<- dc
      set_inmb()
    },

    set_de = function(de){
      #"sets the de of this INMB"
      check_1(list(de=de))
      de <<- de
      set_inmb()
    },

    set_object_lambda = function(object_lambda){
      #"sets the lamba_object of this INMB"
      check_heritage (object_lambda, "Lambda")
      object_lambda <<- object_lambda
    }
  ),
  # Set the inheritance for this class
  contains = "INMB_DIRECT"
)

#' @title Create an object INMB
#' @description The net monetary benefit (NMB) of an intervention is given by E x Lambda - C,
#' where E and C are the effectiveness and cost of this intervention, and Lambda is the threshold value for a unit of effectiveness,
#' the ceiling incremental cost-effectiveness ratio. When the NMB is positive, the value of the intervention's effectiveness overpasses its cost.
#' When evaluating the cost-effectiveness of a new intervention in comparison with the reference, one can estimate the difference between the net monetary benefit of the new or experimental intervention
#' (NMBn) and the net monetary benefit of the reference (NMBr). This difference is known as the incremental net monetary benefit (INMB), which is given by: INMB = NMBn - NMBr = de x lambda - dc.
#' The new intervention is cost-effective if INMB is positive.
#' @param de : Expected point estimate of the difference in mean effectiveness (effectiveness in the experimental group minus effectiveness in the reference group)
#' @param dc : Expected point estimate of the difference in mean cost (cost in the experimental group minus cost in the reference group)
#' @param object_lambda : object containing the ceiling cost-effectiveness ratio or maximum acceptable cost of a unit of effectiveness. See \link{create_object_lambda}
#' @seealso \link{create_object_inmb_direct} for INMB directly defined
#' @return create_object_inmb returns an object of class \link{INMB} which inherits from the class \link{INMB_DIRECT}
#' @examples
#' ## First, create a lambda object
#' object_lambda <- create_object_lambda (20000)
#' ## Then, create an inmb object
#' object_inmb <- create_object_inmb(de = 0.04, dc=-168, object_lambda = object_lambda)
#' ## inmb is calculated by methods inside the object. Retrieve the inmb :
#' object_inmb$get_inmb()
#' @export
create_object_inmb <- function(de, dc, object_lambda){
  inmb <- methods::new (Class="INMB",de = de, dc=dc, object_lambda = object_lambda)
}

