#' @title A Reference Class to represent the INMB (Incremental Net Monetary Benefit)
#' @description If the INMB can be drawn from de, dc and lambda with \link{create_object_inmb}, one can also make directly an hypothesis on the value of the INMB.
#' @name INMB_DIRECT
#' @field inmb : INMB expected Incremental Net monetary Benefit.
#' @seealso \link{create_object_inmb_direct} the constructor
#' @seealso \link{create_object_inmb} to calculate the INMB
#' @section Methods:
#'  \describe{
#'  \item{get_inmb():}{Returns the Incremental Net Monetary Benefit (inmb)}
#'  \item{set_inmb(inmb):}{sets the inmb of this INMB_DIRECT object}
#'  }
## @noRd

setRefClass(
  # the name for the class
  "INMB_DIRECT",

  # Attributs
  fields =  c(
    inmb = "numeric"
  ),

  methods= list (
    ### constructeur
    initialize = function(inmb){
      set_inmb(inmb)
    },

    ### getter
    get_inmb = function(){
      return(inmb)
    },

    ### setter
    set_inmb = function(inmb){
      #"sets the lamba_object of this INMB"
      check_1(list(inmb=inmb))
      inmb <<- inmb
    }
  )
)

#' @title Create an object INMB_DIRECT
#' @description If the INMB can be drawn from de, dc and lambda with \link{create_object_inmb}, one can also make directly an hypothesis on the value of the INMB.
#' @param inmb : expected Incremental Net Monetary Benefit
#' @seealso \link{create_object_inmb}
#' @return create_object_inmb_direct returns an object of class \link{INMB_DIRECT}
#' @examples
#' ## Create an object inmb_direct
#' object_inmb_direct <- create_object_inmb_direct (968)
#' @export

create_object_inmb_direct <- function(inmb){
  inmb <- methods::new (Class="INMB_DIRECT",inmb = inmb)
}



