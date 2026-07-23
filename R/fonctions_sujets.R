#' @title private function to get the bounds in which the true estimated sample size will be
#' @description
#' This function performs a bisection search to find the lower and upper bounds
#' within which the optimal sample size lies.
#'
#' @param object_evpi_decrease  An evpi_decrease object. See \link{create_object_evpi_decrease}
#' @param cost_indiv : Cost of an additional inclusion in your planned cost-effectiveness study
#' @noRd
#' @return A Numeric vector of length 2 containing the lower and upper bounds


get_bornes <- function(object_evpi_decrease, cost_indiv){
  ## verification :
  check_heritage(object_evpi_decrease, "EVPI_DECREASE")
  check_1(list(cost_indiv=cost_indiv))
  check_positif(list(cost_indiv=cost_indiv))
  
  ### debute par des puissances :
  borne_inf <- 0
  borne_max <- 10^7
  iter <- 0
  
  ## l'arret doit dependre du pas
  if (object_evpi_decrease$get_k() < 1){
    min_pas <- object_evpi_decrease$get_step_ref()
  } else {
    min_pas <- object_evpi_decrease$get_step_exp()
  }
  
  while (borne_max - borne_inf > (min_pas * 50)){
    delta <- ceiling((borne_max - borne_inf) / 10)
    sequence <- seq (borne_inf, borne_max, by = delta)
    sequence <- sort (c(sequence,sequence + 1))
    ### Determination de N_exp et N_ref en fonction de k
    if (object_evpi_decrease$get_k() < 1){
      object_evpi_decrease$set_N_ref(sequence)
    } else {
      object_evpi_decrease$set_N_exp(sequence)
    }
    # Calcul de EVPI et de la pente
    test <- object_evpi_decrease$get_evpi()
    pente <- test[-length(test)] - test[-1]
    bool <- 1:length(test) %% 2 != 0
    pente <- pente[bool]
    sequence <- sequence[bool]
    bool <- pente < cost_indiv * (object_evpi_decrease$get_step_exp() + object_evpi_decrease$get_step_ref())
    if (all(bool) || (all(!bool))){
      stop("Convergence error")
    }
    # update bounds
    borne_inf <- max (sequence[!bool])
    borne_max <- min (sequence[bool])
    # check iteration limit
    iter <- iter + 1
    if (iter == 10){
      stop("Convergence error")
    }
  }
  bornes <- c(borne_inf, borne_max)
  return (bornes)
}

#' @title private function to calculate the estimated sample size
#' @description
#' This function calculates the optimal sample size by finding the point where
#' the marginal EVPI gain equals the marginal cost of inclusion.
#' @param object_evpi_decrease An evpi_decrease object. See \link{create_object_evpi_decrease}
#' @param cost_indiv of an additional inclusion in your planned cost-effectiveness study
#' @param bornes A numeric vector of length 2 containing lower and upper bounds,
#'   as returned by \code{\link{get_bornes}}
#' @noRd
#' @return A data frame with three columns: N (total sample size), N_exp (experimental group),
#'  and N_ref (reference group)

get_n_sujet_pente <- function(object_evpi_decrease, cost_indiv, bornes){
  ## Determine le pas et la sequence en fonction de k
  if (object_evpi_decrease$get_k() < 1){
    pas <- object_evpi_decrease$get_step_ref()
    N <- bornes[1]:bornes[2]
    bool <- N %% pas == 0
    N <- N[bool]
    object_evpi_decrease$set_N_ref(N)
  } else {
    pas <- object_evpi_decrease$get_step_exp()
    N <- bornes[1]:bornes[2]
    bool <- N %% pas == 0
    N <- N[bool]
    object_evpi_decrease$set_N_exp(N)
  }
  test <- object_evpi_decrease$get_evpi()
  pente <- test[1:(length(test)-1)] - test[2:(length(test))]
  diff <- pente - cost_indiv * (object_evpi_decrease$get_step_exp() + object_evpi_decrease$get_step_ref())
  bool <- diff > 0
  diff_min <- min(diff[bool])
  num_sujet <- which(diff %in% diff_min)
  n_sujet <- N[num_sujet] + pas
  if (object_evpi_decrease$get_k() < 1){
    object_evpi_decrease$set_N_ref(n_sujet)
  } else {
    object_evpi_decrease$set_N_exp(n_sujet)
  }
  
  # cat ("optimal number of participants to include in the study : ", object_evpi_decrease$get_N(), "\n")
  # cat ("optimal number of participants to include in the experimental group :",   object_evpi_decrease$get_N_exp(), "\n")
  # cat ("optimal number of participants to include in the reference group :",   object_evpi_decrease$get_N_ref(), "\n")
  
  # Output messages
  message(sprintf("Optimal number of participants to include in the study: %d",
                  object_evpi_decrease$get_N()))
  message(sprintf("Optimal number of participants to include in the experimental group: %d",
                  object_evpi_decrease$get_N_exp()))
  message(sprintf("Optimal number of participants to include in the reference group: %d",
                  object_evpi_decrease$get_N_ref()))
  
  # return data frame
  df <- data.frame (N = object_evpi_decrease$get_N(),
                    N_exp = object_evpi_decrease$get_N_exp(),
                    N_ref = object_evpi_decrease$get_N_ref())
  return(df)
}

#' @title Function to calculate the estimated sample size based on the Expected Value of Perfect Information (EVPI)
#' @description This function calculates the optimal total sample size for a planned cost-effectiveness study.
#' The optimal sample size is reached when the marginal EVPI gain is less than or equal to
#'  the marginal cost of inclusion: (step_ref + step_exp) * cost_indiv.
#' @param object_evpi_decrease An evpi_decrease object. See \link{create_object_evpi_decrease}
#' @param cost_indiv cost of an additional inclusion in your planned cost-effectiveness study.
#' @return A dataframe containing three columns :
#'    \item{N}{Total sample size of the planned cost-effectiveness study}
#'    \item{N_exp}{Sample size in the experimental group}
#'    \item{N_ref}{Sample size in the reference group}
#' @export
#'
sample_size <- function(object_evpi_decrease, cost_indiv){
  check_heritage(object_evpi_decrease, "EVPI_DECREASE")
  check_1(list(cost_indiv=cost_indiv))
  check_positif(list(cost_indiv=cost_indiv))
  get_n_sujet_pente (object_evpi_decrease, cost_indiv,get_bornes (object_evpi_decrease, cost_indiv))
}

#' @title Plot to explain the estimated sample size calculation
#' @description Displays a graph showing the estimated sample size based on the EVPI gain after the inclusion of new participants and the associated inclusion costs.
#' Use \link{sample_size} first to estimate the sample size before calling this function.
#' @param object_evpi_decrease An evpi_decrease object. See \link{create_object_evpi_decrease}
#' @param cost_indiv Mean costs induced by the inclusion and follow-up of one participant in the study
#' @return `NULL`, invisibly . Called for its plotting side effect.
#' @export
graph_gain_n <- function(object_evpi_decrease, cost_indiv){
  # Initialisation
  N_exp_ini <- object_evpi_decrease$get_N_exp()
  N_ini <- object_evpi_decrease$get_N()
  N_ref_ini <- object_evpi_decrease$get_N_ref()
  # Validation
  check_1(list(N_exp_ini=N_exp_ini))
  
  # Creation d'une sequence autour du point optimal
  if (object_evpi_decrease$get_k() < 1){
    N_ref <- object_evpi_decrease$get_N_ref()
    pas <- object_evpi_decrease$get_step_ref()
    N_ref <- seq(N_ref-(30*pas), N_ref+(30*pas), pas)
    bool <- N_ref > 0
    N_ref <- N_ref[bool]
    object_evpi_decrease$set_N_ref(N_ref)
  } else {
    N_exp <- object_evpi_decrease$get_N_exp()
    pas <- object_evpi_decrease$get_step_exp()
    N_exp <- seq(N_exp-(30*pas), N_exp+(30*pas), pas)
    bool <- N_exp > 0
    N_exp <- N_exp[bool]
    object_evpi_decrease$set_N_exp(N_exp)
  }
  
  pente <- object_evpi_decrease$get_evpi()[-length(object_evpi_decrease$get_evpi())] - object_evpi_decrease$get_evpi()[-1]
  
  diff <- pente - cost_indiv * (object_evpi_decrease$get_step_exp() + object_evpi_decrease$get_step_ref())
  bool <- diff > 0
  if (all(bool) | all(!bool)){
    stop("Error explaining")
  }
  diff_min <- which(min(diff[bool]) == diff)
  
  ### j'ajoute + 1 pour le graphique
  ## en effet, si la difference est compris entre 0 et 1, j'aurais des valeurs negatives
  # cela permet de conserver un graphique coherent bien que les valeurs ne soient pas exacts
  # cela ne se verra pas car c'est une echelle logorithmique
  diff_log <- suppressWarnings(ifelse (diff > 0, log(diff+1), -log(-diff+1)))
  graphics::plot (diff_log, ylab="log (EVPI gain minus inclusion costs)",
                  xlab="Optimal number of participants to include in the study", xaxt="n")
  graphics::axis (side=1, at=1:length(object_evpi_decrease$get_N()), labels=as.character(object_evpi_decrease$get_N()))
  
  message_plot <- paste (object_evpi_decrease$get_N_exp()[diff_min+1], "exp and ",
                         object_evpi_decrease$get_N_ref()[diff_min+1], "ref",sep="")
  graphics::text(x=(diff_min)+diff_min*0.35,
                 y= diff_log[diff_min] - 0.3*diff_log[diff_min],
                 labels=as.character(message_plot),col="blue")
  graphics::points (x=diff_min, y=diff_log[diff_min], col="blue", pch=19)
  
  #### faire quelque chose plus propre ici
  message_plot <- paste (object_evpi_decrease$get_N_exp()[diff_min+2], "exp and ",
                         object_evpi_decrease$get_N_ref()[diff_min+2], "ref",sep="")
  
  graphics::text(x=(diff_min+1)+(diff_min+1)*0.35,
                 y= diff_log[diff_min+1] - 0.3*diff_log[diff_min+1],
                 labels=as.character(message_plot),col="red")
  graphics::points (x=diff_min+1, y=diff_log[diff_min+1], col="red", pch=19)
  graphics::abline(h=0)
  
  ## zone critique
  # cat ("Gain with ",object_evpi_decrease$get_N()[diff_min+1], " participants (",object_evpi_decrease$get_N_exp()[diff_min+1],
  #      "exp/",object_evpi_decrease$get_N_ref()[diff_min+1],"ref) : ", diff[diff_min], "\n", sep="")
  #
  # cat ("Gain with ",object_evpi_decrease$get_N()[diff_min+2], " participants (",object_evpi_decrease$get_N_exp()[diff_min+2],
  #      "exp/",object_evpi_decrease$get_N_ref()[diff_min+2],"ref) : ", diff[diff_min+1], "\n", sep="")
  
  message(sprintf(
    "Gain with %d participants (%d exp / %d ref): %.4f",
    object_evpi_decrease$get_N()[diff_min+1],
    object_evpi_decrease$get_N_exp()[diff_min+1],
    object_evpi_decrease$get_N_ref()[diff_min+1],
    diff[diff_min]
  ))
  
  message(sprintf(
    "Gain with %d participants (%d exp / %d ref): %.4f",
    object_evpi_decrease$get_N()[diff_min+2],
    object_evpi_decrease$get_N_exp()[diff_min+2],
    object_evpi_decrease$get_N_ref()[diff_min+2],
    diff[diff_min+1]
  ))
  
  
  ## reinitilisation de l'object
  object_evpi_decrease$set_N_exp(N_exp_ini)
  
  invisible(NULL)
}

#' @title Function to estimate the gamma risk
#' @description The gamma risk is the probability that a decision based on the expected mean of the Incremental Net Monetary Benefit (INMB) is wrong.
#' In other terms, it is one minus the cost-effectiveness probability of an intervention.
#' Use \link{sample_size} function first to estimate the sample size before calling this function.
#' @param object_evpi_decrease An evpi_decrease object. See \link{create_object_evpi_decrease}
#' @return A numeric value representing the gamma risk (probability)
#' @export
gamma_risk <- function(object_evpi_decrease){
  variance_attendue <- object_evpi_decrease$get_var_inmb_expected()
  check_1(list(variance_attendue=variance_attendue))
  inmb <- object_evpi_decrease$object_inmb$get_inmb()
  pvalue <- stats::pnorm (0, inmb, sqrt(variance_attendue))
  return (pvalue)
}
