#' @title private function to get the bounds in which the true estimated sample size will be
#' @param object_evpi_decrease : an instance of the \link{EVPI-class}
#' @param cost_indiv : cost of an additional inclusion in your planned cost-effectiveness study
#' @noRd
#' @return A vector of lower and upper bounds
get_bornes <- function(object_evpi_decrease, cost_indiv){
  ## verification :
  check_heritage(object_evpi_decrease, "EVPI_DECREASE")
  check_1(list(cost_indiv=cost_indiv))
  check_positif(list(cost_indiv=cost_indiv))

  ### debute par des puissances :
  borne_inf <- 0
  borne_max <- 10^7
  iter <- 0

  ## l'arret doit dependre du pas :
  if (object_evpi_decrease$get_k() < 1){
    min_pas <- object_evpi_decrease$get_step_ref()
  } else {
    min_pas <- object_evpi_decrease$get_step_exp()
  }

  while (borne_max - borne_inf > (min_pas * 50)){
    delta <- ceiling((borne_max - borne_inf) / 10)
    sequence <- seq (borne_inf, borne_max, delta)
    sequence <- sort (c(sequence,sequence + 1))
    ### Pour avoir un nombre entier pour N_exp et N_ref, on recherche N_exp ou N_ref selon k
    if (object_evpi_decrease$get_k() < 1){
      object_evpi_decrease$set_N_ref(sequence)
    } else {
      object_evpi_decrease$set_N_exp(sequence)
    }
    test <- object_evpi_decrease$get_evpi()
    pente <- test[-length(test)] - test[-1]
    bool <- 1:length(test) %% 2 != 0
    pente <- pente[bool]
    sequence <- sequence[bool]
    bool <- pente < cost_indiv * (object_evpi_decrease$get_step_exp() + object_evpi_decrease$get_step_ref())
    if (all(bool) || (all(!bool))){
      stop("Convergence error")
    }
    borne_inf <- max (sequence[!bool])
    borne_max <- min (sequence[bool])
    iter <- iter + 1
    if (iter == 10){
      stop("Convergence error")
    }
  }
  bornes <- c(borne_inf, borne_max)
  return (bornes)
}

#' @title private function to calculate the estimated sample size
#' @param object_evpi_decrease : an instance of the \link{EVPI-class}
#' @param cost of an additional inclusion in your planned cost-effectiveness study
#' @param bornes : return from \link{get_bornes}
#' @noRd
#' @return estimated sample size of the study
get_n_sujet_pente <- function(object_evpi_decrease, cost_indiv, bornes){
  ## le pas doit dependre de step_exp ou step_ref
  ### Pour avoir un nombre entier pour N_exp et N_ref, on recherche N_exp ou N_ref selon k
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

  cat ("optimal number of participants to include in the study : ", object_evpi_decrease$get_N(), "\n")
  cat ("optimal number of participants to include in the experimental group :",   object_evpi_decrease$get_N_exp(), "\n")
  cat ("optimal number of participants to include in the reference group :",   object_evpi_decrease$get_N_ref(), "\n")
  df <- data.frame (N = object_evpi_decrease$get_N(),
                    N_exp = object_evpi_decrease$get_N_exp(),
                    N_ref = object_evpi_decrease$get_N_ref())
  return(df)
}

#' @title Function to calculate the estimated sample size based on the Expected Value of Perfect Information (EVPI)
#' @description This function will provide the total sample size of your planned cost-effectiveness study.
#' The optimal sample size of your planned cost-effectiveness study is reached when EVPIn (a vector calculated by the evpi_decrease object) is less or equal to (step_ref + step_exp)*cost_indiv
#' @param object_evpi_decrease : evpi_decrease object. See \link{create_object_evpi_decrease}
#' @param cost_indiv : cost of an additional inclusion in your planned cost-effectiveness study.
#' @return A dataframe containing three vectors : N, N_exp, N_ref, the total number of subjects of your planned cost-effectiveness study, in the experimental group and in the reference group respectively.
#' @export
sample_size <- function(object_evpi_decrease, cost_indiv){
  check_heritage(object_evpi_decrease, "EVPI_DECREASE")
  check_1(list(cost_indiv=cost_indiv))
  check_positif(list(cost_indiv=cost_indiv))
  get_n_sujet_pente (object_evpi_decrease, cost_indiv,get_bornes (object_evpi_decrease, cost_indiv))
}

#' @title Explain the estimated sample size calculated
#' @description Produces a plot to explain the estimated sample size calculated based on the EVPI gain after the inclusion of new participants and inclusion costs.
#' Use \link{sample_size} function first to estimate the sample size.
#' @param object_evpi_decrease : evpi_decrease object. See \link{create_object_evpi_decrease}
#' @param cost_indiv : mean costs induced by the inclusion and follow-up of one participant in the study
#' @export
graph_gain_n <- function(object_evpi_decrease, cost_indiv){
  N_exp_ini <- object_evpi_decrease$get_N_exp()
  N_ini <- object_evpi_decrease$get_N()
  N_ref_ini <- object_evpi_decrease$get_N_ref()
  check_1(list(N_exp_ini=N_exp_ini))

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
  cat ("Gain with ",object_evpi_decrease$get_N()[diff_min+1], " participants (",object_evpi_decrease$get_N_exp()[diff_min+1],
       "exp/",object_evpi_decrease$get_N_ref()[diff_min+1],"ref) : ", diff[diff_min], "\n", sep="")
  cat ("Gain with ",object_evpi_decrease$get_N()[diff_min+2], " participants (",object_evpi_decrease$get_N_exp()[diff_min+2],
       "exp/",object_evpi_decrease$get_N_ref()[diff_min+2],"ref) : ", diff[diff_min+1], "\n", sep="")

  ## reinitilisation de l'object
  object_evpi_decrease$set_N_exp(N_exp_ini)
}

#' @title Function to estimate the gamma risk
#' @description The gamma risk is the probability that a decision based on the expected mean of the Incremental Net Monetary Benefit (INMB) is wrong.
#' In other terms, it is one minus the cost-effectiveness probability of an intervention.
#' Use \link{sample_size} function first to estimate the sample size.
#' @param object_evpi_decrease : evpi_decrease object. See \link{create_object_evpi_decrease}
#' @export
gamma_risk <- function(object_evpi_decrease){
  variance_attendue <- object_evpi_decrease$get_var_inmb_expected()
  check_1(list(variance_attendue=variance_attendue))
  inmb <- object_evpi_decrease$object_inmb$get_inmb()
  pvalue <- stats::pnorm (0, inmb, sqrt(variance_attendue))
  return (pvalue)
}
