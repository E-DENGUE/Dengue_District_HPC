#FROM STACKR package by Nikos Bosse, Sebastian Funk, Sam Abbott
#https://github.com/epiforecasts/stackr/blob/main/R/stacking_weights_crps.R
#'   \item{geography}{the location where values where observed and predicted}
#'   \item{model}{the model used to generate predictions}
#'   \item{sample_nr}{the sample number that identifies a predictive sample
#'   for a specific geography and date}
#'   \item{date}{the date for which the prediction was made and the true
#'   value was observed}
#'   \item{y_pred}{the value predicted for a given date and region}
#'   \item{y_obs}{the true value observed for a given date and region}

crps_weights <- function(data,
                         lambda = "equal",
                         gamma = NULL,
                         dirichlet_alpha = 1.001) {
  data.table::setDT(data)
  
  # check if geography exists. if not, create a region
  if (!("geography" %in% names(data))) {
    data <- data[, geography := "Atlantis"]
  }
  
  # number of models
  models <- unique(data$model)
  k <- length(models)
  
  # number of regions
  regions <- unique(data$geography)
  r <- length(regions)
  
  # number of predictive samples
  s <- max(data$sample_nr)
  
  # get number of timepoints
  dates <- unique(data$date)
  t <- length(dates)
  
  # turn predictions into array that can be passed to the stan model
  pred_array <- array(data[order(model, sample_nr, geography)]$y_pred,
                      dim = c(t, r, s, k)
  )
  
  # turn observations into array that can be passed to the stan model
  y <-
    data[sample_nr == 1 & model == models[1]][order(date, geography)][, y_obs]
  y_array <- array(y, dim = c(r, t))
  
  # assign increasing or equal weights if no lambda vector is provided
  #if (is.null(lambda)) {
  #  lambda <- 2 - (1 - (1:t / t))^2
  #} else if (lambda == "equal") {
    lambda <- rep(1 / t, t)
  #}
  
  # assign equal weights to regions if no gamma is provided
  if (is.null(gamma)) {
    gamma <- array(rep(1 / r, r))
  }
  
  standata <- list(
    K = k,
    R = r,
    T = t,
    S = s,
    predict_sample_mat = pred_array,
    y = y_array,
    lambda = lambda,
    gamma = gamma,
    dirichlet_alpha = dirichlet_alpha
  )
  
  model <- stanmodels$stacking_weights_crps
  opt <- rstan::optimizing(model, data = standata)
  return(opt$par)
}