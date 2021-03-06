#' Sigmoid function
#'
#' Define the sigmoid function.
#'
#' @usage sigma_f(t)
#'
#' @param t input to the sigmoid function.
#'
#' @return Output of sigmoid function.
#' @export

sigma_f <- function(t) {
  return(1 / (1 + exp(t)))
}

#' Likelihood function
#'
#' Define the negative log-likelihood function to be maximised.
#'
#' @usage log_reg_naive(w, x, y)
#'
#' @param w matrix where the (i,j) element is number of matches player i has beaten player j.
#' @param x feature matrix for training data.
#' @param y output for training data.
#'
#' @return The negative log-likelihood.
#' @export

log_reg_naive <- function(w, x, y) {
  f <- t(w) %*% as.matrix(x)
  return(-sum(log(sigma_f(f * y))))
}

#' Logistic regression
#'
#' Define a function that returns the estimated weight parameters using logistic regression.
#'
#' @usage log_reg(x,y)
#'
#' @param x feature matrix for training data.
#' @param y output for training data.
#'
#' @return The estimated weight parameters.
#' @export

log_reg <- function(x_dat, y_dat) {
  w <- rnorm(length(x_dat[, 1])) # We randomly initialise the weights

  log_reg_naive(w, x_dat, y_dat)

  # We use optim() to maximise the likelihood, this uses Nelder-Mead as a default

  optim(
    rep(0, length(x_dat[, 1])),
    log_reg_naive,
    x = x_dat,
    y = y_dat,
    control = list(maxit = 200000)
  )$par
}
