#' One step of EM algorithm
#'
#' @usage EM_step(lambda,W,a,b,K,n)
#'
#' @param lambda current values for the estimates of lambda.
#' @param W (KxK) matrix where the (i,j) element is number of matches player i has beaten player j.
#' @param a shape parameter for Gamma prior.
#' @param b rate parameter for Gamma prior.
#' @param K number of players.
#' @param n W + t(W) - (i,j) element is total number of times player i has played player j.
#'
#' @return Updated estimate of lambdas.
#' @export

EM_step <- function(lambda, W, a, b, K, n) {
  # We create a temporary matrix that allows the updating step to be simplified

  temp <- matrix(0, K, K)
  for (i in 1:K) {
    for (j in 1:K) {
      if (i != j)
        temp[i, j] <- n[i, j] / (lambda[i] + lambda[j])
    }
  }

  # We perform the update on the estimated lambdas

  for (i in 1:K) {
    lambda[i] = (a - 1 + sum(W[i, ])) / (b + sum(temp[i, ]))
  }

  return(lambda)
}


#' EM algorithm
#'
#' Performs the update from function EM_step_step n times.
#' A threshold is defined such that the function will break if the change in lambda is smaller than the threshold value.
#'
#' @usage EM(W,nsteps,a,b,eps)
#'
#' @param W (KxK) matrix where the (i,j) element is number of matches player i has beaten player j.
#' @param nsteps number of iterations.
#' @param a shape parameter for Gamma prior.
#' @param b rate parameter for Gamma prior.
#' @param eps threshold for change in lambda.
#'
#' @return The final estimates of lambdas.
#' @export


EM <- function(W, nsteps, a, b, eps) {
  # Initialise Variables

  K <- length(W[1, ])
  n <- matrix(0, nrow = K, ncol = K)
  n <- W + t(W)
  lambda <- runif(K, 10, 20)

  # Perform EM_step for nstep times, but break if within threshold.

  for (t in 1:nsteps) {
    lambda_temp <- EM_step(lambda, W, a, b, K, n)

    if (sqrt(sum((lambda - lambda_temp) ^ 2)) < eps) {
      cat('Converge after', t, 'steps', '\n')

      return(lambda_temp)
    } # Stopping Condition
    lambda <- lambda_temp / sum(lambda_temp)
  }
  return(lambda)
}
