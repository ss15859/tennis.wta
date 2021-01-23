test_that("EM", {

  K <- 10
  lambda <- 1:K
  a <- 0.5
  b <- 2
  w <- matrix(round(runif(K^2,0,5)),K,K)
  diag(w) <- 0
  n <- (w+t(w))


  alg_output <- EM_step(lambda, w, a, b, K, n)

  temp_output <- matrix(0, K, K)
  test_output <- rep(0,K)
  for (i in 1:K) {
    for (j in 1:K) {
      temp_output[i,j] <- n[i,j]/(lambda[i]+lambda[j])
    }
  }
  diag(temp_output) <- 0
  for (i in 1:K) {
    test_output[i] <- (a-1+sum(w[i,]))/(b + sum(temp_output[i,]))
  }

  expect_true(prod(test_output==alg_output)==1)



})
