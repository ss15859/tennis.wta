test_that("log_reg", {
  #generate toy data
  sigma <- matrix(0.5*c(4,2,2,5),nrow=2,ncol=2)
  C1 <- cbind(mvrnorm(300,c(0,0),sigma),1,1)
  C2 <- cbind(mvrnorm(400,c(3,3),sigma),1,-1)
  Dat <- cbind(t(C1),t(C2))
  x_dat <- Dat[1:3,]
  y_dat <- Dat[4,]
  w_logreg <- log_reg(x_dat,y_dat)
  y_glm <- y_dat==1
  glm_output <- glm(y_glm~x_dat[1,]+x_dat[2,],family=binomial)

  order = c(2,3,1)
  w_glm <- glm_output$coefficients[order]
  epsilon = 0.01
  test1 <- sqrt(sum(abs(w_glm-w_logreg)^2)) < epsilon
  test2 <- sqrt(sum(abs(w_glm+w_logreg)^2)) < epsilon
  test3 <- test1 || test2

  expect_true(test3)

})
