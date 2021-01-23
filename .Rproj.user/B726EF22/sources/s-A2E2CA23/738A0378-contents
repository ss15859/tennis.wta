test_that("bradley_terry", {

  #let's first analyze if test_score works
  players <- 1:10
  lambda_hat <- runif(10)
  player_one <- 3 #true case
  player_two <- 11 #false case
  output_one <- extract_score(player_one, lambda_hat, players)
  output_two <- extract_score(player_two, lambda_hat, players)
  expect_true(output_one==lambda_hat[player_one]) #check true case
  expect_true(output_two==-1) #check false case


  #now let's start testing predict winner
  test_dat <- data.frame(winner_id=c(2,1), loser_id=c(3,11))
  output_func <- predict_winner(test_dat,lambda_hat,players)
  expect_true(output_func[1]==(lambda_hat[2]/(lambda_hat[2]+lambda_hat[3]))) #first one should pass
  expect_true(output_func[2]==0.5) #second should fail

  #let's now test w_fill
  data_mat <- data.frame(winner_id=c(1,2,3,4,5,6,7,8), loser_id=c(10,9,9,3,1,8,4,9))
  w_output <- w_fill(data_mat, players)
  true_w <- matrix(0,10,10)
  true_w[1,10] <- 1; true_w[2,9] <- 1; true_w[3,9] <- 1; true_w[4,3] <- 1; true_w[5,1] <- 1;
  true_w[6,8] <- 1; true_w[7,4] <- 1; true_w[8,9] <- 1;
  expect_true(prod(true_w==w_output)==1)


})
