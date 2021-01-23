#load one year of data to test on
variables <-
  c('tourney_date',
    'surface',
    'winner_id',
    'loser_id',
    'winner_age',
    'loser_age')
my_data <- load_data("wta_matches_2018.csv", variables)

# testing the head to head produced a probability that makes sense
test_that("h2h", {
  train <- feature_matrix_train(my_data)$matrix
  index <- which(rownames(train) == "p1_headtohead")
  h2h <- train[index, ]
  for (i in 1:length(h2h)) {
    sumh2h <- sum(h2h + (1 - h2h))
  }
  expect_true(sumh2h == length(h2h))
})

#making sure the it only fills in surface performance for the surface played
test_that("correct_surface", {
  train <- feature_matrix_train(my_data)$matrix
  no_matches <- dim(train)[2]
  surface <- as.vector(unique(my_data$surface))
  train <- train[1:2 * length(surface), ]
  wrong_surface <- 0
  for (i in 1:no_matches) {
    non_zero <- length(which(train[, i] != 0))
    if (non_zero > 2) {
      wrong_surface <- wrong_surface + 1
    }
  }
  expect_true(wrong_surface == 0)
})

#check match form is a probability
test_that("match_form", {
  train <- feature_matrix_train(my_data)$matrix
  p1_index <- which(rownames(train) == "p1_form")
  p2_index <- which(rownames(train) == "p2_form")
  form <- train[c(p1_index, p2_index), ]
  no_matches <- dim(train)[2]
  not_prob <- 0
  for (i in 1:no_matches) {
    if (length(which(form[1, i]  < 0 ||  form[1, i]  > 1)) != 0) {
      not_prob <- not_prob + 1
    }
    if (length(which(form[2, i]  < 0 ||  form[2, i]  > 1)) != 0) {
      not_prob <- not_prob + 1
    }
  }
  expect_true(not_prob == 0)
})


#checkall ages are filled in
test_that("age", {
  train <- feature_matrix_train(my_data)$matrix
  p1_index <- which(rownames(train) == "p1_age")
  p2_index <- which(rownames(train) == "p2_age")
  age <- train[c(p1_index, p2_index), ]
  p1age <- length(which(age[1, ] == 0))
  p2age <- length(which(age[2, ] == 0))
  expect_true(p1age + p2age == 0)
})

#check all intercepts are equal to one
test_that("intercept", {
  train <- feature_matrix_train(my_data)$matrix
  index <- which(rownames(train) == "intercept")
  intercept <- train[index, ]
  expect_true(sum(intercept) == length(intercept))
})

#check that there is a sufficient flip of winner id and loser id
test_that("winner", {
  train <- feature_matrix_train(my_data)$matrix
  index <- which(rownames(train) == "winner")
  winner <- train[index, ]
  no_matches <- dim(train)[2]
  print(sum(winner))
  expect_true(sum(winner) < no_matches / 4 &&
                sum(winner) > -no_matches / 4)
})

#check the dimensions make sense
test_that("dimensions", {
  complete_data <- my_data[complete.cases(my_data), ]
  no_matches <- dim(complete_data)[1]
  features = c('surface_form', 'age', 'match_form', 'head_to_head')
  surface <- as.vector(unique(complete_data$surface))
  train <- feature_matrix_train(complete_data)$matrix
  no_matches <- dim(train)[2]
  no_features <- 2 #for winner and intercept

  if ('surface_form' %in% features == TRUE) {
    no_features <- no_features + 2 * length(surface)
  }
  if ('age' %in% features == TRUE) {
    no_features <- no_features + 2
  }
  if ('match_form' %in% features == TRUE) {
    no_features <- no_features + 2
  }
  if ('head_to_head' %in% features == TRUE) {
    no_features <- no_features + 1
  }

  expect_true(dim(train)[1] == no_features &&
                dim(train)[2] == no_matches)
})
