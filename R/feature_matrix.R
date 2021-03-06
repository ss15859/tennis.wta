#' Head to head performance
#'
#' Calculates the number of matches won/loss against each other. However, if players have never played each other both outputs are set to 0.5.
#'
#' @usage head_to_head(my_data, player1, player2)
#'
#' @param my_data input data.
#' @param player1 player 1 id.
#' @param player2 player 2 id.
#'
#' @return Vector with two values, first, the percentage of games player 1 beat player 2 and second, the percentage of games player 2 beat player 1.
#' @export

head_to_head <- function(my_data, player1, player2) {
  won_player1 <-length(which((my_data$winner_id) == player1 & (my_data$loser_id) == player2))
  won_player2 <- length(which((my_data$winner_id) == player2 & (my_data$loser_id) == player1))
  total_matches <- won_player1 + won_player2
  if (total_matches == 0) {
    # if players have never played we set each percentage to 0.5
    percent_won_player1 <- 0.5
    percent_won_player2 <- 0.5
  } else{
    percent_won_player1 <- won_player1 / total_matches
    percent_won_player2 <- won_player2 / total_matches
  }
  return(c(percent_won_player1, percent_won_player2))
}


#' Historical performance of player
#'
#' Calculates the percentages of matches won from all matches in the data set.
#'
#' @usage match_based_form_total(my_data, player)
#'
#' @param my_data input data.
#' @param player player id.
#'
#' @return The percentage of total matches won.
#' @export


match_based_form_total <- function(my_data, player){
  # finds indexes of matches where player played
  row_index_win <- which(my_data$winner_id == player)
  row_index_lose <- which(my_data$loser_id == player)
  row_index <- sort(c(row_index_win, row_index_lose))
  n <- length(row_index)
  #counts wins
  wins <- 0
  for (i in 1:n){
    if(my_data$winner_id[row_index[i]] == player){
      wins <- wins +1
    }else{
      wins <- wins
    }
  }
  return(wins/n)
}

#' Performance of player on different surfaces
#'
#' Calculates percentage of matches won by a player on different surfaces.
#'
#' @usage surface_based_form_total(my_data, player, surface)
#'
#' @param my_data input data.
#' @param player player id.
#' @param surface a list of surfaces played on.
#'
#' @return A list with percentages of matches won on that surface.
#' @export

surface_based_form_total <-
  function(my_data, player,surface) {
    #indexes where player played
    row_index_win <- which(my_data$winner_id == player)
    row_index_lose <- which(my_data$loser_id == player)
    row_index <- sort(c(row_index_win, row_index_lose))
    #gets relevant rows
    my_data <- my_data[row_index, ]
    #wins one each surface
    for (i in 1:length(surface)) {
      surface_id <- which(my_data$surface == surface[i])
      n_surface <- length(surface_id)
      wins <- 0
      if (n_surface == 0) {
        wins <- 0
        n_surface <- 1
      } else{
        for (j in 1:n_surface) {
          if (my_data[surface_id,]$winner_id[j] == player) {
            wins <- wins + 1
          } else{
            wins <- wins
          }
        }
      }
      surface[i] <- wins / n_surface
    }
    return(as.numeric(surface))
  }

#' Construct player data frame
#'
#' Creates a data frame of all players using functions surface_based_form_total and match_based_form_total.
#'
#' @usage player_data_frame(my_data)
#'
#' @param my_data input data.
#'
#' @return A data frame with columns: player_id, age, one for each surface and form.
#' @export


player_data_frame <- function(my_data){
  #binds player ids and ages
  players_age_winner <- data.frame(player_id = my_data$winner_id, age = my_data$winner_age)
  players_age_loser <- data.frame(player_id = my_data$loser_id, age = my_data$loser_age)
  player_data <- rbind(players_age_winner,players_age_loser)
  #get rid of missing values
  player_data <- player_data[complete.cases(player_data), ]
  player_data <- player_data[!duplicated(player_data[,1],fromLast = TRUE),]
  no_players <- dim(player_data)[1]

  surface <- as.vector(unique(my_data$surface))
  if(length(which(surface == "")) != 0){
    surface <- surface[-which(surface == "")]
  }
  no_surfaces <- length(surface)

  surface_data <-
    matrix(0, ncol = no_surfaces, nrow = no_players)
  form <-
    matrix(0, ncol =1, nrow = no_players)
  #calculates surface based form and match based form
  for (j in 1:no_players) {
    surface_data[j,] <-
      surface_based_form_total(my_data, player_data[j,1],surface)
    form[j,] <-
      match_based_form_total(my_data, player_data[j,1])
  }
  colnames(surface_data) <- surface[1:no_surfaces]
  player_data <- cbind(player_data, surface_data, form)
  rownames(player_data) <- 1: no_players
  return(data.frame(player_data))
}

#' Matrix of surface features
#'
#' Let n be the number of surfaces and m the number of matches . Creates a (2n x m) matrix where the first n rows describe surface features for player 1 and the next n rows describe surface features for player 2.
#' Each column signifies a match and the only non-zero elements of that column are for the 2 rows (one for each player) which describe the surface on which the match was played.
#' The two values for the column will be the performance of player 1 and player 2 respectively on that surface.
#'
#' @usage feature_surface(my_data, no_matches, player_data, p1_p2, surface, carpet = 'FALSE')
#'
#' @param my_data input data.
#' @param  no_matches number of matches played.
#' @param player_data player data frame which is the output of function player_data_frame.
#' @param p1_p2 a matrix where each column signifies a match and the two rows are the player ids for player 1 and player 2 for that match.
#' @param surface a list of surfaces played on.
#' @param carpet as matches are no longer played on carpet, carpet performance is by default not included. However, carpet can be included by including, carpet == 'TRUE'.
#'
#' @return A matrix of players surface features depending on which surface the match is played on.
#' @export
#'
#' @examples
#' feature_surface(data, no_matches = 1000, player_data, p1_p2, c("Hard","Clay", "Grass"))
#' feature_surface(data, no_matches = 31205, player_data, p1_p2, c("Hard","Clay", "Grass", "Carpet"), carpet = 'TRUE')

feature_surface <- function(my_data, no_matches, player_data, p1_p2, surface, carpet = 'FALSE'){
  no_surfaces <- length(surface)
  surface_matrix <- matrix(0,nrow = 3*no_surfaces, ncol = length(p1_p2[1,]))
  #gets rid of blank rows and carpet rows
  if (carpet == 'FALSE' && length(which(my_data$surface == "Carpet")) != 0){
    surface_id <- which(my_data$surface == "Carpet")
    my_data <- my_data[-surface_id,]
 }

  for (i in 1: no_surfaces){
    surface_id <- which(my_data$surface == surface[i])
    surface_matrix[i,surface_id] <- 1
    rownames(surface_matrix) <- c(surface,surface,surface)
  }
  #fills in players surface performance for each column (each match)
  for (i in 1: length(p1_p2[1,])){
    player1 <- p1_p2[1,i]
    player2 <- p1_p2[2,i]
    player1_id <- which(player_data[,1] == player1)
    player2_id <- which(player_data[,1] == player2)
    player1_surface <- player_data[player1_id,no_surfaces:(2*no_surfaces-1)]
    player2_surface <- player_data[player2_id,no_surfaces:(2*no_surfaces-1)]
    for(j in 1: no_surfaces){
      if (surface_matrix[j,i] ==1){
        surface_matrix[j+no_surfaces,i] <- as.numeric(player1_surface[j])
        surface_matrix[j+2*no_surfaces,i] <- as.numeric(player2_surface[j])
      } else{
        surface_matrix[j+no_surfaces,i] <- 0
        surface_matrix[j+2*no_surfaces,i] <- 0
      }
    }
  }
  surface_matrix <- surface_matrix[-c(1:no_surfaces),]
  return(surface_matrix)
}

#' Training feature matrix
#'
#' Makes the feature matrix to train on based on which features are chosen.
#' Uses functions: player_data_frame, head_to_head, feature_surface.
#'
#' @usage feature_matrix_train(my_data,match_date = "2019-01-01",features = c('surface_form', 'age', 'match_form', 'head_to_head'), carpet = 'FALSE')
#'
#' @param my_data input data.
#' @param match_date set to the starting date of test data - by default this is 2019-01-01.
#' @param features a list of features wanted to be included. By default all features are used.
#' @param carpet as matches are no longer played on carpet, carpet performance is by default not included. However, carpet can be included by including, carpet == 'TRUE'
#'
#' @return Creates a matrix where each row is a feature and each column is a match. Additionally, a row of ones is added for the intercept and a row which signifies the winner of that match -
#' set to +1 if player 1 one and -1 if player 2 won.
#' @export
#'
#' @examples
#' feature_matrix_train(data)
#' feature_matrix_train(my_data,match_date = "2018-05-11",features = c('surface_form', 'match_form', 'head_to_head'), carpet = 'TRUE')
#'


feature_matrix_train <-function(my_data,match_date = "2019-01-01",features = c('surface_form', 'age', 'match_form', 'head_to_head'), carpet = 'FALSE') {
  #get rid of matches before match date
  my_data <-
    my_data %>% filter(tourney_date < as.character(match_date))
  #get rid of data with missing values
  my_data <- my_data[complete.cases(my_data),]
  no_matches <- dim(my_data)[1]

  #load player data frame
  player_data <- player_data_frame(my_data)
  no_players <- dim(player_data)[1]

  surface <- as.vector(unique(my_data$surface))
  if(length(which(surface == "")) != 0){
    surface <- surface[-which(surface == "")]
  }
  no_surfaces <- length(surface)

  #gets rid of blank and carpet
  if (carpet == 'FALSE' && length(which(my_data$surface == "Carpet")) != 0){
    carpet <- which(surface == 'Carpet')
    surface <- surface[-carpet]
    no_surfaces <- length(surface)
  }

  p1_age <-
    p2_age <-
    p1_form <-
    p2_form <- p1_headtohead <- matrix(0, nrow = 1, ncol = no_matches)

  winner <- rep(0, no_matches)
  uni_samples <- runif(no_matches)
  p1_p2 <- matrix(0, nrow = 2, ncol = no_matches)
  rownames(p1_p2) <- c('player1','player2')
  p1_p2[1, ] <- my_data$winner_id
  p1_p2[2, ] <- my_data$loser_id

  #randomly splits approx. half of player 1 and player 2
  for (i in 1:no_matches) {
    if (uni_samples[i] < 0.5) {
      winner[i] <- 1
    } else {
      swap <- p1_p2[2, i]
      p1_p2[2, i] <- p1_p2[1, i]
      p1_p2[1, i] <- swap
      winner[i] <- -1
    }

    player1 <- p1_p2[1,i]
    player2 <- p1_p2[2,i]
    player1_id <- which(player_data[, 1] == player1)
    player2_id <- which(player_data[, 1] == player2)

    p1_age[i] <- player_data$age[player1_id]
    p2_age[i] <- player_data$age[player2_id]
    p1_form[i] <- player_data$form[player1_id]
    p2_form[i] <- player_data$form[player2_id]
    p1_headtohead[i] <- head_to_head(my_data, player1, player2)[1]
  }
  rownames(p1_age) <- 'p1_age' ; rownames(p2_age) <- 'p2_age'; rownames(p1_form) <- 'p1_form'
  rownames(p2_form) <- 'p2_form';rownames(p1_headtohead) <- 'p1_headtohead'
  surface_matrix <-
    feature_surface(my_data,no_matches,player_data,p1_p2,surface, carpet)
  intercept <- rep(1, no_matches)
  feature_matrix <- p1_p2

  #only include the features we want
  if ('surface_form' %in% features == TRUE) {
    feature_matrix <- rbind(feature_matrix, surface_matrix)
  }
  if ('age' %in% features == TRUE) {
    feature_matrix <- rbind(feature_matrix, p1_age, p2_age)
  }
  if ('match_form' %in% features == TRUE) {
    feature_matrix <- rbind(feature_matrix, p1_form, p2_form)
  }
  if ('head_to_head' %in% features == TRUE) {
    feature_matrix <- rbind(feature_matrix, p1_headtohead)
  }
  feature_matrix <- rbind(feature_matrix, intercept, winner)
  feature_matrix <- feature_matrix[-c(1,2),]
  #returns feature matrix and list of player 1 and player 2 for each match
  return(list('matrix' = feature_matrix, 'players' = p1_p2))
}

#' Test feature matrix
#'
#' Makes the feature matrix to test model based on which features are chosen. Uses functions: player_data_frame, head_to_head, feature_surface.
#' As this is the testing matrix, the columns are the matches we wish to predict but the player characteristics are still calculated from the training data.
#'
#' @usage feature_matrix_test(my_data,match_date = "2019-01-01",features = c('surface_form', 'age', 'match_form', 'head_to_head'), carpet = 'FALSE')
#'
#' @param my_data input data.
#' @param match_date set to the starting date of test data - be default is 2019-01-01.
#' @param features a list of features wanted to be included. By default all features are used.
#' @param carpet as matches are no longer played on carpet, carpet performance is by default not included. However, carpet can be included by including, carpet == 'TRUE'.
#'
#' @return Creates a matrix where each row is a feature and each column is a match. Additionally, a row of ones is added for the intercept.
#' @export
#'
#' @examples
#' feature_matrix_test(my_data)
#' feature_matrix_test(my_data,match_date = "2016-01-01",features = c('surface_form', 'age'), carpet = 'TRUE')
#'

feature_matrix_test <- function(my_data,match_date = "2019-01-01",features = c('surface_form', 'age', 'match_form', 'head_to_head'), carpet = 'FALSE'){
  #loads test data and train data
  test_data <- my_data %>% filter(tourney_date > as.character(match_date))
  test_data <- test_data[complete.cases(test_data), ]
  train_data <-
    my_data %>% filter(tourney_date < as.character(match_date))
  #get rid of data with missing values
  train_data <- train_data[complete.cases(train_data),]


  p1_p2 <- matrix(0, ncol= length(test_data[,1]), nrow= 2)
  rownames(p1_p2) <- c('player1','player2')
  p1_p2[1,] <- test_data$winner_id
  p1_p2[2,] <- test_data$loser_id

  player_data <- player_data_frame(train_data)

  #gets rid of matches with new players
  index <- rep(0,0)
  for(i in 1:length(p1_p2[1,])){
    if((!(p1_p2[1,i] %in% player_data[,1])) || (!(p1_p2[2,i] %in% player_data[,1])) ){
      index <- cbind(index,i)
    }
  }
  p1_p2 <- p1_p2[,-index]
  no_matches <- length(p1_p2[1,])

  test_data <- test_data[-index,]
  rownames(test_data) <- seq(1:dim(test_data)[1])

  p1_age <-
    p2_age <-
    p1_form <-
    p2_form <- p1_headtohead <- matrix(0, nrow = 1, ncol = no_matches)


  surface <- as.vector(unique(test_data$surface))
  if(length(which(surface == "")) != 0){
    surface <- surface[-which(surface == "")]
  }
  no_surfaces <- length(surface)

  for (i in 1:no_matches) {
    player1 <- p1_p2[1,i]
    player2 <- p1_p2[2,i]
    player1_id <- which(player_data[, 1] == player1)
    player2_id <- which(player_data[, 1] == player2)

    p1_age[i] <- player_data$age[player1_id]
    p2_age[i] <- player_data$age[player2_id]
    p1_form[i] <- player_data$form[player1_id]
    p2_form[i] <- player_data$form[player2_id]
    p1_headtohead[i] <- head_to_head(train_data, player1, player2)[1]
  }
  rownames(p1_age) <- 'p1_age' ; rownames(p2_age) <- 'p2_age'; rownames(p1_form) <- 'p1_form'
  rownames(p2_form) <- 'p2_form';rownames(p1_headtohead) <- 'p1_headtohead'
  surface_matrix <-
    feature_surface(test_data,no_matches,player_data,p1_p2,surface, carpet)
  intercept <- rep(1, no_matches)
  feature_matrix <- p1_p2

  #only include features we want
  if ('surface_form' %in% features == TRUE) {
    feature_matrix <- rbind(feature_matrix, surface_matrix)
  }
  if ('age' %in% features == TRUE) {
    feature_matrix <- rbind(feature_matrix, p1_age, p2_age)
  }
  if ('match_form' %in% features == TRUE) {
    feature_matrix <- rbind(feature_matrix, p1_form, p2_form)
  }
  if ('head_to_head' %in% features == TRUE) {
    feature_matrix <- rbind(feature_matrix, p1_headtohead)
  }
  feature_matrix <- rbind(feature_matrix, intercept)
  feature_matrix <- feature_matrix[-c(1,2),]
  #returns test matrix
  return(list('matrix' = feature_matrix, 'players' = p1_p2))
}
