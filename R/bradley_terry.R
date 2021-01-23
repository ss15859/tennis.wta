#' Construct the matrix of players wins/losses
#'
#' Constructs a matrix with (i,j) which denotes the number of matches where player i beats player j.
#'
#' @usage w_fill(data_mat, players)
#'
#' @param data_mat table containing matches with variables winner_id and loser_id.
#' @param players vector containing a list of players from which to construct the matrix.
#'
#' @return A square matrix with dimension equal to the number of players
#' @export

w_fill <- function(data_mat, players) {
  #constructs w matrix
  no_players <- length(players)
  w <- matrix(0, no_players, no_players) #initialize
  for (i in 1:no_players) {
    player <- players[i] #choose our first player
    win_ind <-
      which(data_mat$winner_id == player) #find the indexes in which this player wins
    los_ids <-
      data_mat[win_ind, ]$loser_id #find the ids of the players this player beat
    for (j in 1:length(los_ids)) {
      #for each loser
      los_ind <-
        which(players == los_ids[j]) #find their respective index in the matrix w
      w[i, los_ind] <-
        w[i, los_ind] + 1 #update the w matrix for our player
    }
  }
  return(w)
}


#' Find the predicted Bradley-Terry score of a player
#'
#' Finds the predicted score of a player following the use of the EM algorithm. If no such score exists for a player -1 is returned as the score for that player.
#'
#' @usage extract_score(player,lambda_hat,players)
#'
#' @param player id of the player for whom we would like the estimated Bradley-Terry score.
#' @param lambda_hat vector contain a list of all the predicted scores from the EM algorithm.
#' @param players vector containing a list of players for which there is a predicted Bradley-Terry score.
#'
#' @return The predicted score of the player. If the player is not found within the list of players then -1 is returned.
#'
#' @export
#'

extract_score <- function(player, lambda_hat, players) {
  if (player %in% players) {
    lambda_hat[which(players == player)]
  }
  else {
    return(-1)
  }
}

#' Predict the winner of a series of matches
#'
#' Finds the probability of player1 winning in each match, using the predicted Bradley-Terry scores.
#'
#' @usage predict_winner(test_dat,lambda_hat,players)
#'
#' @param test_dat a matrix containing the matches played.
#' @param lambda_hat vector contain a list of all the predicted scores from the EM algorithm.
#' @param players vector containing a list of players for which there is a predicted Bradley-Terry score.
#'
#' @return The probability that player 1 beats player 2 using the Bradley-Terry formula: lambda_1/(lambda_1+lambda_2). If either of the player does not have a predicted score then 0.5 is returned to express uncertainty.
#'
#' @export
#'

predict_winner <- function(test_dat, lambda_hat, players) {
  prob_of_win <- rep(0, length(test_dat[, 1]))

  for (i in 1:length(test_dat[, 1])) {
    player1 <- test_dat$winner_id[i]
    player2 <- test_dat$loser_id[i]

    score1 <-  extract_score(player1, lambda_hat, players)

    score2 <-  extract_score(player2, lambda_hat, players)

    prob_of_win[i] <- score1 / (score1 + score2)

    if (score1 < 0 || score2 < 0)
      prob_of_win[i] <- 0.5
  }
  return(prob_of_win)
}
