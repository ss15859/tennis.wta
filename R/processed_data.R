#' Load data
#'
#' Loads multiple files of data and extracts features that are desired. It also formats the date and puts data in date order.
#'
#' @usage load_data(file_list, variables = FALSE)
#'
#' @param file_list a list of files to be loaded.
#' @param variables the variables from the files that we wish to load.
#'
#' @return A data frame with columns for each variable and each row is a match.
#' @export
#'
#' @examples
#' load_data("wta_matches_2000.csv")
#' load_data(file_list, variables = c('tourney_date','surface', 'winner_id', 'loser_id', 'winner_age', 'loser_age'))

load_data <- function(file_list, variables = FALSE) {
  no_files <- length(file_list)
  for (file_index in 1:no_files) {
    path <-
      system.file("extdata", file_list[[file_index]], package = "tennis.wta")
    temp_file <- read.csv(path)
    if (class(variables) == "character") {
      temp_file <- subset(temp_file, select = variables)
    }
    if (file_index == 1) {
      melted_files <- temp_file
    } else {
      melted_files <- rbind(melted_files, temp_file)
    }
  }
  my_data <- melted_files
  my_data$tourney_date <-
    as.Date(as.character(my_data$tourney_date), format = "%Y%m%d")
  my_data <-
    my_data[order(as.Date(my_data$tourney_date, format = "%Y-%m-%d")),]
  row.names(my_data) <- seq(1:dim(my_data)[1])
  return(my_data)
}

#loads data and sorts out date, then puts in date order
#variables is the list of which columns we will be using
# path_allfiles <- system.file("extdata", package = "tennis.wta")
# file_list = list.files(path_allfiles)
# variables <- c('tourney_date','surface', 'winner_id', 'loser_id', 'winner_age', 'loser_age')
# my_data <- load_data(file_list, variables)
