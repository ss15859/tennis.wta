% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_matrix.R
\name{feature_matrix_train}
\alias{feature_matrix_train}
\title{Training feature matrix}
\usage{
feature_matrix_train(my_data,match_date = "2019-01-01",features = c('surface_form', 'age', 'match_form', 'head_to_head'), carpet = 'FALSE')
}
\arguments{
\item{my_data}{input data.}

\item{match_date}{set to the starting date of test data - by default this is 2019-01-01.}

\item{features}{a list of features wanted to be included. By default all features are used.}

\item{carpet}{as matches are no longer played on carpet, carpet performance is by default not included. However, carpet can be included by including, carpet == 'TRUE'}
}
\value{
Creates a matrix where each row is a feature and each column is a match. Additionally, a row of ones is added for the intercept and a row which signifies the winner of that match -
set to +1 if player 1 one and -1 if player 2 won.
}
\description{
Makes the feature matrix to train on based on which features are chosen.
Uses functions: player_data_frame, head_to_head, feature_surface.
}
\examples{
feature_matrix_train(data)
feature_matrix_train(my_data,match_date = "2018-05-11",features = c('surface_form', 'match_form', 'head_to_head'), carpet = 'TRUE')

}
