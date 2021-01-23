# Tennis Match Prediction Package

This is an R package intended for use in the application of logistic regression and the Bradley-Terry model to tennis match data.

We include the data for wta matches from the years 2000-2018 as well as the function load data to combine these data files into a data frame with the features (columns) we require. 
There is also functions to check that the matrix of number of matches players have won/lost against each other is strongly connected and to reduce if needed. This allows the Bradley-Terry model to be run using the EM algorithm to learn the 'skill' parameters of players. 
This package allows users to construct test and train matrices from the data with desired features that are calculated from the data, then the train matrix can be used to run logistic regression.

The usage of functions is well documented in the R markdown document contained in the package. 
