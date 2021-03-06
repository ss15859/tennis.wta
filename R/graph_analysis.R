#' Check for strongly connected graph
#'
#' Determines whether a matrix forms a strongly connected graph.
#'
#' @usage strongly_connected(w)
#'
#' @param w matrix to check.
#'
#' @return A statement that is either: 'The graph is strongly connected' or if the graph is not strongly connected then:
#' 'The graph contains n strongly connected components and the largest one contains m nodes' where n and m will be determined.
#' @export
#'

strongly_connected <- function(w) {
  adj <- w
  adj[which(adj != 0)] <- 1 # converts w into an adjacency matrix
  g <- graph_from_adjacency_matrix(adj, mode = "directed")
  temp <-
    components(g, mode = "strong") # computes the strongly connected components of the adjacency matrix
  if (temp$no == 1) {
    cat("The graph is strongly connected")
  }
  else
    cat(
      'The graph contains',
      temp$no,
      'strongly connected components \nThe largest one contains',
      max(temp$csize),
      'nodes'
    )

}

#' Forms strongly connected graph
#'
#' Reduces a matrix to one that forms a strongly connected graph.
#'
#' @usage reduce_to_connected(w)
#'
#' @param w matrix to be reduced.
#'
#' @return The reduced matrix.
#' @export
#'

reduce_to_connected <- function(w) {
  adj <- w
  adj[which(adj != 0)] <- 1 # converts w into an adjacency matrix
  g <- graph_from_adjacency_matrix(adj, mode = "directed")
  temp <- components(g, mode = "strong")
  if (temp$no == 1) {
    cat("The graph is strongly connected")
    return(w)
  }
  else {
    comp_ind <-
      which(temp$csize == max(temp$csize)) #maximal component index
    player_index <-
      which(temp$membership == comp_ind) # finds the players in the maximal component
    return(list(matrix = w[player_index, player_index], new_index = player_index))
  }

}

#' Plots graph of matrix
#'
#' Plots the graph formed by a matrix.
#'
#' @usage plot_comparisons(w)
#'
#' @param w matrix to be plotted.
#'
#' @return The graph of matrix.
#' @export


plot_comparisons <- function(w) {
  adj <- w
  adj[which(adj != 0)] <- 1
  g <- graph_from_adjacency_matrix(adj, mode = "directed")
  tkplot(g)

}

