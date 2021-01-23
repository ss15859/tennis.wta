test_that("graph_analysis", {

  #create w which is not connected
  w <- matrix(c(0,1,1,0,1,1,1,1,0,0,1,0,0,0,1,0,1,0,0,1,1,1,0,0,1), 5, 5, byrow=TRUE)
  g <- graph_from_adjacency_matrix(w, mode="directed")
  temp <- components(g, mode = "strong") #we see it fails test

  w_new <- reduce_to_connected(w)
  mat_new <- w_new$matrix
  ind_new <- w_new$new_index
  g_new <- graph_from_adjacency_matrix(mat_new, mode="directed")
  temp_new <- components(g_new, mode = "strong")

  expect_true(temp_new$no==1)
  expect_true(prod(mat_new==w[ind_new,ind_new])==1)
})
