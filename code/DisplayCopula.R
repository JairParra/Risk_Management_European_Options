f_display_copula <- function(my_copula, grid_1, grid_2, type = 1) {
  
  n_mesh <- length(grid_1) 
  
  f_U <- matrix(NA, n_mesh, n_mesh) 
  for (j in 1:n_mesh) {
    for (k in 1:n_mesh) {
      u <- c(grid_1[j],  grid_2[k])         
      f_U[j,k] <- my_copula(u)
    }
  }
  
  if (type == 1) {
    persp(x = grid_1, 
          y = grid_2, 
          z = f_U, 
          xlab = "U_1", 
          ylab = "U_2", 
          zlab = "Pdf",
          theta = 30, 
          r = 20, 
          col = "yellow", 
          ticktype = "detailed", 
          nticks = 4, 
          expand = 0.5)
  }
  if (type == 2) {
    rgl::persp3d(x = grid_1, y = grid_2, z = f_U, add = FALSE)
  }
}