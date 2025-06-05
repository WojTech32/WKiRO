generate__multiclass_poisson_data <- function(n = 100, lambdas = list(c(5, 5), c(15, 15)), range = 25) {
  set.seed(42)
  
  all_points <- data.frame()
  
  for (i in seq_along(lambdas)) {
    lambda <- lambdas[[i]]
    
    x <- rpois(n, lambda = lambda[1])
    y <- rpois(n, lambda = lambda[2])
    group_data <- data.frame(x = x, y = y, group = as.factor(i))
    
    all_points <- rbind(all_points, group_data)
  }
  
  # Plotting
  colors <- rainbow(length(lambdas), alpha = 0.5)
  plot(all_points$x, all_points$y, 
       col = colors[as.numeric(all_points$group)],
       pch = 19, xlim = c(0, range), ylim = c(0, range),
       xlab = "x", ylab = "y", main = "Multiple Poisson Groups")
  
  # Mark centers
  for (i in seq_along(lambdas)) {
    lambda <- lambdas[[i]]
    points(lambda[1], lambda[2], col = colors[i], pch = 4, lwd = 2, cex = 2)
  }
  
  legend("topright", legend = paste("Group", seq_along(lambdas)), 
         col = colors, pch = 19)
  
  return(all_points)
}
