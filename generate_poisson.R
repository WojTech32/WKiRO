generate_poisson_data <- function(n = 200, lambda1 = c(5, 5), lambda2 = c(15, 15), range = 25) {
  set.seed(42)
  
  # Group 1
  x1 <- rpois(n, lambda = lambda1[1])
  y1 <- rpois(n, lambda = lambda1[2])
  group1 <- data.frame(x = x1, y = y1, group = 1)
  
  # Group 2
  x2 <- rpois(n, lambda = lambda2[1])
  y2 <- rpois(n, lambda = lambda2[2])
  group2 <- data.frame(x = x2, y = y2, group = 2)
  
  # Combine into one data frame
  all_points <- rbind(group1, group2)
  
  # Plotting
  plot(all_points$x, all_points$y, 
       col = ifelse(all_points$group == 1, rgb(1, 0.5, 0, 0.5), rgb(0, 0, 1, 0.5)),
       pch = 19, xlim = c(0, range), ylim = c(0, range),
       xlab = "x", ylab = "y", main = "Two Groups from Poisson Distribution")
  
  # Mark lambda centers
  points(lambda1[1], lambda1[2], col = "orange", pch = 4, lwd = 2, cex = 2)
  points(lambda2[1], lambda2[2], col = "darkblue", pch = 4, lwd = 2, cex = 2)
  
  legend("topright", legend = c("Group 1", "Group 2"), col = c("orange", "blue"), pch = 19)
  
  return(all_points)
}

# Example usage
poisson_groups <- generate_poisson_data(n = 100, lambda1 = c(5, 5), lambda2 = c(15, 15))