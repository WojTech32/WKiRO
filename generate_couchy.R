generate_cauchy_data <- function(n = 200, center1 = c(-5, -5), center2 = c(5, 5), range = 10) {
  set.seed(42)
  
  # Group 1
  x1 <- rcauchy(n, location = center1[1], scale = 1)
  y1 <- rcauchy(n, location = center1[2], scale = 1)
  group1 <- data.frame(x = x1, y = y1, group = 1)
  
  # Group 2
  x2 <- rcauchy(n, location = center2[1], scale = 1)
  y2 <- rcauchy(n, location = center2[2], scale = 1)
  group2 <- data.frame(x = x2, y = y2, group = 2)
  
  # Combine into one data frame
  all_points <- rbind(group1, group2)
  
  # Plotting
  plot(all_points$x, all_points$y, 
       col = ifelse(all_points$group == 1, rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)),
       pch = 19, xlim = c(-range, range), ylim = c(-range, range),
       xlab = "x", ylab = "y", main = "Two Groups from Cauchy Distribution")
  
  # Mark centers
  points(center1[1], center1[2], col = "darkred", pch = 4, lwd = 2, cex = 2)
  points(center2[1], center2[2], col = "darkblue", pch = 4, lwd = 2, cex = 2)
  
  legend("topright", legend = c("Group 1", "Group 2"), col = c("red", "blue"), pch = 19)
  
  return(all_points)
}

# Example usage
groups <- generate_cauchy_data(n = 100, center1 = c(-3, -3), center2 = c(3, 3))