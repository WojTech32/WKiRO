generate_gaussian_data <- function(n = 200, center1 = c(-5, -5), center2 = c(5, 5), range = 10, sd = 1) {
  set.seed(42)
  
  # Group 1
  x1 <- rnorm(n, mean = center1[1], sd = sd)
  y1 <- rnorm(n, mean = center1[2], sd = sd)
  group1 <- data.frame(x = x1, y = y1, group = 1)
  
  # Group 2
  x2 <- rnorm(n, mean = center2[1], sd = sd)
  y2 <- rnorm(n, mean = center2[2], sd = sd)
  group2 <- data.frame(x = x2, y = y2, group = 2)
  
  # Combine into one data frame
  all_points <- rbind(group1, group2)
  
  # Plotting
  plot(all_points$x, all_points$y, 
       col = ifelse(all_points$group == 1, rgb(0, 0.6, 0, 0.5), rgb(0, 0, 1, 0.5)),
       pch = 19, xlim = c(-range, range), ylim = c(-range, range),
       xlab = "x", ylab = "y", main = "Two Groups from Gaussian Distribution")
  
  # Mark centers
  points(center1[1], center1[2], col = "darkgreen", pch = 4, lwd = 2, cex = 2)
  points(center2[1], center2[2], col = "darkblue", pch = 4, lwd = 2, cex = 2)
  
  legend("topright", legend = c("Group 1", "Group 2"), col = c("darkgreen", "blue"), pch = 19)
  
  return(all_points)
}

# Example usage
gaussian_groups <- generate_gaussian_data(n = 100, center1 = c(-3, -3), center2 = c(3, 3), sd = 1.5)