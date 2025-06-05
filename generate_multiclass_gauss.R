generate_multiclass_gaussian_data <- function(n = 200, centers = list(c(-5, -5), c(5, 5)), range = 10, sd = 1) {
  set.seed(42)
  
  all_points <- data.frame()
  
  for (i in seq_along(centers)) {
    center <- centers[[i]]
    
    x <- rnorm(n, mean = center[1],sd = sd)
    y <- rnorm(n, mean = center[2],sd = sd)
    group_data <- data.frame(x = x, y = y, group = as.factor(i))
    
    all_points <- rbind(all_points, group_data)
  }
  
  # Plot
  colors <- rainbow(length(centers), alpha = 0.5)
  plot(all_points$x, all_points$y,
       col = colors[as.numeric(all_points$group)],
       pch = 19, xlim = c(-range, range), ylim = c(-range, range),
       xlab = "x", ylab = "y", main = "Multiple Cauchy Groups")
  
  # Mark centers
  for (i in seq_along(centers)) {
    center <- centers[[i]]
    points(center[1], center[2], col = colors[i], pch = 4, lwd = 2, cex = 2)
  }
  
  legend("topright", legend = paste("Group", seq_along(centers)), 
         col = colors, pch = 19)
  
  return(all_points)
}

