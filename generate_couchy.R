generuj_dane_cauchy <- function(n = 200, center1 = c(-5, -5), center2 = c(5, 5), zakres = 10) {
  set.seed(42)
  
  # Grupa 1
  x1 <- rcauchy(n, location=center1[1], scale=1)
  y1 <- rcauchy(n, location=center1[2], scale=1)
  grupa1 <- data.frame(x = x1, y = y1, grupa = 1)
  
  # Grupa 2
  x2 <- rcauchy(n, location=center2[1], scale=1)
  y2 <- rcauchy(n, location=center2[2], scale=1)
  grupa2 <- data.frame(x = x2, y = y2, grupa = 2)
  
  # Połączenie w jeden data frame
  wszystkie_punkty <- rbind(grupa1, grupa2)
  
  # Rysowanie
  plot(wszystkie_punkty$x, wszystkie_punkty$y, 
       col = ifelse(wszystkie_punkty$grupa == "Grupa 1", rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)),
       pch = 19, xlim = c(-zakres, zakres), ylim = c(-zakres, zakres),
       xlab = "x", ylab = "y", main = "Dwie grupy z rozkładu Cauchy’ego")
  
  # Oznaczenie środków
  points(center1[1], center1[2], col="darkred", pch=4, lwd=2, cex=2)
  points(center2[1], center2[2], col="darkblue", pch=4, lwd=2, cex=2)
  
  legend("topright", legend=c("Grupa 1", "Grupa 2"), col=c("red", "blue"), pch=19)
  
  return(wszystkie_punkty)
}

#Przykladowe wywolanie
grupa <- generuj_dane_cauchy(n=100, center1=c(-3, -3), center2=c(3, 3))