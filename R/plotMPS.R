#' Plot an object of class MPS
#'
#' Función que gráfica las salidas de la función MPS.
#'
#' @param MPSObject Un objeto de clase MPS (FasMPS o EvalMPS).
#' @param Dist vector de distancias promedio de cada línea candidata.
#' @param qLoss Cuantil para la pérdida.
#' @param qDistance Cuantil para la distancia.
#' @param B0 (vector) with the overall mean of each trait of length equal to number of traits (\eqn{t}).
#' @return Un gráfico de la salida de MPS.
#' @export
plotMPS <- function(MPSObject, Dist, qLoss, qDistance, B0){
  
  df <- data.frame(line = 1:length(Dist),
                   loss = MPSObject$loss, 
                   ranking = MPSObject$ranking, 
                   aveDist = Dist)
  
  D <- quantile(df$aveDist, probs = qDistance)
  L <- quantile(df$loss, probs = qLoss)
  best <- df$aveDist >= D & df$loss <= L
  
  NSelected <- sum(best)
  prom_best <- colMeans(MPSObject$yHat[best, ])
  prom_loss <- colMeans(MPSObject$yHat[MPSObject$ranking %in% 1:NSelected, ])
  
  
  porc_best <- (prom_best - B0)/B0*100
  porc_loss <- (prom_loss - B0)/B0*100
  
  m <- matrix(c(prom_best, prom_loss), nrow = 2, byrow = TRUE)
  colnames(m) <- paste("Trait", 1:ncol(m))
  rownames(m) <- c("Both", "MinLoss")
  
  
  par(mfrow = c(2,1))
  # 
  # df2 <- df[order(df$loss,decreasing = FALSE),]   
  # 
  # barplot(df2$loss, names.arg = df2$line, 
  #         horiz = TRUE, las = 1, cex.names=.2)
  
  # # Histogram of eloss
  # hist(MPSObject$loss, breaks = 30,
  #      main = "(a)",
  #      xlab = "Posterior expected loss")
  # abline(v = L, col = "red", lty = 2)
  
  # # Histogram of average distances
  # hist(Dist, breaks = 30,
  #      main = "(b)",
  #      xlab = "Average distances")
  # abline(v = D, col = "red", lty = 2)
  
  
  # Plot selected
  plot(Dist, MPSObject$loss, axes = FALSE, 
       ylab = "Expected loss",
       xlab = "Average distance",
       col = ifelse(best, "black", "gray"),
       pch = 19, 
       main = "(a)")
  axis(1)
  axis(2)
  
  text(Dist, MPSObject$loss,
       labels = (1:length(Dist)),
       col = ifelse(best, "black", "gray"),
       cex = 0.6, pos = 3)
  
  abline(h = L, lty = 2, col = "red")
  abline(v = D, lty = 2, col = "red")
  
  # Barplot
  barplot(m, 
          main = "(b)", 
          xlab = "", 
          ylab = "Percentage",
          beside = TRUE,
          legend = rownames(m))
  
  # barplot(m, 
  #         main = "(d)", 
  #         xlab = "", 
  #         ylab = "Average BVs of selected",
  #         beside = TRUE,
  #         legend = rownames(m))
  
  par(mfrow = c(1,1))
}