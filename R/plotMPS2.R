#' Plot an object of class MPS
#'
#' Función que gráfica las salidas de la función ApproxMPS.
#'
#' @param MPSObject Un objeto de clase MPS (ApproxMPS).
#' @param qLoss Cuantil para la pérdida.
#' @param qSimilarity Cuantil para la similaridad.
#' @return Un gráfico de la salida de MPS.
#' @export
plotMPS2 <- function(MPSObject, qLoss, qSimilarity){
  
  df <- data.frame(loss = MPSObject$loss, 
                   ranking = MPSObject$ranking, 
                   aveSim = MPSObject$aveSim)
  
  D <- quantile(df$aveSim, probs = qSimilarity)
  L <- quantile(df$loss, probs = qLoss)
  best <- df$aveSim <= D & df$loss <= L
  
  NSelected <- sum(best)
  prom_best <- colMeans(MPSObject$yHat[best, ])
  prom_loss <- colMeans(MPSObject$yHat[MPSObject$ranking %in% 1:NSelected, ])
  
  m <- matrix(c(prom_best, prom_loss), nrow = 2, byrow = TRUE)
  colnames(m) <- paste("Trait", 1:ncol(m))
  rownames(m) <- c("Both", "MinLoss")
  
  
  par(mfrow = c(2,2))
  # Histogram of eloss
  hist(MPSObject$loss, breaks = 30,
       main = "(a)",
       xlab = "Posterior expected loss")
  abline(v = L, col = "red", lty = 2)
  
  # Histogram of average distances
  hist(MPSObject$aveSim, breaks = 30,
       main = "(b)",
       xlab = "Average similarity")
  abline(v = D, col = "red", lty = 2)
  
  
  # Plot selected
  plot(MPSObject$aveSim, MPSObject$loss, axes = FALSE, 
       ylab = "Expected loss",
       xlab = "Average similarity",
       col = ifelse(best, "black", "gray"),
       pch = 19, 
       main = "(c)")
  axis(1)
  axis(2)
  
  text(MPSObject$aveSim, MPSObject$loss,
       labels = (1:length(MPSObject$aveSim)),
       col = ifelse(best, "black", "gray"),
       cex = 0.6, pos = 3)
  
  abline(h = L, lty = 2, col = "red")
  abline(v = D, lty = 2, col = "red")
  
  # Barplot
  barplot(m, 
          main = "(d)", 
          xlab = "", 
          ylab = "Average BVs of selected",
          beside = TRUE,
          legend = rownames(m))
  
  par(mfrow = c(1,1))
}
