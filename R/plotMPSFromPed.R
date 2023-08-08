#' Plot an object of class MPS
#'
#' Función que gráfica las salidas de la función ApproxMPS.
#'
#' @param MPSObject Un objeto de clase MPS (ApproxMPS).
#' @param Sim vector de similaridades promedio para cada línea candidata.
#' @param qLoss Cuantil para la pérdida.
#' @param qSimilarity Cuantil para la similaridad.
#' @param B0 (vector) with the overall mean of each trait of length equal to number of traits (\eqn{t}).
#' @param bandera Un indicador de si B0 son ceros.
#' @return Un gráfico de la salida de MPS.
#' @export
plotMPSFromPed <- function(MPSObject, Sim, qLoss, qSimilarity){
  
  df <- data.frame(loss = MPSObject$loss, 
                   ranking = MPSObject$ranking, 
                   aveSim = Sim)
  
  D <- quantile(df$aveSim, probs = qSimilarity)
  L <- quantile(df$loss, probs = qLoss)
  best <- df$aveSim <= D & df$loss <= L
  
  NSelected <- sum(best)
  prom_best <- colMeans(MPSObject$yHat[best, ])
  prom_loss <- colMeans(MPSObject$yHat[MPSObject$ranking %in% 1:NSelected, ])

  m <- matrix(c(prom_best, prom_loss), nrow = 2, byrow = TRUE)
  colnames(m) <- paste("Trait", 1:ncol(m))
  rownames(m) <- c("Both", "Min Loss")

  
  #par(mfrow = c(2,2))
  # Histogram of eloss
  # hist(MPSObject$loss, breaks = 30,
  #      main = "(a)",
  #      xlab = "Posterior expected loss")
  # abline(v = L, col = "red", lty = 2)

  # Histogram of average distances
  # hist(Sim, breaks = 30,
  #      main = "(b)",
  #      xlab = "Average similarity")
  # abline(v = D, col = "red", lty = 2)


  # Plot selected
  plot(Sim, MPSObject$loss, axes = FALSE, 
       ylab = "Expected loss",
       xlab = "Average similarity",
       col = ifelse(best, "black", "gray"),
       pch = 19, 
       main = "")
  axis(1)
  axis(2)
  
  text(Sim, MPSObject$loss,
       labels = (1:length(Sim)),
       col = ifelse(best, "black", "gray"),
       cex = 0.6, pos = 3)
  
  abline(h = L, lty = 2, col = "red")
  abline(v = D, lty = 2, col = "red")
  
  
  # barplot(m, 
  #         main = "(d)", 
  #         xlab = "", 
  #         ylab = "Average BVs of selected",
  #         beside = TRUE,
  #         legend = rownames(m))
  
  #par(mfrow = c(1,1))
  
  return(best)
}
