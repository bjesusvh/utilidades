#' Plot an object of class MPS
#'
#' Función que gráfica las salidas de la función ApproxMPS.
#'
#' @param MPSObject Un objeto de clase MPS (ApproxMPS).
#' @param Sim vector de similaridades promedio para cada línea candidata.
#' @param qLoss Cuantil para la pérdida.
#' @param qSimI Cuantil inferior para la similaridad.
#' @param qSimS Cuantil superior para la similaridad.
#' @param B0 (vector) with the overall mean of each trait of length equal to number of traits (\eqn{t}).
#' @return Un gráfico de la salida de MPS.
#' @export
plotMPSFromG <- function(MPSObject, Sim, qLoss, qSimI, B0){
# plotMPSFromG <- function(MPSObject, Sim, qLoss, qSimI, qSimS){
  
  df <- data.frame(loss = MPSObject$loss, 
                   ranking = MPSObject$ranking, 
                   aveSim = abs(Sim))
  
  D <- quantile(df$aveSim, probs = c(qSimI)) #D <- quantile(df$aveSim, probs = c(qSimI, qSimS))
  L <- quantile(df$loss, probs = qLoss)
  best <- df$aveSim <= D & df$loss <= L
  # best <- (df$aveSim >= D[1] & df$aveSim <= D[2]) & df$loss <= L
    
  NSelected <- sum(best)
  prom_best <- colMeans(MPSObject$yHat[best, ])
  prom_loss <- colMeans(MPSObject$yHat[MPSObject$ranking %in% 1:NSelected, ])
  
  porc_best <- (prom_best - B0)/B0*100
  porc_loss <- (prom_loss - B0)/B0*100
  
  m <- matrix(c(porc_best, porc_loss), nrow = 2, byrow = TRUE)
  colnames(m) <- paste("Trait", 1:ncol(m))
  rownames(m) <- c("Both", "MinLoss")
  
  
  par(mfrow = c(2,1))
  # # Histogram of eloss
  # hist(MPSObject$loss, breaks = 20,
  #      main = "(a)",
  #      xlab = "Expected loss")
  # abline(v = L, col = "red", lty = 2)
  # 
  # # Histogram of average distances
  # hist(abs(Sim), breaks = 20,
  #      main = "(b)",
  #      xlab = "Average relationship")
  # abline(v = D, col = "red", lty = 2)
  
  
  # Plot selected
  plot(abs(Sim), MPSObject$loss, axes = FALSE, 
       ylab = "Expected loss",
       xlab = "Average relationship",
       col = ifelse(best, "black", "gray"),
       pch = 19, 
       main = "(a)")
  axis(1)
  axis(2)
  
  text(abs(Sim), MPSObject$loss,
       labels = (1:length(Sim)),
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

  return(selected = best)
}
