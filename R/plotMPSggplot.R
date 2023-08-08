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
plotMPSggplot <- function(MPSObject, Sim, qLoss, qSimilarity){
  
  df <- data.frame(loss = MPSObject$loss, 
                   ranking = MPSObject$ranking, 
                   aveSim = Sim)
  
  D <- quantile(df$aveSim, probs = qSimilarity)
  L <- quantile(df$loss, probs = qLoss)
  best <- df$aveSim <= D & df$loss <= L
  
  etiquetas <- 1:length(Sim)
  etiquetas[!best] <- NA
  
  p1 <- ggplot(df, aes(x = aveSim, y = loss)) +
      geom_point(size = 3, col = ifelse(best, "black", "gray")) +
      geom_vline(xintercept = D, lty = 2, col = "#de2d26") + 
      geom_hline(yintercept = L, lty = 2, col = "#de2d26") +
      geom_text_repel(aes(label = etiquetas),
                      size = 3.5) +
      labs(x = "Average similarity", y = "Expected loss",
           title = "")

  
  # colores <- c("#43a2ca", "#7bccc4", "#a8ddb5")
  # 
  # p2 <- ggplot(m, aes(x=Trait, y = mean, fill = scenario)) +
  #     geom_bar(stat="identity", position=position_dodge()) +
  #     scale_fill_manual(values=colores[1:cols]) +
  #     labs(y = "Average BVs", x = "",
  #          title = "(b)")
  
  multiplot(p1)

  return(best)
}
