#' Funcao dos momentos
#' @description Essa funcao utilizada para estimacao nao parametrica
#' @param p.y variavel a ser esrimada
#' @param p.m vetor de momentos
#' @param p.t mediana da medida de tamanho = valor


model1 <- function(p.y, momentos, med_PIB){
  aux <- rep(0,length(p.y))
  expoente <- 1:length(momentos)
  denominador <- factorial(expoente)
  i = 1
  for(X in p.y){
    aux[i] <- 1 + sum((momentos*X^expoente)/denominador) - exp(X*med_PIB)
    i = i + 1
  }
  return(aux)
}

