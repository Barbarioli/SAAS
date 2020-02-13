#' Funcao dos momentos
#' @description Essa funcao utilizada para estimacao nao parametrica
#' @param p.y variavel a ser esrimada
#' @param p.m vetor de momentos
#' @param p.t mediana da medida de tamanho = valor


model1 <- function(p.y, momentos, med_logvalor){
    aux <- rep(0,length(p.y))
    expoente <- 1:length(momentos)
    denominador <- factorial(expoente)
    i = 1
    for(X in p.y){
        aux[i] <- 1 + sum((momentos*X^expoente)/denominador) - exp(X*med_logvalor) - 0.5*((exp(X*med_logvalor)-exp(X*t0))/(0.5-delta0))
        i = i + 1
    }
    return(aux)
}
