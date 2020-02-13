library(data.table)
library(fitdistrplus)
library(plyr)
library(moments)
library(rootSolve)
library(ggplot2)
##-----##

##-- Lendo dados --##
data_path <- "C:/Users/Bruno Barbarioli/Documents/Dissertação/Infosas/naounificado.csv"
dados <- fread(data_path, sep = ";", encoding = 'UTF-8')
##-----##

cod_alvo   <- 21
n_momentos <- 6
dist_nome <- "norm"
nalvos <- 1
algoritmo <- 4
tipo_score <- 2

lista_anomalos <- vector(mode = 'list', nalvos)
# for(cod_alvo in 1:nalvos) {
alvo_sub      <- dados[CO_SEQ_ALVO == cod_alvo & CO_SEQ_ALGORITMO == algoritmo & CO_SEQ_TIPO_SCORE == tipo_score & VL_MOVIMENTACAO > 0]
head(alvo_sub)
valor_mov     <- alvo_sub$VL_MOVIMENTACAO
log_valor_mov3 <- log(valor_mov + 1)
##-- Nao parametrico --##
momentos3     <- all.moments(log_valor_mov3, order.max = n_momentos, na.rm = TRUE)[-1]
med_logvalor3 <- median(log_valor_mov3)
B_np3 <- uniroot.all(function(x)model1(p.y=x,momentos=momentos3,med_logvalor = med_logvalor3),c(-1,-.00099))
B_np3
##-----##
##-- Parametrico --##
fit.dist3 <- fitdist(log_valor_mov3, dist_nome)
B_par3 <- 2*((med_logvalor3 - fit.dist3$estimate[1])/(fit.dist3$estimate[2])^2)
##-----##
##-- Estimação de C --##

t03  <- min(log_valor_mov3)
delta0 <- 0.01
alfa3   <- 0.005

C_par3 <- ((delta0*exp(B_par3*med_logvalor3) - exp(B_par3*t03))*alfa3)/(exp(B_par3*med_logvalor3) - exp(B_par3*t03))
C_np3 <- ((delta0*exp(B_np3*med_logvalor3) - exp(B_np3*t03))*alfa3)/(exp(B_np3*med_logvalor3) - exp(B_np3*t03))
##-----##

##-- Estimação de A --##
A_par3 <- ((1 - delta0)*alfa3)/(exp(B_par3*med_logvalor3) - exp(B_par3*t03))
A_np3  <- ((1 - delta0)*alfa3)/(exp(B_np3*med_logvalor3) - exp(B_np3*t03))
##-----##

##-- Função Pr --##
Pr_par3 <- A_par3*exp(B_par3*log_valor_mov3) + C_par3
Pr_np3  <- A_np3*exp(B_np3*log_valor_mov3) + C_np3
##-----##

##-- Curva de rejeição --##
Score <- sort(alvo_sub$NU_SCORE)
Teta_par3 <- 1-Pr_par3
Teta_np3  <- 1-Pr_np3
Cr_par3 <- Score[round(Teta_par3*length(Score))]
Cr_np3  <- Score[round(Teta_np3*length(Score))]
##-----##

df_par3 <- data.frame(log_valor_mov3, Cr_par3)
df_par3 <- arrange(df_par3, log_valor_mov3)
alvo_sub <- arrange(alvo_sub, alvo_sub$VL_MOVIMENTACAO)

cr_par_df3 <- cbind(alvo_sub, df_par3)
anomalos_par3 <- subset(cr_par_df3, alvo_sub$NU_SCORE > Cr_par3)

df_np3 <- data.frame(log_valor_mov3, Cr_np3)
df_np3 <- arrange(df_np3, log_valor_mov3)
alvo_sub <- arrange(alvo_sub, alvo_sub$VL_MOVIMENTACAO)

cr_np_df3 <- cbind(alvo_sub, df_np3)
anomalos_np3 <- subset(cr_np_df3, alvo_sub$NU_SCORE > Cr_np3)

anomalos3 <- list(anomalos_par = anomalos_par3, anomalos_np = anomalos_np3)
lista_anomalos[[cod_alvo]] <- anomalos3

cr_np_df3[, diferenca := NU_SCORE - Cr_np3]
# }
length(anomalos_par3$CO_CNES)
length(anomalos_np3$CO_CNES)
length(alvo_sub$CO_CNES)

##FDR sem armazenar as curvas##
cr_np_df3 <- subset(cr_np_df3,diferenca >= 0)

cr_np_df3[, Curva_deslogada := 0 ]
cr_np_df3[, p_valor := 0 ]
for (j in 1:nrow(cr_np_df3)) {
    for (i in 1:nrow(cr_np_df3)) {
        cr_np_df3[i,18] <- cr_np_df3[i,16,with=F] + cr_np_df3[j,17,with=F]
    }
    cr_np_df3[j,19] <- (sum(cr_np_df3$NU_SCORE > cr_np_df3[,18, with = F]))/(25461)
}

cr_np_df3 <- setorder(cr_np_df3, p_valor)

