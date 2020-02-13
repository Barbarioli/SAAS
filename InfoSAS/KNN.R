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
algoritmo <- 2
tipo_score <- 2

lista_anomalos <- vector(mode = 'list', nalvos)
# for(cod_alvo in 1:nalvos) {
alvo_sub      <- dados[CO_SEQ_ALVO == cod_alvo & CO_SEQ_ALGORITMO == algoritmo & CO_SEQ_TIPO_SCORE == tipo_score & VL_MOVIMENTACAO > 0]
valor_mov     <- alvo_sub$VL_MOVIMENTACAO
log_valor_mov4 <- log(valor_mov + 1)
##-- Nao parametrico --##
momentos     <- all.moments(log_valor_mov4, order.max = n_momentos, na.rm = TRUE)[-1]
med_logvalor4 <- median(log_valor_mov4)
B_np4 <- uniroot.all(function(x)model1(p.y=x,momentos=momentos,med_logvalor = med_logvalor4),c(-1,-.00099))
B_np4
##-----##
##-- Parametrico --##
fit.dist <- fitdist(log_valor_mov4, dist_nome)
B_par4 <- 2*((med_logvalor - fit.dist$estimate[1])/(fit.dist$estimate[2])^2)
##-----##
##-- Estimação de C --##

t04  <- min(log_valor_mov4)
delta0 <- 0.01
alfa4   <- 0.0075

C_par4 <- ((delta0*exp(B_par4*med_logvalor4) - exp(B_par4*t04))*alfa4)/(exp(B_par4*med_logvalor4) - exp(B_par4*t04))
C_np4 <- ((delta0*exp(B_np4*med_logvalor4) - exp(B_np4*t04))*alfa4)/(exp(B_np4*med_logvalor4) - exp(B_np4*t04))
##-----##

##-- Estimação de A --##
A_par4 <- ((1 - delta0)*alfa4)/(exp(B_par4*med_logvalor4) - exp(B_par4*t04))
A_np4  <- ((1 - delta0)*alfa4)/(exp(B_np4*med_logvalor4) - exp(B_np4*t04))
##-----##

##-- Função Pr --##
Pr_par4 <- A_par4*exp(B_par4*log_valor_mov4) + C_par4
Pr_np4  <- A_np4*exp(B_np4*log_valor_mov4) + C_np4
##-----##

##-- Curva de rejeição --##
Score <- sort(alvo_sub$NU_SCORE)
Teta_par4 <- 1-Pr_par4
Teta_np4  <- 1-Pr_np4
Cr_par4 <- Score[round(Teta_par4*length(Score))]
Cr_np4  <- Score[round(Teta_np4*length(Score))]
##-----##

df_par4 <- data.frame(log_valor_mov4, Cr_par4)
df_par4 <- arrange(df_par4, log_valor_mov4)
alvo_sub <- arrange(alvo_sub, alvo_sub$VL_MOVIMENTACAO)

cr_par_df4 <- cbind(alvo_sub, df_par4)
anomalos_par4 <- subset(cr_par_df4, alvo_sub$NU_SCORE > Cr_par4)

df_np4 <- data.frame(log_valor_mov4, Cr_np4)
df_np4 <- arrange(df_np4, log_valor_mov4)
alvo_sub <- arrange(alvo_sub, alvo_sub$VL_MOVIMENTACAO)

cr_np_df4 <- cbind(alvo_sub, df_np4)
anomalos_np4 <- subset(cr_np_df4, alvo_sub$NU_SCORE > Cr_np4)

anomalos4 <- list(anomalos_par = anomalos_par4, anomalos_np = anomalos_np4)
lista_anomalos[[cod_alvo]] <- anomalos4

cr_np_df4[, diferenca := NU_SCORE - Cr_np4]

# }
length(anomalos_par4$CO_CNES)
length(alvo_sub$CO_CNES)

##FDR sem armazenar as curvas##
cr_np_df4 <- subset(cr_np_df4,diferenca >= 0)

cr_np_df4[, Curva_deslogada := 0 ]
cr_np_df4[, p_valor := 0 ]
for (j in 1:nrow(cr_np_df4)) {
    for (i in 1:nrow(cr_np_df4)) {
        cr_np_df4[i,18] <- cr_np_df4[i,16,with=F] + cr_np_df4[j,17,with=F]
    }
    cr_np_df4[j,19] <- (sum(cr_np_df4$NU_SCORE > cr_np_df4[,18, with = F]))/(25461)
}

cr_np_df4 <- setorder(cr_np_df4, p_valor)

