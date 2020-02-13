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
algoritmo <- 5
tipo_score <- 2

lista_anomalos <- vector(mode = 'list', nalvos)
# for(cod_alvo in 1:nalvos) {
alvo_sub      <- dados[CO_SEQ_ALVO == cod_alvo & CO_SEQ_ALGORITMO == algoritmo & CO_SEQ_TIPO_SCORE == tipo_score & VL_MOVIMENTACAO > 0]
head(alvo_sub)
valor_mov     <- alvo_sub$VL_MOVIMENTACAO
log_valor_mov5 <- log(valor_mov + 1)
##-- Nao parametrico --##
momentos5     <- all.moments(log_valor_mov5, order.max = n_momentos, na.rm = TRUE)[-1]
med_logvalor5 <- median(log_valor_mov5)
B_np5 <- uniroot.all(function(x)model1(p.y=x,momentos=momentos5,med_logvalor = med_logvalor5),c(-1,-.00099))
B_np5
##-----##
##-- Parametrico --##
fit.dist5 <- fitdist(log_valor_mov5, dist_nome)
B_par5 <- 2*((med_logvalor5 - fit.dist5$estimate[1])/(fit.dist5$estimate[2])^2)
##-----##
##-- Estimação de C --##

t05  <- min(log_valor_mov5)
delta0 <- 0.01
alfa5   <- 0.0075

C_par5 <- ((delta0*exp(B_par5*med_logvalor5) - exp(B_par5*t05))*alfa5)/(exp(B_par5*med_logvalor5) - exp(B_par5*t05))
C_np5 <- ((delta0*exp(B_np5*med_logvalor5) - exp(B_np5*t05))*alfa5)/(exp(B_np5*med_logvalor5) - exp(B_np5*t05))
##-----##

##-- Estimação de A --##
A_par5 <- ((1 - delta0)*alfa5)/(exp(B_par5*med_logvalor5) - exp(B_par5*t05))
A_np5  <- ((1 - delta0)*alfa5)/(exp(B_np5*med_logvalor5) - exp(B_np5*t05))
##-----##

##-- Função Pr --##
Pr_par5 <- A_par5*exp(B_par5*log_valor_mov5) + C_par5
Pr_np5  <- A_np5*exp(B_np5*log_valor_mov5) + C_np5
##-----##

##-- Curva de rejeição --##
Score <- sort(alvo_sub$NU_SCORE)
Teta_par5 <- 1-Pr_par5
Teta_np5  <- 1-Pr_np5
Cr_par5 <- Score[round(Teta_par5*length(Score))]
Cr_np5  <- Score[round(Teta_np5*length(Score))]
##-----##

df_par5 <- data.frame(log_valor_mov5, Cr_par5)
df_par5 <- arrange(df_par5, log_valor_mov5)
alvo_sub <- arrange(alvo_sub, alvo_sub$VL_MOVIMENTACAO)

cr_par_df5 <- cbind(alvo_sub, df_par5)
anomalos_par5 <- subset(cr_par_df5, alvo_sub$NU_SCORE > Cr_par5)

df_np5 <- data.frame(log_valor_mov5, Cr_np5)
df_np5 <- arrange(df_np5, log_valor_mov5)
alvo_sub <- arrange(alvo_sub, alvo_sub$VL_MOVIMENTACAO)

cr_np_df5 <- cbind(alvo_sub, df_np5)
anomalos_np5 <- subset(cr_np_df5, alvo_sub$NU_SCORE > Cr_np5)

anomalos5 <- list(anomalos_par = anomalos_par5, anomalos_np = anomalos_np5)
lista_anomalos[[cod_alvo]] <- anomalos5

cr_np_df5[, diferenca := NU_SCORE - Cr_np5]

# }
length(anomalos_par5$CO_CNES)
length(anomalos_np5$CO_CNES)
length(alvo_sub$CO_CNES)

##FDR sem armazenar as curvas##
cr_np_df5 <- subset(cr_np_df5,diferenca >= 0)

cr_np_df5[, Curva_deslogada := 0 ]
cr_np_df5[, p_valor := 0 ]
for (j in 1:nrow(cr_np_df5)) {
    for (i in 1:nrow(cr_np_df5)) {
        cr_np_df5[i,18] <- cr_np_df5[i,16,with=F] + cr_np_df5[j,17,with=F]
    }
    cr_np_df5[j,19] <- (sum(cr_np_df5$NU_SCORE > cr_np_df5[,18, with = F]))/(25461)
}

cr_np_df5 <- setorder(cr_np_df5, p_valor)


