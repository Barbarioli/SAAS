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
algoritmo <- 8
tipo_score <- 2

lista_anomalos <- vector(mode = 'list', nalvos)
# for(cod_alvo in 1:nalvos) {
alvo_sub      <- dados[CO_SEQ_ALVO == cod_alvo & CO_SEQ_ALGORITMO == algoritmo & CO_SEQ_TIPO_SCORE == tipo_score & VL_MOVIMENTACAO > 0]
valor_mov     <- alvo_sub$VL_MOVIMENTACAO
log_valor_mov2 <- log(valor_mov + 1)
##-- Nao parametrico --##
momentos2     <- all.moments(log_valor_mov2, order.max = n_momentos, na.rm = TRUE)[-1]
med_logvalor2 <- median(log_valor_mov2)
B_np2 <- uniroot.all(function(x)model1(p.y=x,momentos=momentos2,med_logvalor = med_logvalor2),c(-1,-.00099))
B_np2
##-----##
##-- Parametrico --##
fit.dist2 <- fitdist(log_valor_mov2, dist_nome)
B_par2 <- 2*((med_logvalor2 - fit.dist2$estimate[1])/(fit.dist2$estimate[2])^2)
##-----##
##-- Estimação de C --##

t02  <- min(log_valor_mov2)
delta0 <- 0.01
alfa2   <- 0.025

C_par2 <- ((delta0*exp(B_par2*med_logvalor2) - exp(B_par2*t02))*alfa2)/(exp(B_par2*med_logvalor2) - exp(B_par2*t02))
C_np2 <- ((delta0*exp(B_np2*med_logvalor2) - exp(B_np2*t02))*alfa2)/(exp(B_np2*med_logvalor2) - exp(B_np2*t02))
##-----##

##-- Estimação de A --##
A_par2 <- ((1 - delta0)*alfa2)/(exp(B_par2*med_logvalor2) - exp(B_par2*t02))
A_np2  <- ((1 - delta0)*alfa2)/(exp(B_np2*med_logvalor2) - exp(B_np2*t02))
##-----##

##-- Função Pr --##
Pr_par2 <- A_par2*exp(B_par2*log_valor_mov2) + C_par2
Pr_np2  <- A_np2*exp(B_np2*log_valor_mov2) + C_np2
##-----##

##-- Curva de rejeição --##
Score <- sort(alvo_sub$NU_SCORE)
Teta_par2 <- 1-Pr_par2
Teta_np2  <- 1-Pr_np2
Cr_par2 <- Score[round(Teta_par2*length(Score))]
Cr_np2  <- Score[round(Teta_np2*length(Score))]
##-----##

df_par2 <- data.frame(log_valor_mov2, Cr_par2)
df_par2 <- arrange(df_par2, log_valor_mov2)
alvo_sub <- arrange(alvo_sub, alvo_sub$VL_MOVIMENTACAO)

cr_par_df2 <- cbind(alvo_sub, df_par2)
anomalos_par2 <- subset(cr_par_df2, alvo_sub$NU_SCORE > Cr_par2)

df_np2 <- data.frame(log_valor_mov2, Cr_np2)
df_np2 <- arrange(df_np2, log_valor_mov2)
alvo_sub <- arrange(alvo_sub, alvo_sub$VL_MOVIMENTACAO)

cr_np_df2 <- cbind(alvo_sub, df_np2)
anomalos_np2 <- subset(cr_np_df2, alvo_sub$NU_SCORE > Cr_np2)

anomalos2 <- list(anomalos_par = anomalos_par2, anomalos_np = anomalos_np2)
lista_anomalos[[cod_alvo]] <- anomalos2

cr_np_df2[, diferenca := NU_SCORE - Cr_np2]

# }
length(anomalos_par2$CO_CNES)
length(anomalos_np2$CO_CNES)
length(alvo_sub$CO_CNES)

##FDR sem armazenar as curvas##
cr_np_df2 <- subset(cr_np_df2,diferenca >= 0)

cr_np_df2[, Curva_deslogada := 0 ]
cr_np_df2[, p_valor := 0 ]
for (j in 1:nrow(cr_np_df2)) {
    for (i in 1:nrow(cr_np_df2)) {
        cr_np_df2[i,18] <- cr_np_df2[i,16,with=F] + cr_np_df2[j,17,with=F]
    }
    cr_np_df2[j,19] <- (sum(cr_np_df2$NU_SCORE > cr_np_df2[,18, with = F]))/(25461)
}

cr_np_df2 <- setorder(cr_np_df2, p_valor)

