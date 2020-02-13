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
algoritmo <- 3
tipo_score <- 2

lista_anomalos <- vector(mode = 'list', nalvos)
# for(cod_alvo in 1:nalvos) {
alvo_sub      <- dados[CO_SEQ_ALVO == cod_alvo & CO_SEQ_ALGORITMO == algoritmo & CO_SEQ_TIPO_SCORE == tipo_score & VL_MOVIMENTACAO > 0]
head(alvo_sub)
valor_mov     <- alvo_sub$VL_MOVIMENTACAO
log_valor_mov1 <- log(valor_mov + 1)
##-- Nao parametrico --##
momentos1     <- all.moments(log_valor_mov1, order.max = n_momentos, na.rm = TRUE)[-1]
med_logvalor1 <- median(log_valor_mov1)
B_np1 <- uniroot.all(function(x)model1(p.y=x,momentos=momentos1,med_logvalor = med_logvalor1),c(-1,-.00099))
B_np1
##-----##
##-- Parametrico --##
fit.dist1 <- fitdist(log_valor_mov1, dist_nome)
B_par1 <- 2*((med_logvalor1 - fit.dist1$estimate[1])/(fit.dist1$estimate[2])^2)
##-----##
##-- Estimação de C --##

t01  <- min(log_valor_mov1)
delta0 <- 0.01
alfa1   <- 0.005

C_par1 <- ((delta0*exp(B_par1*med_logvalor1) - exp(B_par1*t01))*alfa1)/(exp(B_par1*med_logvalor1) - exp(B_par1*t01))
C_np1 <- ((delta0*exp(B_np1*med_logvalor1) - exp(B_np1*t01))*alfa1)/(exp(B_np1*med_logvalor1) - exp(B_np1*t01))
##-----##

##-- Estimação de A --##
A_par1 <- ((1 - delta0)*alfa1)/(exp(B_par1*med_logvalor1) - exp(B_par1*t01))
A_np1  <- ((1 - delta0)*alfa1)/(exp(B_np1*med_logvalor1) - exp(B_np1*t01))
##-----##

##-- Função Pr --##
Pr_par1 <- A_par1*exp(B_par1*log_valor_mov1) + C_par1
Pr_np1  <- A_np1*exp(B_np1*log_valor_mov1) + C_np1
##-----##

##-- Curva de rejeição --##
Score <- sort(alvo_sub$NU_SCORE)
Teta_par1 <- 1-Pr_par1
Teta_np1  <- 1-Pr_np1
Cr_par1 <- Score[round(Teta_par1*length(Score))]
Cr_np1  <- Score[round(Teta_np1*length(Score))]
##-----##

df_par1 <- data.frame(log_valor_mov1, Cr_par1)
df_par1 <- arrange(df_par1, log_valor_mov1)
alvo_sub <- arrange(alvo_sub, alvo_sub$VL_MOVIMENTACAO)

cr_par_df1 <- cbind(alvo_sub, df_par1)
anomalos_par1 <- subset(cr_par_df1, alvo_sub$NU_SCORE > Cr_par1)

df_np1 <- data.frame(log_valor_mov1, Cr_np1)
df_np1 <- arrange(df_np1, log_valor_mov1)
alvo_sub <- arrange(alvo_sub, alvo_sub$VL_MOVIMENTACAO)

cr_np_df1 <- cbind(alvo_sub, df_np1)
anomalos_np1 <- subset(cr_np_df1, alvo_sub$NU_SCORE > Cr_np1)

anomalos1 <- list(anomalos_par = anomalos_par1, anomalos_np = anomalos_np1)
lista_anomalos[[cod_alvo]] <- anomalos1

cr_np_df1[, diferenca := NU_SCORE - Cr_np1]

# }

length(anomalos_par1$CO_CNES)
length(anomalos_np1$CO_CNES)
length(alvo_sub$CO_CNES)


##FDR sem armazenar as curvas##
cr_np_df1 <- subset(cr_np_df1,diferenca >= 0)

cr_np_df1[, Curva_deslogada := 0 ]
cr_np_df1[, p_valor := 0 ]
for (j in 1:nrow(cr_np_df1)) {
    for (i in 1:nrow(cr_np_df1)) {
        cr_np_df1[i,18] <- cr_np_df1[i,16,with=F] + cr_np_df1[j,17,with=F]
    }
    cr_np_df1[j,19] <- (sum(cr_np_df1$NU_SCORE > cr_np_df1[,18, with = F]))/(25461)
}

cr_np_df1 <- setorder(cr_np_df1, p_valor)
