##---Packages---##
library("data.table")
library("ggplot2")
library("fitdistrplus")
library("plyr")
library("moments")
library("rootSolve")
##

##---Reading Data---##
data_path <- "C:/Users/Bruno/Documents/Dissertacao/Data/municipios.csv"
dados <- fread(data_path, sep = ",", encoding = 'UTF-8', na.string=c("#DIV/0!"))
dados <- na.omit(dados)
dados <- dados[Score <= 2]
##-----##

##---Data Analysis---##
plot(x = log(dados$PIB), y = dados$Score)

Size <- data.frame(log(dados$PIB))
ggplot(Size, aes(x = Size))+ 
  geom_histogram(binwidth = 8/50)+
  labs(x="Size", y="Count")+
  theme(legend.position="none",
        axis.title=element_text(size = 17),
        axis.text=element_text(size = 12))
##----###

##-- Parameters definitions --##
n_momentos <- 6
dist_nome <- "norm"
##---##

##-- Rejection Curve --##
log_PIB <- log(dados$PIB)
  
##-- Nao parametrico --##
momentos     <- all.moments(log_PIB, order.max = n_momentos, na.rm = TRUE)[-1]
med_PIB <- median(log_PIB)
B_np <- uniroot.all(function(x)model1(p.y=x,momentos=momentos,med_PIB = med_PIB),c(-1,-.00099))
B_np
##-----##
##-- Parametrico --##
fit.dist <- fitdist(log_PIB, dist_nome)
B_par <- 2*((med_PIB - fit.dist$estimate[1])/(fit.dist$estimate[2])^2)
##-----##
##-- Estimação de C --##

t0  <- min(log_PIB)
delta0 <- 0.01
alfa   <- 0.05

C_par <- ((delta0*exp(B_par*med_PIB) - exp(B_par*t0))*alfa)/(exp(B_par*med_PIB) - exp(B_par*t0))
C_np <- ((delta0*exp(B_np*med_PIB) - exp(B_np*t0))*alfa)/(exp(B_np*med_PIB) - exp(B_np*t0))
##-----##

##-- Estimação de A --##
A_par <- ((1 - delta0)*alfa)/(exp(B_par*med_PIB) - exp(B_par*t0))
A_np  <- ((1 - delta0)*alfa)/(exp(B_np*med_PIB) - exp(B_np*t0))
##-----##

##-- Função Pr --##
Pr_par <- A_par*exp(B_par*log_PIB) + C_par
Pr_np  <- A_np*exp(B_np*log_PIB) + C_np
##-----##
##Condicional Probability##
##nonparametric##
Size1 <- data.frame(log_PIB)
dadosprob1 <- data.frame(log_PIB,Pr_par)
dadosprob2 <- data.frame(log_PIB,Pr_np)
ggplot(dadosprob1,aes(x=log_PIB,y=Pr_par))+
  geom_line(color = 'darkblue')+
  geom_line(data=dadosprob2,aes(x=Size,y=Pr_np), color = 'green')+
  labs(x="Size", y="Conditional Probability")+
  theme(legend.position="none",
        axis.title=element_text(size = 17),
        axis.text=element_text(size = 12))

##-- Curva de rejeição --##
Score <- sort(dados$Score)
Teta_par <- 1-Pr_par
Teta_np  <- 1-Pr_np
Cr_par <- Score[round(Teta_par*length(Score))]
Cr_np  <- Score[round(Teta_np*length(Score))]
##-----##

##-- Plot --##

df_par <- data.frame(log_PIB, Cr_par)
df_par <- arrange(df_par, log_PIB)
dados <- arrange(dados, dados$PIB)

cr_par_df <- cbind(dados, df_par)
anomalos_par <- subset(cr_par_df, dados$Score > Cr_par)

df_np <- data.frame(log_PIB, Cr_np)
df_np <- arrange(df_np, log_PIB)
dados <- arrange(dados, dados$PIB)

cr_np_df <- cbind(dados, df_np)
anomalos_np <- subset(cr_np_df, dados$Score > Cr_np)

anomalos <- list(anomalos_par = anomalos_par, anomalos_np = anomalos_np)
lista_anomalos[[cod_alvo]] <- anomalos

##---Plot---##
dadoscertos2 <- data.frame(Size1, Cr_np)
ggplot(dadoscertos1,aes(x=Size1,y=Cr_np))+
  geom_line()+
  geom_point(aes(x = log(dados$PIB),
                 y=dados$Score),
             col="seagreen", 
             alpha = 0.3)+
  theme(legend.position="none",
        axis.title=element_text(size = 17),
        axis.text=element_text(size = 12))+
  labs(x="Size", y="Score")

