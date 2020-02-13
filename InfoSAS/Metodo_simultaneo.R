Completo <- rbind(cr_np_df1[,c(3,19), with = F],cr_np_df2[,c(3,19), with = F],cr_np_df3[,c(3,19), with = F],cr_np_df4[,c(3,19), with = F],cr_np_df5[,c(3,19), with = F])

Completo <- setorder(Completo, p_valor)

##lambda = ??##

q = 0.05
m = nrow(Completo)
lambda = 0.00675
m0 = (m + 1 - sum(Completo$p_valor <= lambda))/(1-lambda)
Completo[, lambda := 0 ]

for(j in 1:nrow(Completo)){
    Completo[j,3] <- (j/m0)*q
}

sum(Completo$p_valor < Completo$lambda)

Completo1 <- Completo[p_valor < lambda]
length(unique(Completo1$CO_CNES))
