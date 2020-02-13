#Gráficos da dissertação


##Histogram##
Size <- data.frame(log_valor_mov)
ggplot(Size, aes(x = Size))+ 
    geom_histogram(binwidth = 8/50)+
    labs(x="Size", y="Count")+
    theme(legend.position="none",
    axis.title=element_text(size = 17),
    axis.text=element_text(size = 12))
##

##Condicional Probability##
##nonparametric##
dadosprob1 <- data.frame(log_valor_mov,Pr_np)
ggplot(dadosprob1,aes(x=log_valor_mov,y=Pr_np))+
    geom_line()+
    labs(x="Size", y="Conditional Probability")+
    theme(legend.position="none",
          axis.title=element_text(size = 17),
          axis.text=element_text(size = 12))
##Parametric##
dadosprob2 <- data.frame(log_valor_mov,Pr_par)
ggplot(dadosprob2,aes(x=log_valor_mov,y=Pr_par))+
    geom_line()+
    labs(x="Size", y="Conditional Probability")+
    theme(legend.position="none",
          axis.title=element_text(size = 17),
          axis.text=element_text(size = 12))
##

##Rejection Curve##
dadoscertos1 <- data.frame(Size, Cr_np)
##dadoscertos1 <- arrange(dadoscertos1,Size)
dadoscertos2 <- data.frame(Size, Cr_par, Cr_des = (Cr_par-0.25))
##dadoscertos2 <- arrange(dadoscertos2,Size)
plot(dadoscertos1)
##Parametric
ggplot(dadoscertos1,aes(x=Size,y=Cr_par))+
    geom_line()+
    geom_point(aes(x = log(alvo_sub$VL_MOVIMENTACAO),
                   y=alvo_sub$NU_SCORE_UNIF_QTDE),
               col="red", 
               alpha = 0.3)+
    theme(legend.position="none",
          axis.title=element_text(size = 17),
          axis.text=element_text(size = 12))+
    labs(x="Size", y="Score")
##Nonparametric
ggplot(dadoscertos1,aes(x=Size,y=Cr_np))+
    geom_line()+
    geom_point(aes(x = log(alvo_sub$VL_MOVIMENTACAO),
                   y=alvo_sub$NU_SCORE_UNIF_QTDE),
                    col="seagreen", 
                    alpha = 0.3)+
    theme(legend.position="none",
          axis.title=element_text(size = 17),
          axis.text=element_text(size = 12))+
    labs(x="Size", y="Score")
    
##
ggplot(dadoscertos2,aes(x=Size,y=Cr_par))+geom_line()+geom_point(aes(x = log(alvo_sub$VL_MOVIMENTACAO),y=alvo_sub$NU_SCORE_UNIF_QTDE,col="blue", alpha = 0.3))+theme(legend.position="none")
ggplot(dadoscertos1,aes(x=Size, y=Cr_np))+geom_line()+geom_line(data=dadoscertos2,aes(x=Size,y=Cr_par))
##
##pdf(file = "grafico10.pdf", width=10,height=10)
ggplot(dadoscertos1,aes(x=Size,y=Cr_np))+
    geom_line(size=1.0)+
    geom_line(aes(x=Size,y=Cr_par1),linetype="dotted",size=1.5)+
    geom_line(aes(x=Size,y=Cr_par2),linetype="dashed",size=1.5)+
    geom_point(aes(x = log(alvo_sub$VL_MOVIMENTACAO),y=alvo_sub$NU_SCORE_UNIF_QTDE),
                   colour="blue", alpha = 0.3) +
    theme(legend.position="none",
          axis.title=element_text(size = 17, vjust = -0.2),
          axis.text=element_text(size = 12))+
    labs(x="Size", y="Score")
##dev.off()
##Método dos momentos

y = function(B){1+7.37*B+((55.79*B^2)/2)+((432.73*B^3)/6)+((3438.88*B^4)/24)+((27975.82*B^5)/120)+((232741.4*B^6)/720)}
plot(y,-0.2,0)
w = function(B){exp(7.29*B)}
plot(x,-0.2,0)

ggplot(data.frame(x = c(-0.2, -0.1)), aes(x)) +
    stat_function(fun = y, geom = "line", aes(colour = "Moments")) +
    stat_function(fun = w, geom = "line", aes(colour = "Exponetial")) +
    scale_color_manual("Functions", values=c("red","blue")) +
    labs(x = "B", y = "")

## methodology graphic

n_points <- 5000
sizegraph  <- rnorm(n = n_points, mean = 7, sd = 2)
scoregraph <- runif(n = n_points, min = 0, max = 1)
dadoscerto <- as.data.table(sizegraph,Cr_par)

ggplot(dadoscerto,aes(x=sizegraph,y=Cr_par))+
    geom_line()+
    geom_point(aes(x = sizegraph,
                   y=scoregraph),
               col="darkgrey", 
               alpha = 0.3)+
    theme(legend.position="none",
          axis.title=element_text(size = 17),
          axis.text=element_text(size = 12))+
    labs(x="Size", y="Score")


##Methodology graph 2
Size <- data.frame(log_valor_mov)

dadoscertos2 <- data.frame(Size, Cr_par, Cr_des = (Cr_par-0.25))
linha1 <- subset(dadoscertos2, Size > 7.8 & Size < 8.1)[1,]
linha2 <- melt(linha1, id = 1)
linha2[1,3] = linha2[1,3] - 0.025
linha2[2,3] = linha2[2,3] + 0.05

ggplot(dadoscertos2,aes(x=Size,y=Cr_par))+
    geom_line()+
    geom_line(aes(x = Size, y = dadoscertos2$Cr_des ))+
    geom_point(aes(x = Size,
                   y=alvo_sub$NU_SCORE_UNIF_QTDE),
               col="darkgrey", 
               alpha = 0.3)+
    geom_point(data = linha1,aes(x = linha1$log_valor_mov,
                   y=linha1$Cr_par),
               col="black", 
               alpha = 1)+
    theme(legend.position="none",
          axis.title=element_text(size = 17),
          axis.text=element_text(size = 12))+
    labs(x="Size", y="Score")+
    geom_line(aes(x = log_valor_mov, y = value), 
                  data = linha2, arrow = arrow(length = unit(0.05, "npc"), type = "closed", ends = "first"))

