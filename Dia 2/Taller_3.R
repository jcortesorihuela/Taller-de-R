
#setwd("C:/Users/juadi/OneDrive/Escritorio/Diploma_Pronostico_Demanda")    
data_ventas <- read.csv(file="data_clase_reg_multiple.csv", header=TRUE, sep=",")
head(data_ventas)
summary(data_ventas)

data_ventas$Mall3=0
data_ventas$Mall3[data_ventas$Mall==1]=1

lm1 <- lm(Ventas ~ 1+Publicidad+Ingreso+mt2+Mall2,data=data_ventas)

summary(lm1)

lm2 <- lm(Ventas ~ 1+Ingreso+mt2+Mall2,data=data_ventas)

summary(lm2)


predict(lm1,newdata=data.frame(Publicidad=400,Ingreso=500,mt2=22,Mall2=1))

round(cor(data_ventas[c('Publicidad','Ingreso','mt2','Mall2')]),2)

lm2 <- lm(Ventas ~ 1,data=data_ventas)
summary(lm2)
mean(data_ventas$Ventas)

anova(lm2,lm1)


#install.packages("L1pack")
library(L1pack)

lm1_lad=lad(Ventas ~ Publicidad+Ingreso+mt2+Mall2, data=data_ventas)
summary(lm1_lad)

