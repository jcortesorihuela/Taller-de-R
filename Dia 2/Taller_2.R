

#C?digo de regresi?n lineal simple del m?dulo pron?stico de demanda
#

#Indicar directorio
#setwd("C:/Users/juadi/OneDrive/Escritorio/Diploma_Pronostico_Demanda")    

#Importar datos
data_ventas = read.csv(file="data_clase_reg.csv", header=T, sep=",")

head(data_ventas)
dim(data_ventas)


plot(Ventas ~ Publicidad, 
     data=data_ventas, 
     pch=16, cex=0.7, ylab= "Ventas", xlab="Gasto en Publicidad")
abline(lm1 <- lm(Ventas ~ Publicidad, data=data_ventas),col="red",lty=2)
points(lm1$fitted.values~data_ventas$Publicidad,col="purple",pch=16,cex=0.7)
x = data_ventas$Publicidad
y = data_ventas$Ventas
y.hat = lm1$fitted.values

i <- 3

for (i in 1:100){
 
  print(paste0("Numero ", i))
   
}

i <- 5

for (i in 1:length(x)){
  lines(x=c(x[i],x[i]),y=c(y[i],y.hat[i]), col="blue")
}

lm1 = lm(Ventas ~ Publicidad, data=data_ventas)
summary(lm1)
confint(lm1, level=0.95)


lm2 <- lm(data_ventas$Ventas ~ data_ventas$Publicidad)
summary(lm2)
confint(lm2)
confint(lm2,level=0.99)


predict(lm1, newdata=data.frame(Publicidad=850), interval = c("confidence"),level=0.95)

predict(lm1, newdata=data.frame(Publicidad=850), interval = c("prediction"),level=0.95)



#install.packages("L1pack")
library(L1pack)

lm1_lad=lad(Ventas ~ Publicidad, data=data_ventas)
summary(lm1_lad)




