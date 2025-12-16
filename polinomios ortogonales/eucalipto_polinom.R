# Dr. Byron González
# http://byrong.cc

if(!require(readxl)){install.packages("readxl")}

euc<-read_excel("polinomioseu.xlsx")
attach(euc)
head(euc)
str(euc)
dk<-as.factor(dosisk)
# Andeva
mod1<-lm(altura~dk)
anova(mod1)

# Evaluación de polinomios hasta el grado 3 porque son t-1 dosis
mod2<-lm(altura ~ dosisk + I(dosisk^2) + I(dosisk^3))
anova(mod2)

# Coeficientes del modelo completo
summary(mod2)

# Construcción del modelo de grado 2
mod3<-lm(altura ~ dosisk + I(dosisk^2))
anova(mod3)

# El término lineal del modelo no es satisfactorio, se requiere eliminarlo
mod4<-lm(altura~-1+dosisk + I(dosisk^2), data=euc)
anova(mod4)

# Gráfico de dispersión
plot(dosisk,altura,col="blue", xlab="Dosis de potasio (ppm)",ylab="Altura de planta (cm)", ylim = c(0, 180), main="Gráfico de dispersión")
d<-seq(0,100,1)
predicted.intervals <- predict(mod4,data.frame(dosisk=d),interval="confidence",level=0.95)         
lines(d,predicted.intervals[,1],col='green',lwd=3)
lines(d,predicted.intervals[,2],col='black',lwd=1)
lines(d,predicted.intervals[,3],col='black',lwd=1)
title(sub="Figura 1. Curva ajustada y valores observados")

# Revisión del ajuste del modelo
plot(fitted(mod4),residuals(mod4), xlab="Valores predichos",ylab=
                              "Residuos")

