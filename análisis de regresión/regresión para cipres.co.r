# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(readxl)){install.packages("readxl")}
cipres.co<-read_excel("cipres.co.xlsx")
attach(cipres.co) 
plot(diametro,volumen)
plot(altura,volumen)
# Calcular un modelo de regresión lineal simple
regsimple<-lm(volumen~diametro)
summary(regsimple)
plot(diametro,volumen)
abline(regsimple, fitted.values=volumen, col="blue", lwd=2)
plot(regsimple,which=1:2)
shapiro.test(regsimple$residuals)
#Calcular un modelo lineal simple usando logaritmos
plot(log(diametro),log(volumen))
#Calcular un modelo con log en Y e X
#log(volumen)= a + b*Log(diametro) + e
reg<-lm(log(volumen)~log(diametro))
summary(reg)
#Revisión de supuestos
plot(reg,which=1:2)
shapiro.test(reg$residuals)
#Línea de regresión
plot(log(diametro),log(volumen))
abline(reg, pred = volumen, col="red", lwd=2)
#Convertir el modelo lineal transformado
#a un modelo de potencia de la forma volumen=a*diametro^b
#El modelo original es Log(volumen)=-7.69002+2.06162Log(diametro)
#Antilogaritmo a -7.69002
a<-exp(-7.69002)
a
#Modelo potencial: volumen=0.000457369*diametro^2.06162
#Graficar el modelo potencial
plot(diametro,volumen)
curve(0.000457369*x^2.06162, add=TRUE, col="blue", lwd=2)
#Continuación del modelo lineal...
#Calcular un modelo con dos entradas de datos para una variable independiente
#Ln(volumen)= a + b*Ln(D2*H) + e
reg1<-lm(log(volumen)~log(diametro^2*altura))
summary(reg1)
#Revisión de supuestos
plot(reg1,which=1:2)
shapiro.test(reg1$residuals)
#Graficar la línea de tendencia
plot(log(diametro^2*altura),log(volumen))
abline(reg1, pred = volumen, col="red", lwd=2)
d2=diametro^2*altura
#Ajustar un modelo de regresión polinomial de grado 2 entre volumen y diámetro
reg2<-lm(log(volumen)~log(diametro)+I(log(diametro)^2))#Agregar I al término cuadrático de manera que se opere
summary(reg2)
#Revisión de supuestos
plot(reg2,which=1:2)
shapiro.test(reg2$residuals)
#REVISAR si el modelo polinómico de grado 1 con log resulta ser adecuado
# O bien el modelo potencial
#Ajustar un modelo de regresión polinomial de grado 3 entre volumen y diámetro
reg3<-lm(log(volumen)~log(diametro)+I(log(diametro)^2)+I(log(diametro)^3))
summary(reg3)
plot(reg3,which=1:2)
#Revision de supuestos
shapiro.test(reg3$residuals)
plot(diametro,volumen, log="xy")
D <- 10^seq(par("usr")[1],par("usr")[2],length=7)
lines(D,exp(predict(reg3,newdata=data.frame(diametro=D))), col="red", lwd=2)
#resulta necesario mejorar el ajuste...
#Usar el principio de la parsimonia. Elegir el modelo con menor AIC y con el menor
# número de parámetros (navaja de Occam)
#Uso del modelo para predecir el intervalo de confianza
#Caso del modelo lineal simple para el árbol promedio de 20.8 cm
predict(regsimple,newdata=data.frame(diametro=20.8),interval="confidence",level=0.95)
#Uso del modelo para predecir el intervalo de predicción
#Caso del modelo lineal simple para un árbol de 20.8 cm de diámetro tomado al azar
predict(regsimple, newdata=data.frame(diametro=20.8),interval="prediction",level=0.95)
