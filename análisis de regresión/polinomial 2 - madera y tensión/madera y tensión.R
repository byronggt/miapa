# Ajuste de modelo polinomial de grado 2 para la madera y tensión
# Dr. Byron González
# http://byrong.cc

if(!require(performance)){install.packages("performance")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(readxl)){install.packages("readxl")}
if(!require(car)) install.packages("car")

# Importar el archivo de datos
madera<-read_excel("madera.y.tension.xlsx")
attach(madera)
plot(madera,tension)
# Note la falta de ajuste

mad1<- lm(tension~madera, data=madera)
summary(mad1)
plot(mad1,which=1:2)
shapiro.test(mad1$residuals)


# Ajuste de un modelo polinomial de grado 2
mad2<- lm(tension~madera+I(madera^2), data=madera)
summary(mad2)
plot(mad2,which=1:2)
shapiro.test(mad2$residuals)
vif2<-vif(mad2); vif2

# Note que debe eliminarse el intercepto
mad3<- lm(tension~-1+madera+I(madera^2), data=madera)
summary(mad3)
plot(mad3,which=1:2)
shapiro.test(mad3$residuals)
vif3<-vif(mad3); vif3
AIC(mad2,mad3)
