# Ajuste de modelo polinomial de grado 2 para la madera y tensión
# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(performance)){install.packages("performance")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(readxl)){install.packages("readxl")}

# Importar el archivo de datos
madera<-read_excel("madera.y.tension.xlsx")

#Ajuste de un modelo lineal
# Note la falta de ajuste

mad1<- lm(tension~madera, data=madera)
summary(mad1)
check_model(mad1)

# Ajuste de un modelo polinomial de grado 2
mad2<- lm(tension~madera+I(madera^2), data=madera)
summary(mad2)
check_model(mad2)

# Note que debe eliminarse el intercepto
mad3<- lm(tension~-1+madera+I(madera^2), data=madera)
summary(mad3)
check_model(mad3)
