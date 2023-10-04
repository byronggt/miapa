# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(coin)){install.packages("coin")}
if(!require(readxl)){install.packages("readxl")}

# Calcular el estadístico de U de Mann-Whitney a partir de
# la distribución exacta. Es decir, con aproximación a la normal

# Importar la hoja "data2" a usar con el paquete "coin"
cacao<-read_excel("cacao.xlsx", sheet = "data2")
head(cacao)
str(cacao)
attach(cacao)

# Convertir el campo de variedad en factor
variedad<-as.factor(variedad)

# Calcular el estadístico de prueba de Mann-Whitney
wilcox_test(temp~variedad)

# Realizar la prueba con wilcox.test del paquete base de R
# Importar la hoja 1 de "cacao"

cacao1<-read_excel("cacao.xlsx", sheet = "data1")
wilcox.test(cacao1$criollo, cacao1$trinidad, exact=F, alternative = "t", conf.int = 0.95)