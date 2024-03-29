# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(readxl)){install.packages("readxl")} # Lectura de archivos de Excel

nem<-read_excel("nem_lec3.xlsx")
print(nem)
attach(nem)
# Anova con la variable original y revisión de supuestos
mod<-lm(Lar_h~trat)
anova(mod, test=F)

# Note que en los gráficos se aprecia problemas de heterogeneidad de varianzas y de normalidad

# Observar que hay forma de embudo en el gráfico de residuos vs predichos

if(!require(performance)){install.packages("performance")}

check_model(mod)
check_normality(mod)

# En la prueba de Shapiro & Wilk se rechaza Ho. La distribución de los residuos no es normal
shapiro.test(mod$residuals)
# Se transforman los datos al sumar la unidad (1) y mediante el uso de la 
# familia de transformaciones de Box-Cox

if(!require(car)){install.packages("car")}

lar1=Lar_h+1
summary(powerTransform(lar1))
nem$pot_lar1=lar1^0.1307
colnames(nem)
mod1<-lm(nem$pot_lar1~trat)
anova(mod1, test=F)

# Ahora se cumple con los supuestos de homogeneidad de varianzas y normalidad
# Debe de tener instalada la library(performance)
check_model(mod1)
check_normality(mod1)

# El p-value de la prueba de Shapiro & Wilk es superior a 0.05 y satisfactorio
shapiro.test(mod1$residuals)
resultado<-aov(nem$pot_lar1~trat)

if(!require(agricolae)){install.packages("agricolae")}

outHSD<-HSD.test(resultado, "trat",console=TRUE)
outHSD

# Presentación de la prueba Post-Anova

if(!require(flextable)){install.packages("flextable")}
tablaT <- flextable(outHSD$groups)
tablaT <- set_caption(tablaT, "Prueba post Andeva bajo el criterio de Tukey")
tablaT
