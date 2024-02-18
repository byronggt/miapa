# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(readxl)){install.packages("readxl")}
if(!require(doebioresearch)){install.packages("doebioresearch")}
if(!require(performance)){install.packages("performance")}
if(!require(dplyr)){install.packages("dplyr")}

#Importar la tabla de datos
pdsoya<-read_excel("soya_pardiv.xlsx")
attach(pdsoya)

# Gráfico de la interacción
interaction.plot(esurcos,dsem,rend, fixed=F, xlab="Distancia entre surcos", ylab="Distancia entre semillas", legend = T, type = "b",trace.label="Concentrado", pch = c(5,7,5))

# Análisis de varianza
mod1<-splitplot(pdsoya[4],bloque,esurcos,dsem,3); mod1

# Verificación de supuestos del modelo matemático-estadístico
# Ejecutar sin la línea 18 para evitar confusión
pdsoya<- pdsoya %>% 
  mutate(across(c(esurcos, dsem, bloque), .fns = factor))

mod2<-aov(rend~bloque+esurcos*dsem+bloque:esurcos, data = pdsoya)

summary(mod2)

plot(mod2,1)
plot(mod2,2)

