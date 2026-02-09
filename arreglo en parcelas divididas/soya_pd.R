# Dr. Byron González
# http://byrong.cc

if(!require(readxl)){install.packages("readxl")}
if(!require(doebioresearch)){install.packages("doebioresearch")}
if(!require(performance)){install.packages("performance")}
if(!require(dplyr)){install.packages("dplyr")}

#Importar la tabla de datos
pdsoya<-read_excel("soya_pardiv.xlsx")
attach(pdsoya)

# Gráfico de la interacción
interaction.plot(esurcos,dsem,rend, fixed=F, xlab="Distancia entre surcos", ylab="Rendimiento", legend = T, type = "b",trace.label="Distancia entre semillas", pch = c(5,7,5))

# Análisis de varianza
mod1<-splitplot(pdsoya[4],bloque,esurcos,dsem,3); mod1

# Verificación de supuestos del modelo matemático-estadístico

pdsoya<- pdsoya %>% 
  mutate(across(c(esurcos, dsem, bloque), .fns = factor))

mod2<-aov(rend~bloque+esurcos*dsem+bloque:esurcos, data = pdsoya)

summary(mod2)

plot(mod2,1)
<<<<<<< HEAD
plot(mod2,2)


if(!require(ScottKnott)){install.packages("ScottKnott")}
library(ScottKnott)

sk_esurcos <- SK(mod2, which = "esurcos", error = "bloque:esurcos")
summary(sk_esurcos)

sk_dsem <- SK(mod2, which = "dsem")
summary(sk_dsem)

sk_inter <- SK(mod2, which = "esurcos:dsem")
summary(sk_inter)
=======
plot(mod2,2)
>>>>>>> 084a5eae0aca40f9413824418c67f765f47129c6
