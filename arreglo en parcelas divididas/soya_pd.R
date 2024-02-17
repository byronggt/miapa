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

#Verificación de supuestos del modelo matemático-estadístico (Pendiente se revisión)
resg1<-aov(rend~bloque+esurcos+dsem+esurcos*dsem+Error(bloque/esurcos))

summary(resg1)
plot(resg1,1)
plot(resg1,2)
plot(resg1,5)
shapiro.test(resg1$res)

bartlett.test(resg1$res, esurcos)
bartlett.test(resg1$res, dsem)
bartlett.test(resg1$res, esurcos*dsem)
