# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Revise los siguientes vídeos:
# https://www.youtube.com/watch?v=h1ZWj6TbuwU
# https://www.youtube.com/watch?v=5mtCMhOGDSM

# Para eliminar los objetos creados anteriormente

rm(list=ls())
# Bibliotecas a emplear

if(!require(readxl)){install.packages("readxl")}
if(!require(data.table)){install.packages("data.table")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(lsmeans)){install.packages("emmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(tidyverse)){install.packages("tidyverse")}

franjas<- fread("https://archive.org/download/byrong_Maiz/Maiz.txt",header=T, sep="\t", dec=",")
head(franjas)

# Andeva de efectos fijos

nitr<- factor(franjas$Nitrog)
riego<- factor(franjas$Riego)
bloque<-factor(franjas$Bloque)

#Variable de respuesta
resp<-as.vector(franjas$Rend)
resp1<-as.numeric(resp)

#Gráfico de la interacción

franjas %>% 
  ggplot() +
  aes(x = nitr, color = riego, group = riego, y = resp1) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  xlab("Dosis de Nitrógeno") + ylab("Rendimiento de maíz")+
  labs(colour = "Niveles de Riego")

# Análisis de la varianza con agricolae

fran.anova<-strip.plot(bloque, nitr, riego, resp1)

# Revisión de los supuestos del Andeva
# Pendiente

# Pruebas de comparación múltiple de medias

FactorA<-HSD.test(resp1,nitr,fran.anova$gl.a,fran.anova$Ea);FactorA
FactorB<-HSD.test(resp1,riego,fran.anova$gl.b,fran.anova$Eb);FactorB
FactorAB<-HSD.test(resp1,riego:nitr,fran.anova$gl.c,fran.anova$Ec);FactorAB
