# Dr. Byron González
# http://byrong.cc

if(!require(data.table)){install.packages("data.table")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(tidyverse)){install.packages("tidyverse")}

# Eliminar objetos antiguos de la memoria
rm(list=ls())

PD<- fread("https://archive.org/download/Cerdos_201810/Cerdos.txt",header=T, sep="\t", dec=",")
Raz<- factor(PD$Raza)
Con<- factor(PD$Conc)
Re<-factor(PD$Rep)
Pe<-as.vector(PD$Peso)
Pe1<-as.numeric(Pe)

# Gráfico de la interacción
interaction.plot(Raz,Con,Pe1, fixed=F, xlab="Raza de Cerdos", ylab="Ganancia de Peso", legend = T, type = "b",trace.label="Concentrado", pch = c(5,7,5))

# Gráfico de interacción con ggplot2
PD %>% 
  ggplot() +
  aes(x = Raz, color = Con, group = Con, y = Pe1) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  xlab("Razas") + ylab("Peso de los cerdos")+
  labs(colour = "% Concentrado")

PD %>% 
  ggplot() +
  aes(x = Con, color = Raz, group = Raz, y = Pe1) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  xlab("% Concentrado") + ylab("Peso de los cerdos")+
  labs(colour = "Razas")


# Análisis de varianza
mod.DCA <- aov(Pe1~Raz+Error(Re/Raz)+Con+Raz:Con)
summary(mod.DCA)
model.tables(mod.DCA, type="means")


# Revisión de los supuestos del modelo
# Andeva para calcular un solo residuo
resp1<-aov(Pe1~Raz*Con+Re/Raz)
summary(resp1)

# Gráficos de predichos vs residuos
plot(resp1,1)

# Gráfico de cuantil-cuantil
plot(resp1,2)

# Prueba de Shapiro & Wilk
shapiro.test(resp1$residuals)

# Prueba de homogeneidad de varianzas
residuos<-residuals(resp1)
check_model(resp1)
check_normality(resp1)
bartlett.test(residuos~interaction(Raz,Con),resp1)

# Razas, colocadas en las parcelas grandes
bartlett.test(resp1$res, Raz)

# Porcentaje de concentrado, colocado en las parcelas pequeñas
bartlett.test(resp1$res, Con)

# Interacción de tipo de raza y porcentaje de concentrado
bartlett.test(resp1$res, Raz:Con)

# Ejemplo de prueba de medias (aunque no sea necesario)

# Tipos de Razas, colocados en las parcelas grandes

Tukey_R<-HSD.test(Pe1, Raz, DFerror = 6, MSerror = 3.16);Tukey_R

#Porcentaje de concentrado, colocadas en las parcelas pequeñas

Tukey_C<-HSD.test(Pe1, Con, DFerror = 12, MSerror = 3.45);Tukey_C

# Interacción Tipo de razas y porcentaje de concentrado

Tukey_RC<-HSD.test(Pe1, Raz:Con, DFerror = 12, MSerror = 3.45);Tukey_RC

