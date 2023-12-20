# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(readxl)){install.packages("readxl")}
if(!require(ScottKnott)){install.packages("ScottKnott")}
if(!require(agricolae)){install.packages("agricolae")}

alpacas<-read_excel("peso_alpacas.xlsx")
head(alpacas)

# Convertir a factores
alpacas$raza<-as.factor(alpacas$raza)
alpacas$ambiente<-as.factor(alpacas$ambiente)
alpacas$alimentacion<-as.factor(alpacas$alimentacion)
alpacas$peso<-as.vector(alpacas$peso)
alpacas$peso<-as.numeric(alpacas$peso)
str(alpacas)
attach(alpacas)

# Gráficos de interacción
interaction.plot(ambiente,raza,peso, fixed=T, xlab="Ambiente", ylab="peso",col = "blue")
interaction.plot(alimentacion,raza, peso, fixed=T, xlab="Alimentación", ylab="peso",col = "blue")
interaction.plot(alimentacion, ambiente, peso, fixed=T, xlab="Alimentación", ylab="peso",col = "blue")

# Análisis de varianza en arreglo combinatorio

mod1<-aov(peso~raza*ambiente*alimentacion)
anova(mod1)

# Revisión de los supuestos del anova
plot(mod1,1)
plot(mod1,2)
shapiro.test(mod1$residuals)

# Prueba de media para la interacción raza*ambiente
Tukey<-HSD.test(peso, raza:ambiente, DFerror = 60, MSerror = 0.4369);Tukey


