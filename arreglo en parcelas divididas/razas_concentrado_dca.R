# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Colocar en memoria las bibliotecas a necesitar
if(!require(readxl)){install.packages("readxl")}
if(!require(agricolae)){install.packages("agricolae")}

# Lectura de la tabla de datos de cerdos_concentrado
cerdos<-read_excel("cerdos_concentrado.xlsx")
head(cerdos)
attach(cerdos)
raza<-as.factor(raza)
rep<-as.factor(rep)
pconcent<-as.factor(pconcent)

# Gráfico de la interacción
attach(cerdos)
interaction.plot(raza,pconcent,gpeso, fixed=T, xlab="Raza de Cerdos", ylab="Ganancia de Peso", legend = T, type = "b",trace.label="Concentrado", pch = c(5,7,5))

# Análisis de varianza
model.DCA <- aov(gpeso~raza+Error(rep/raza)+pconcent+raza:pconcent)
summary(model.DCA)
model.tables(model.DCA, type="means")
summary(mod.DCA)

# Revisión de los supuestos del modelo
# Andeva para calcular un solo residuo
resp1<-aov(gpeso~raza*pconcent+rep/raza)
summary(resp1)

# Gráficos de predichos vs residuos
plot(resp1,1)

# Gráfico de cuantil-cuantil
plot(resp1,2)

# Prueba de Shapiro & Wilk
shapiro.test(resp1$residuals)

# Prueba de homogeneidad de varianzas

# Razas, colocadas en las parcelas grandes
bartlett.test(resp1$res, raza)

# Porcentaje de concentrado, colocado en las parcelas pequeñas
bartlett.test(resp1$res, pconcent)

# Interacción de tipo de raza y porcentaje de concentrado
bartlett.test(resp1$res, raza:pconcent)

# Ejemplo de prueba de medias (aunque no sea necesario)

# Tipos de Razas, colocados en las parcelas grandes

Tukey_R<-HSD.test(gpeso, raza, DFerror = 6, MSerror = 3.16);Tukey_R

#Porcentaje de concentrado, colocadas en las parcelas pequeñas

Tukey_C<-HSD.test(gpeso, pconcent, DFerror = 12, MSerror = 3.45);Tukey_C

# Interacción Tipo de razas y porcentaje de concentrado

Tukey_RC<-HSD.test(gpeso, raza:pconcent, DFerror = 12, MSerror = 3.45);Tukey_RC
