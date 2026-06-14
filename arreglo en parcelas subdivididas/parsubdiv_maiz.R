# Dr. Byron González
# http://byrong.cc

# Colocar en memoria las bibliotecas a emplear

if(!require(car)){install.packages("car")}
if(!require(lattice)){install.packages("lattice")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(performance)){install.packages("performance")}
if(!require(rstatix)){install.packages("rstatix")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(readxl)){install.packages("readxl")}
if(!require(AgroR)){install.packages("AgroR")}


# Experimento en bloques completos al azar con
# arreglo en parcelas subdidividas en maíz
# Parcela grande: labranza (Lab)
# Parcela mediana: variedad (Var)
# Parcela pequeña: fertilizante (Fer)

# Importar la tabla de datos "parsubdivmaiz.xslx"
psdv<-read_excel("parsubdivmaiz.xlsx")
head(psdv)
psdv$bloque<-as.factor(psdv$bloque)
psdv$lab<-as.factor(psdv$lab)
psdv$var<-as.factor(psdv$var)
psdv$fer<-as.factor(psdv$fer)
model<-with(psdv,ssp.plot(bloque,lab,var,fer,rend))
gla<-model$gl.a
glb<-model$gl.b
glc<-model$gl.c
Ea<-model$Ea
Eb<-model$Eb
Ec<-model$Ec
s1<-with(psdv,LSD.test(rend,lab,gla,Ea,console=T))
s2<-with(psdv,LSD.test(rend,var,glb,Eb,console=T))
s3<-with(psdv,LSD.test(rend,fer,glc,Ec,console=T))
plot(s1,xlab="Métodos de labranza",las=1,variation = "IQR")                              
plot(s2,xlab="Variedades de maíz", variation = "IQR")
plot(s3,xlab="Tipos de fertilizante", variation = "IQR")


# Revisión de los supuestos del Andeva

with(psdv,PSUBSUBDBC(lab,var,fer,bloque,rend,mcomp = "tukey"))

# Encontrar el valor de lambda para la transformación de Box-Cox.
# Usar un modelo lineal completo para estimar lambda
modelo_completo <- aov(rend ~ bloque + lab*var*fer, data = psdv)
bc <- boxcox(modelo_completo, lambda = seq(-2, 2, 1/10))
lambda <- bc$x[which.max(bc$y)]


# Imprimir el valor de lambda encontrado
print(paste("Lambda óptimo para Box-Cox:", round(lambda, 4)))

# Aplicar la transformación a la variable de respuesta.
# Crear una nueva columna 'rend_t' con los datos transformados.
psdv$rend_t <- (psdv$rend^lambda - 1) / lambda


# Volver a ejecutar el análisis de varianza con la variable transformada.
model_t <- with(psdv, ssp.plot(bloque, lab, var, fer, rend_t))

# Verificar nuevamente los supuestos con la variable transformada.
# Se espera que el p-valor de la prueba de Bartlett sea ahora > 0.05.

with(psdv, PSUBSUBDBC(lab, var, fer, bloque, rend_t, mcomp = "tukey"))


# Prueba de Tukey con datos transformados

# Extraer los nuevos grados de libertad y errores del modelo transformado.
glb_t <- model_t$gl.b
glc_t <- model_t$gl.c
Eb_t <- model_t$Eb
Ec_t <- model_t$Ec

# Prueba de Tukey para el factor Fertilizante (fer)
# Este es el factor de la parcela pequeña, se usa el Error C (Ec_t)

tukey_fer_t <- with(psdv, HSD.test(rend_t, fer, glc_t, Ec_t, console = TRUE))

# Prueba de Tukey para la interacción Labranza x Variedad (lab:var)
# Esta interacción corresponde a la parcela mediana, por lo que se usa el Error B (Eb_t)
# El ANOVA original mostró que esta interacción era marginalmente significativa (p=0.069)
# Con los datos transformados, el p-valor es 0.049. Es correcto analizarla
# Para analizar la interacción, es necesario crear un nuevo factor que combine los niveles de 'lab' y 'var'

psdv$lab_var <- with(psdv, interaction(lab, var))
tukey_lab_var_t <- with(psdv, HSD.test(rend_t, lab_var, glb_t, Eb_t, console = TRUE))