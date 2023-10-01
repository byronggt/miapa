# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Colocar en memoria las bibliotecas a emplear

if(!require(car)){install.packages("car")}
if(!require(lattice)){install.packages("lattice")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(performance)){install.packages("performance")}
if(!require(rstatix)){install.packages("rstatix")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(readxl)){install.packages("readxl")}

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

# Análisis de varianza con AOV
model1<-aov(rend~bloque+lab*var*fer + Error(bloque/lab/var), psdv)
model1

# Revisión del supuesto de homogeneidad de varianzas (Pendiente)

psdv %>% levene_test(rend~ lab*var*fer)
bartlett.test(rend~interaction(lab,var,fer), psdv)
leveneTest( rend ~ lab*var*fer, psdv)


