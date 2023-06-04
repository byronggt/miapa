# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(lattice)){install.packages("lattice")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(performance)){install.packages("performance")}
if(!require(nlme)){install.packages("nlme")}
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}
if(!require(readxl)){install.packages("readxl")}

# Experimento en bloques completos al azar

# Importar la tabla de datos "frijol.xslx"
grupo<-read_excel("frijol.xlsx")
head(grupo)

grupo$Loc<- as.factor(grupo$Loc)
grupo$Lin<- as.factor(grupo$Lin)
grupo$Bloq<-as.factor(grupo$Bloq)
grupo$Loc_Bloq<-as.factor(grupo$Loc_Bloq)

attach(grupo)

# Gráfico da interacción
interaction.plot(Loc,Lin,Rend, fixed=T, xlab="Localidades", ylab="Rendimento") 

# Análisis exploratorio
sapply(as.list(grupo[c("Loc","Lin","Bloq")]), levels) # Revisar
with(grupo, table(Loc, Lin)) 		# Todas las localidades tienen todas las líneas
with(grupo, table(Loc, Bloq)) 	# Mismo número de bloques en cada experimento

# Análisis gráfico para rendimiento

xyplot(Rend~Lin|Loc) 
xyplot(Rend~Loc|Lin)


# Análisis de varianza por localidad
# Modelo estadístico: y_ij = mu + Bloq_i + Lin_j + e_ij
#  Bloq: efecto fijo de bloques, i=4
#  Lin: efecto fijo de las líneas, j=14
#  e: error experimental

names(grupo)

# Análisis para cada localidad (Ejemplo para el Jícaro)
jic <- subset(grupo, Loc=="Jicaro")

aov.jic<- lm(Rend~Bloq+Lin) # modelo de efectos fijos
summary(aov.jic)
plot(aov.jic,1)
plot(aov.jic,2)

# Análisis para todas las localidades de una sola vez
das <- split(grupo, grupo$Loc) # Genera una lista para cada localidad
str(das)
lapply(das, function(loc) with(loc, tapply(Rend, list(Bloq,Lin), identity)))

# Ajusta los modelos y muestra un anova por localidad

m01 <- lapply(das, FUN=aov, formula=Rend~Bloq+Lin) # ajusta para todas las localidades
lapply(m01, summary)               # Muestra los andevas
glrs <- sapply(m01, df.residual)   # Grados de libertad
qmrs <- sapply(m01, deviance)/glrs # Cuadrados medios

# Ajuste para el análisis de los residuos (modelo reducido)

mc11 <- lm(Rend~Loc/Bloq+Loc*Lin)
anova(mc11)
windows(10,10)
check_model(mc11)
check_normality(mc11)

# Análisis con modelos mixtos 

mc15<-lme(Rend~1+Loc+Lin+Loc:Lin
          ,random=list(Loc_Bloq=pdIdent(~1))
          ,weights=varComb(varIdent(form=~1|Loc))
          ,method="REML"
          ,control=lmeControl(niterEM=150
                              ,msMaxIter=200)
          ,na.action=na.omit
          ,data=grupo
          ,keep.data=FALSE)
summary(mc15)
anova(mc15) # Significancia en la interacción de localidad*línea

# Pendiente las pruebas de medias

