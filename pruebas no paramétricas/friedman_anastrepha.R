# Dr. Byron González
# http://byrong.cc

if(!require(readxl)){install.packages("readxl")}
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(lattice)){install.packages("lattice")}
if(!require(coin)){install.packages("coin")}
if(!require(PMCMRplus)){install.packages("PMCMRplus")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(dplyr)){install.packages("dplyr")}

# Importar la tabla "anastrepha"
an<-read_excel("anastrepha.xlsx", sheet = "data2")
head(an)
str(an)
attach(an)

# Calcular el estadístico de prueba para Friedman

friedman.test(imuertos~trat | rep)
friedman.test(imuertos,trat,rep)

# Solicitar la mediana de imuertos por grupo

median.t<-an %>%
  group_by(trat) %>%
  summarize(median.t= median(imuertos))
median.t

## Prueba post hoc de Conover

# 1) Ordenar los grupos de acuerdo a su mediana
an$trat<- factor(an$trat,
                 levels = c("Beauveria", "Diplogasterido",
                            "Heteroabditis", "Spicaria", "Steirnema",
                            "Testigo"))
CT = frdAllPairsConoverTest(y = imuertos,
                            groups = trat,
                            blocks = rep,
                            p.adjust.method = "single-step")
CT
CTT=PMCMRTable(CT)
CTT

# 2) Mostrar los resultados con grupos de literales

cldList(p.value~Comparison, data = CTT)  