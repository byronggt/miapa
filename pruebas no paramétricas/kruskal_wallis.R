# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(readxl)){install.packages("readxl")}
if(!require(stat)){install.packages("stat")}
if(!require(ggstatsplot)){install.packages("ggstatsplot")}

# Importar la tabla "epapa" ------
papa<-read_excel("epapa.xlsx", sheet = "data1")
head(papa)
attach(papa)

# Calcular el estadístico de prueba

kruskal.test(list(producto1,producto2,producto3,producto4,producto5,producto6))
# No se rechaza Ho
# Conclusión: No hay diferencia en la influencia de los productos sobre
# la brotación de papa

## Resolución mediante ggstatsplot ------


# Leer el archivo "epapa"
papa1<-read_excel("epapa.xlsx", sheet = "data2")
ggbetweenstats(
  data = papa1,
  x = producto,
  y = aspecto,
  type = "nonparametric", # ANOVA para Kruskall-Wallis
  plot.type = "box",
  pairwise.comparisons = T,
  pairwise.display = "significant",
  centrality.plotting = T,
  bf.message = FALSE
)
# Visitar https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html
# para los detalles de argumentos de la función que genera el gráfico
# También este vínculo para ampliar sobre las pruebas post hoc
# https://statsandr.com/blog/anova-in-r/#post-hoc-test 
# https://statsandr.com/blog/kruskal-wallis-test-nonparametric-version-anova/ 

# Prueba post hoc de Dunnet para Kruskal-Wallis
# con propósitos ilustrativos

# Calcular la mediana de los grupos
library(dplyr)
median.p<-papa1 %>%
  group_by(producto) %>%
  summarise(median.pr = median(aspecto))
median.p

# Ordenar en forma descendente
arrange(median.p, desc(median.pr))

# Ordenar los grupos
papa1$producto = factor(papa1$producto,
                        levels=c("producto3", 
                                 "producto2",
                                 "producto6",
                                 "producto4",
                                 "producto5",
                                 "producto1"))
levels(papa1$producto)

if(!require(FSA)){install.packages("FSA")}

DT <- dunnTest(aspecto ~ producto,
               data=papa1,
               method="bh")      # Ajusta p-valores para múltiples comparaciones

# Ver ?dunnTest para más opciones
DT

### Vista compacta de letras
PT = DT$res
PT

if(!require(rcompanion)){install.packages("rcompanion")}
cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)

# Método 2 de Posthoc
if(!require(PMCMRplus)){install.packages("PMCMRplus")}

DT1 = kwAllPairsDunnTest(aspecto ~ producto, data=papa1, method="bh")
DTT =PMCMRTable(DT1)
DTT
cldList(p.value ~ Comparison, data=DTT)

# Graficar las medianas e intervalos de confianza
# Solo para efectos ilustrativos de código
# porque el error ocurre al observar
# que hay demasiados valores de 2

#Sum = groupwiseMedian(aspecto ~ producto,
#                      data       = papa1,
#                      conf       = 0.95,
#                      R          = 5000,
#                      percentile = TRUE,
#                      bca        = FALSE,
#                      digits     = 4)

#Sum
#X     = 1:3
#Y     = Sum$Percentile.upper + 0.2
#Label = c("producto1", "producto2", "producto3", "producto4",
#          "producto5", "producto6")

#if(!require(ggplot2)){install.packages("ggplot2")}

#ggplot(Sum,                ### The data frame to use.
#       aes(x = producto,
#           y = Median)) +
#  geom_errorbar(aes(ymin = Percentile.lower,
#                    ymax = Percentile.upper),
#                width = 0.05,
#                size  = 0.5) +
#  geom_point(shape = 15,
#             size  = 4) +
#  theme_bw() +
#  theme(axis.title   = element_text(face  = "bold")) +
  
#  ylab("Mediana de la calificación de aspecto del tubérculo") +
  
#  annotate("text",
#           x = X,
#           y = Y,
#           label = Label)