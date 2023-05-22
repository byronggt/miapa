# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Colocar en memoria las bibliotecas a necesitar
if(!require(corrplot)){install.packages("corrplot")}
if(!require(PerformanceAnalytics)){install.packages("PerformanceAnalytics")}
if(!require(ggcorrplot)){install.packages("ggcorrplot")}
if(!require(psych)){install.packages("psych")}
if(!require(GGally)){install.packages("GGally")}
if(!require(readxl)){install.packages("readxl")}
# Lectura de la tabla de datos de salinidad
salinidad<-read_excel("salinidad.xlsx")
corrplot(cor(salinidad), method="circle")
corrplot(cor(salinidad), method="ellipse")
corrplot(cor(salinidad), method="number")
corrplot(cor(salinidad), type="upper")
corrplot(cor(salinidad), type="lower")
chart.Correlation(salinidad, histogram=T, pch=15)
sal<-cor(salinidad)
ggcorrplot(sal)
ggcorrplot(sal, type="lower", lab=T)
pairs.panels(salinidad)
ggpairs(salinidad)
