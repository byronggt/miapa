# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt 

if(!require(readxl)){install.packages("readxl")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(performance)){install.packages("performance")}
if(!require(car)){install.packages("car")}
if(!require(jtools)){install.packages("jtools")}
if(!require(caret)){install.packages("caret")}
if(!require(leaps)){install.packages("leaps")}
if(!require(MASS)){install.packages("MASS")}
if(!require(olsrr)){install.packages("olsrr")}
if(!require(corrplot)){install.packages("corrplot")}


# Ver la URL para referencia
# https://tinyurl.com/2fc6zpaj 

# Abrir la tabla de datos "fitness" -----
fitn<-read_excel("fitness.xlsx")
head(fitn)

# Modelo de regresión múltiple paso a paso para explicar 
# el Oxígeno
attach(fitn)
# Ajuste del modelo completo
model1<-lm(oxigeno~., data = fitn)
summary(model1)
ols_step_all_possible(model1)

k <- ols_step_all_possible(model1)
plot(k)


# Ajuste del modelo hacia adelante
model2<- lm(oxigeno ~ ., data = fitn)
ols_step_forward_p(model2, details = T) # Quedan 5 variables, aunque el peso no es significativo


# Graficar la selección del modelo hacia adelante
model.adl<- ols_step_forward_p(model2)
plot(model.adl)

# Ajuste del modelo, sin el peso
model3 <- lm(oxigeno ~ edad+tiempocorrer+freqcorrer+freqmaxcorrer, data = fitn)
summary(model3)


# Revisar VIF para cada variable independiente
ols_vif_tol(model3) # Valores superiores a 5 o cercanos a 10 son problema
ols_step_forward_p(model3, details = TRUE)

# Construir correlograma
fitn1<-fitn %>% 
  dplyr::select(edad,tiempocorrer,freqcorrer,freqmaxcorrer)

corrplot(cor(fitn1), diag = FALSE, order = "AOE",
         tl.pos = "td", tl.cex = 1.0, method = "number", type = "upper")
names(fitn)

# Verificar que en el nuevo modelo los VIF sean inferiores a 10
ols_vif_tol(model3)

# Graficar los residuos parciales con cada variable independiente
crPlots(model3)

# Eliminar la variable freqmaxcorrer
model4 <- lm(oxigeno ~ edad+tiempocorrer+freqcorrer, data = fitn)
summary(model4)


# Revisar VIF para cada variable independiente
ols_vif_tol(model4) # Valores superiores a 5 o cercanos a 10 son problema
ols_step_forward_p(model4, details = TRUE)



# Recalcular el modelo
ols_regress(model4)

## Verificar el cumplimiento de los supuestos
# Gráficos de residuos vs predichos
ols_plot_resid_fit(model4)

# QQ Plot de residuos
ols_plot_resid_qq(model4)

# Prueba de normalidad para los residuos
ols_test_normality(model4)

# Histograma de los residuos
ols_plot_resid_hist(model4)

# Prueba de homogeneidad de varianzas de los residuos
# Breusch-Pagan
ols_test_breusch_pagan(model4)

#########

# Establecer la mejor regresión en subconjunto
ols_step_best_subset(model1, metric= c("AIC"))

# Graficar el panel de criterios para el mejor modelo ajustado
k1 <- ols_step_best_subset(model1)
plot(k1)

############

# Selección hacia atrás

ols_step_backward_p(model1)
ols_step_backward_p(model1, details = TRUE)

# Selección paso a paso
ols_step_both_p(model1)
ols_step_both_p(model1, details = TRUE)

# Selección mediante AIC
ols_step_forward_aic(model1)
ols_step_forward_aic(model1, details = TRUE)


########### SEGUNDO CASO DE CREATININA ##########

# Modelo de regresión y residuos
creatin<-read_excel("creatininab.xlsx")
attach(creatin)
model0<-lm(creatinina~peso+estatura)
summary(model0)

# Ajuste del modelo hacia adelante
model1<- lm(creatinina ~ ., data = creatin)
ols_step_forward_p(model1, details = T) # Eliminar la estatura y el intercepto


# Graficar la selección del modelo hacia adelante
model.adl<- ols_step_forward_p(model1)
plot(model.adl)

# Ajuste del modelo, sin la estatura e intercepto
model2 <- lm(creatinina ~ peso -1, data = creatin)
summary(model2)

# Construir correlograma

corrplot(cor(creatin), diag = FALSE, order = "AOE",
         tl.pos = "td", tl.cex = 1.0, method = "number", type = "upper")
names(fitn)

# Revisión de los supuestos
windows(11)
check_model(model2)


check_normality(model2)

# Adición de predichos y residuos a la tabla original
creatin$pred_creat<-model2$fitted.values
creatin$res_creat<-model2$residuals


# Graficar el modelo ajustado
effect_plot(model2, pred = peso, interval = TRUE, plot.points = TRUE, x.lab="Peso (Kg)", y.lab="Creatinina (mg/día)")