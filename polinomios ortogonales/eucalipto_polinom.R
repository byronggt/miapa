# Dr. Byron González
# http://byrong.cc

if(!require(readxl)){install.packages("readxl")}

euc<-read_excel("polinomioseu.xlsx")
attach(euc)
head(euc)
str(euc)
dk<-as.factor(dosisk)
# Andeva
mod1<-lm(altura~dk)
anova(mod1)

# Evaluación de polinomios hasta el grado 3 porque son t-1 dosis
mod2<-lm(altura ~ dosisk + I(dosisk^2) + I(dosisk^3))
anova(mod2)

# Coeficientes del modelo completo
summary(mod2)

# Construcción del modelo de grado 2
mod3<-lm(altura ~ dosisk + I(dosisk^2))
anova(mod3)

# El término lineal del modelo no es satisfactorio, se requiere eliminarlo
mod4<-lm(altura~-1+dosisk + I(dosisk^2), data=euc)
anova(mod4)




         
         




