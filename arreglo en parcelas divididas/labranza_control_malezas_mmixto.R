# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Con información del texto de http://statforbiology.com
# de Andrea Onofri and Dario Sacco 

rm(list=ls())

if(!require(tidyverse)){install.packages("tidyverse")}

fileName <- "https://www.casaonofri.it/_datasets/beet.csv"
labrm <- read_csv(fileName)
labrm <- labrm %>% 
  mutate(across(c(Tillage, WeedControl, Block), .fns = factor))
head(labrm)

# Andeva con efectos fijos
mod.aov <- aov(Yield ~ Block + Tillage*WeedControl +
                  Error(Block:Tillage), data = labrm)
summary(mod.aov)

# Verificación de supuestos del modelo fijo 
mod.aov1<-aov(Yield~Block+Tillage*WeedControl+Block:Tillage, data=labrm)
summary(mod.aov1)
plot(mod.aov1,1)
plot(mod.aov1,2)
plot(mod.aov1,3)

# Andeva con modelos mixtos
# Considerar la combinación de bloque*labranza como efecto aleatorio

if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}

mod.mixto <- lmer(Yield ~ Block + WeedControl*Tillage +
                         (1|Block:Tillage), 
                       data=labrm)
# Revisar el cumplimiento de los supuestos
plot(mod.mixto)
shapiro.test(residuals(mod.mixto))


anova(mod.mixto, ddf = "Kenward-Roger")

# Dado que la interacción es significativa, se solicita 
# el procedimiento emmeans

if(!require(emmeans)){install.packages("emmeans")}
meansAB <- emmeans(mod.mixto, ~Tillage:WeedControl)
multcomp::cld(meansAB, Letters = LETTERS)


