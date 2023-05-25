# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Con información del texto de http://statforbiology.com
# de Andrea Onofri and Dario Sacco

rm(list=ls())

if(!require(tidyverse)){install.packages("tidyverse")}

fileName <- "https://www.casaonofri.it/_datasets/beet.csv"
dataset <- read_csv(fileName)
dataset <- dataset %>% 
  mutate(across(c(Tillage, WeedControl, Block), .fns = factor))
head(dataset)

# Andeva con efectos fijos
mod.aov <- aov(Yield ~ Block + Tillage*WeedControl +
                  Error(Block:Tillage), data = dataset)
summary(mod.aov)

# Andeva con modelos mixtos
# Considerar la combinación de bloque*labranza como efecto aleatorio

if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}

mod.mixto <- lmer(Yield ~ Block + WeedControl*Tillage +
                         (1|Block:Tillage), 
                       data=dataset)
# Revisar el cumplimiento de los supuestos
plot(mod.mixto)
shapiro.test(residuals(mod.mixto))


anova(mod.split, ddf = "Kenward-Roger")

# Dado que la interacción es significativa, se solicita 
# el procedimiento emmeans

if(!require(emmeans)){install.packages("emmeans")}
meansAB <- emmeans(mod.mixto, ~Tillage:WeedControl)
multcomp::cld(meansAB, Letters = LETTERS)


