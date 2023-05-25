# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Con información del texto de http://statforbiology.com

rm(list=ls())

if(!require(tidyverse)){install.packages("tidyverse")}

fileName <- "https://www.casaonofri.it/_datasets/beet.csv"
dataset <- read_csv(fileName)
dataset <- dataset %>% 
  mutate(across(c(Tillage, WeedControl, Block), .fns = factor))
head(dataset)

mod.aov <- aov(Yield ~ Tillage*WeedControl +
                 Error(Block/Tillage), data = dataset)

summary(mod.aov)

mod.aov2 <- aov(Yield ~ Block + Tillage*WeedControl +
                  Error(Block:Tillage), data = dataset)
summary(mod.aov2)

# Andeva con modelos mixtos
# Considerar la labranza como efecto aleatorio

if(!require(nlme)){install.packages("nlme")}

mod.lme <- lme(Yield ~ Block + Tillage*WeedControl,
               random = ~1|Block/Tillage, data = dataset)
anova(mod.lme)

if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}

mod.lmer.split <- lmer(Yield ~ Block + WeedControl*Tillage +
                         (1|Block:Tillage), 
                       data=dataset)
# Revisar el cumplimiento de los supuestos
shapiro.test(residuals(mod.lmer.split))


anova(mod.lmer.split, ddf = "Kenward-Roger")

# Dado que la interacción es significativa, se solicita 
# el procedimiento emmeans

if(!require(emmeans)){install.packages("emmeans")}
meansAB <- emmeans(mod.lmer.split, ~Tillage:WeedControl)
multcomp::cld(meansAB, Letters = LETTERS)


