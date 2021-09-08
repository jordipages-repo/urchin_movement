# # # # # # # # # # # # # # # # 
#   Trajectory analysis using #
#    straightness index and   #
#     "classic" parameters    #
#                             #
#       Jordi F. Pagès        #
#       September 2020        #
# # # # # # # # # # # # # # # # 

library(adehabitatLT)
source("qmomentsFunctions.R")
# # # 
# STRAIGHTNESS INDEX ----
# # # 

# We want to calculate the straightness index Is=D/L
# D=distance between the two points representing the first and last position of the path
# L=the path of each individual was broken into sections per minute of locomotion. The sum of all sections (length of path) is L.
# After several trials, we will use tau=1


# # # 
# Straigthness index for control sea urchins ----
# # #
load("RData/urch.null.RData")

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.null <- urch.null[-which(id(urch.null) == "20120528_4" | id(urch.null) == "20120528_5" | id(urch.null) == "20120528_6" | id(urch.null) == "20120607_5")]

# We iteratively calculate D for all individual sea urchins
# D=distance between the two points representing the first and last position of the path
tot.D <- NULL
for(i in 1:length(urch.null)){
  D <- sqrt((urch.null[[i]]$x[length(urch.null[[i]]$x)] - urch.null[[i]]$x[1])^2 + (urch.null[[i]]$y[length(urch.null[[i]]$y)]-urch.null[[i]]$y[1])^2)
  tot.D[i] <- D 
}

# We iteratively calculate the length of each movement step for all individual sea urchins, this is to get L, from straightness index formula.
# L=the path of each individual broken into sections per minute of locomotion. The sum of all sections (length of path) is L.
graella.t1 <- NULL
for(i in 1:length(urch.null)){
  dist <- urch.null[[i]]$dist
  nom <- adehabitatLT::id(urch.null)[i]
  tot <- data.frame(dist, rep(nom, length(urch.null[[i]]$x)))
  graella.t1 <- rbind(graella.t1, tot)
}
names(graella.t1) <- c("dist", "id")
sum.Lt1 <- tapply(graella.t1$dist, graella.t1$id, sum, na.rm = T)

# We finally calculate straightness index.
tortuosity.null <- tot.D/sum.Lt1


# # # 
# Straigthness index for sea urchins with predator cues ----
# # #
load("RData/urch.pred.RData")

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.pred <- urch.pred[-which(id(urch.pred) == "20120613_12" | id(urch.pred) == "20120614_8")]

# We iteratively calculate D for all individual sea urchins
# D=distance between the two points representing the first and last position of the path
tot.D <- NULL
for(i in 1:length(urch.pred)){
  D <- sqrt((urch.pred[[i]]$x[length(urch.pred[[i]]$x)] - urch.pred[[i]]$x[1])^2 + (urch.pred[[i]]$y[length(urch.pred[[i]]$y)]-urch.pred[[i]]$y[1])^2)
  tot.D[i] <- D 
}

# We iteratively calculate the length of each movement step for all individual sea urchins, this is to get L, from straightness index formula.
# L=the path of each individual broken into sections per minute of locomotion. The sum of all sections (length of path) is L.
graella.t1 <- NULL
for(i in 1:length(urch.pred)){
  dist <- urch.pred[[i]]$dist
  nom <- adehabitatLT::id(urch.pred)[i]
  tot <- data.frame(dist, rep(nom, length(urch.pred[[i]]$x)))
  graella.t1 <- rbind(graella.t1, tot)
}
names(graella.t1) <- c("dist", "id")
sum.Lt1 <- tapply(graella.t1$dist, graella.t1$id, sum, na.rm = T)

# We finally calculate straightness index.
tortuosity.pred <- tot.D/sum.Lt1


# # # 
# Testing straightness for each experiment ----
# # #

experiment <- c(rep("null", 29), rep("predator", 21))
dades <- data.frame(experiment, c(tortuosity.null, tortuosity.pred))
names(dades) <- c("exp", "tortuosity")
dades$asin.tort <- asin(dades$tortuosity)
boxplot(tortuosity~exp, data = dades)
boxplot(asin.tort~exp, data = dades)
model1 <- lm(tortuosity~exp, data = dades)
car::Anova(model1)
# Analysis of Variance Table
# Response: tortuosity
#           Df Sum Sq  Mean Sq F value  Pr(>F)  
# exp        1 0.30617 0.306166  4.6603 0.0359 *
# Residuals 48 3.15346 0.065697        
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
TukeyHSD(aov(lm(tortuosity~exp, data = dades)))
mcheck(model1) # Good validation.

# model2 <- lm(asin.tort~exp, data = dades)
# anova(model2)
# # Analysis of Variance Table
# # Response: asin.tort
# #           Df Sum Sq Mean Sq F value   Pr(>F)   
# # exp        1 0.5530 0.55303  4.2385   0.04497 *
# # Residuals 48 6.2629 0.13048                       
# # ---
# # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# TukeyHSD(aov(lm(asin.tort~exp, data = dades)))
# mcheck(model2) # Validation is better for model1



# # # 
# Testing straightness for each experiment, with the day of trial as a random effect ----
# # #

library(tidyverse)
fin2 <- dades %>% 
  mutate(ID = rownames(dades),
         trial_day = str_sub(ID, 1,8))

library(nlme)
library(car)
m1 <- gls(tortuosity ~ exp, data = fin2)
m2 <- lme(tortuosity ~ exp, data = fin2, random = ~1|trial_day)
m3 <- lme(tortuosity ~ exp, data = fin2, random = ~1|trial_day, weights = varIdent(form = ~1|exp))
anova(m1, m2) # RANDOM EFFECT NOT NEEDED!
anova(m1, m3) # Weights NOT needed.
anova(m2, m3) # Weights NOT needed.
Anova(m1) 
Anova(m2)
Anova(m3)
# All 3 models give more or less the same results
mcheck(m1)
mcheck(m2) 
mcheck2(m3)
# All validations OK.



# Barplotting it
mitjana <- tapply(dades$tortuosity, dades$exp, mean)
error <- tapply(dades$tortuosity, dades$exp, std.error)

bp <- barplot(mitjana, ylim = c(0,1), ylab = "Tortuosity")
arrows(bp, mitjana, bp, mitjana + error,  lwd = 1.5, angle = 90, length = 0.1)
arrows(bp, mitjana, bp, mitjana - error,  lwd = 1.5, angle = 90, length = 0.1)
text(x = bp, y = mitjana + 0.15, labels = c("a", "b", "a"))


# Boxplot in ggplot
library(tidyverse)
library(dplyr)
# Boxplot
tort <- as_tibble(dades)
tort <- tort %>% 
  mutate(exp = recode(exp,
                      null = "Control",
                      predator = "Predators"))

ggplot(tort, aes(x = exp, y = tortuosity)) +
  geom_boxplot(aes(fill = exp)) +
  coord_cartesian(ylim = c(0,1)) +
  xlab("") +
  ylab("Straightness index") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/boxplot_straightness_vs_treatment.pdf")


# Barplot
tort %>% 
  group_by(exp) %>% 
  summarise(mean = mean(tortuosity),
            std = std.error(tortuosity)) %>% 
  ggplot(aes(x = exp, y = mean)) +
  geom_bar(aes(fill = exp), stat = "identity") +
  geom_errorbar(aes(ymin = mean-std, ymax = mean + std), width = 0.1) +
  coord_cartesian(ylim = c(0,1)) +
  xlab("") +
  ylab("Straightness index") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/barplot_straightness_vs_treatment.pdf")


# # # 
# CALCULATING VELOCITY ----
# # # 

# # # 
# Velocity for sea urchins from the controls ----
# # #
detach("package:dplyr")
load("RData/urch.null.RData")

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.null <- urch.null[-which(id(urch.null) == "20120528_4" | id(urch.null) == "20120528_5" | id(urch.null) == "20120528_6" | id(urch.null) == "20120607_5")]

# Calculating velocity
mean.speed.null <- data.frame(matrix(ncol = 2, nrow = 1))
for(i in 1:length(urch.null)){
  mean.speed.null[i,1] <- adehabitatLT::id(urch.null)[i]
  mean.speed.null[i,2] <- mean(urch.null[[i]]$dist/0.5, na.rm = T) # velocity is in píxels . minut-1
}
names(mean.speed.null) <- c("ID", "mean.speed")


# # # 
# Velocity for sea urchins with predator cues ----
# # #

load("RData/urch.pred.RData")

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.pred <- urch.pred[-which(id(urch.pred) == "20120613_12" | id(urch.pred) == "20120614_8")]

# Calculating velocity
mean.speed.pred <- data.frame(matrix(ncol = 2, nrow = 1))
for(i in 1:length(urch.pred)){
  mean.speed.pred[i,1] <- adehabitatLT::id(urch.pred)[i]
  mean.speed.pred[i,2] <- mean(urch.pred[[i]]$dist/0.5, na.rm = T) # en píxels per minut
}
names(mean.speed.pred) <- c("ID", "mean.speed")


# # # 
# Testing velocity for each experiment ----
# # #

experiment <- c(rep("null", 29), rep("predator", 21))
dades <- cbind(experiment, rbind(mean.speed.null, mean.speed.pred))
boxplot(mean.speed~experiment, data = dades) # We can see 2 outliers

# Modelling velocity vs. experiment, without deleting outliers
model <- lm(mean.speed~experiment, data = dades)
car::Anova(model) 
# Analysis of Variance Table
# Response: mean.speed
#           Df  Sum Sq Mean Sq F value   Pr(>F)   
# exp        1  2656.9 2656.94   11.35 0.001495 **
# Residuals 48 11236.5  234.09                
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
mcheck(model) # Validation is OK!

# # Modelling velocity vs. experiment, deleting outliers
# dades2 <- dades[-44,]
# boxplot(mean.speed~exp, data = dades2)
# model1 <- lm(mean.speed~exp, data = dades2)
# anova(model1)
# # Analysis of Variance Table
# # Response: mean.speed
# #           Df  Sum Sq Mean Sq F value    Pr(>F)
# # exp        1 3310.6  3310.6  16.154 0.0002097 ***
# # Residuals 47 9632.3   204.9
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# mcheck(model1)


# # # 
# Testing velocity for each experiment, with the day of trial as a random effect ----
# # #

library(tidyverse)
library(dplyr)
fin2 <- dades %>% 
  mutate(trial_day = str_sub(ID, 1,8))

library(nlme)
library(car)
m1 <- gls(mean.speed ~ experiment, data = fin2)
m2 <- lme(mean.speed ~ experiment, data = fin2, random = ~1|trial_day)
m3 <- lme(mean.speed ~ experiment, data = fin2, random = ~1|trial_day, weights = varIdent(form = ~1|experiment))
anova(m1, m2) # RANDOM EFFECT NOT NEEDED!
anova(m1, m3) # Weights NOT needed.
anova(m2, m3) # Weights NOT needed.
Anova(m1) 
Anova(m2)
Anova(m3)
# All 3 models give more or less the same results
mcheck(m1)
mcheck(m2)
mcheck2(m3)
# All validations OK.


# Barplotting velocities
dades$mean.speed.cm <- dades$mean.speed*0.1834 # els 0.1834 is the equivalence pixel-cm for our experiment
mitjana <- tapply(dades$mean.speed.cm, dades$exp, mean)
error <- tapply(dades$mean.speed.cm, dades$exp, std.error)
bp <- barplot(mitjana, ylim = c(0,13), ylab = "Mean speed (cm/minute)")
arrows(bp, mitjana, bp, mitjana + error,  lwd = 1.5, angle = 90, length = 0.1)
arrows(bp, mitjana, bp, mitjana - error,  lwd = 1.5, angle = 90, length = 0.1)
text(x = bp, y = mitjana + 1.5, labels = c("a", "b", "a"))


# Boxplot in ggplot
library(tidyverse)
library(dplyr)
# Boxplot
speed <- as_tibble(dades)
speed <- speed %>% 
  mutate(exp = recode(exp,
                      null = "Control",
                      predator = "Predators"))

ggplot(speed, aes(x = exp, y = mean.speed.cm)) +
  geom_boxplot(aes(fill = exp)) +
  xlab("") +
  ylab("Mean speed (cm/minute)") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/boxplot_speed_vs_treatment.pdf")


# Barplot
speed %>% 
  group_by(exp) %>% 
  summarise(mean = mean(mean.speed.cm),
            std = std.error(mean.speed.cm)) %>% 
  ggplot(aes(x = exp, y = mean)) +
  geom_bar(aes(fill = exp), stat = "identity") +
  geom_errorbar(aes(ymin = mean-std, ymax = mean + std), width = 0.1) +
  xlab("") +
  ylab("Mean speed (cm/minute)") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/barplot_speed_vs_treatment.pdf")


# # # # 
# END #
# # # # 

