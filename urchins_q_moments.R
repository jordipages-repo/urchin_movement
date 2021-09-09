# # # # # # # # # # # # # # # # # # # # #
#  Calculating long range correlations  #
#   following Seuront & Stanley PNAS    #
#                                       #
#           Jordi F. Pagès              #
#           September 2020              # 
# # # # # # # # # # # # # # # # # # # # # 

library(adehabitatLT)


# # # 
# Loading function to estimate q moments ----
# # #
source("qmomentsFunctions.R")


# # # 
# Control urchins -----
# # # 
load("RData/urch.null.RData")
# We start with the group of urchins we called "null" = no predator cues
# These urchins are the "control" group, and we'll use them to compare against the rest of treatments

# We want a single data.frame with all the data with the x, y coordinates
urch.null.MAT <- NULL
for(i in 1:length(urch.null)){
  x <- urch.null[[i]]$x
  y <- urch.null[[i]]$y
  ID <- rep(adehabitatLT::id(urch.null)[i], length(x))
  time <- urch.null[[i]]$date
  matriu <- data.frame(x, y, ID, time)
  urch.null.MAT <- rbind(urch.null.MAT, matriu)
}

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.null.MAT <- subset(urch.null.MAT, ID != "20120528_4" & ID != "20120528_5" & ID != "20120528_6" & ID != "20120607_5")

# We'll now calculate the q's exponents for a whole stack of urchins with qmom() function
# listExp.urch.null <- qmom(urch.null.MAT)
listExp.urch.null <- qmom_logbin(urch.null.MAT)

# We now plot all control urchins individually
qs <- seq(from = 0, to = 8, by=1)
plot(y=qs/2, x = qs, type = "l", xlab = "q", ylab = expression(paste(zeta,"(q)")), 
     lwd = 1.5, lty = 2, ylim = c(0,8), xlim = c(0,8)) # Brownian motion Psi(q) = q/2. Dashed line
lines(y = qs, x = qs, lwd = 1.5, lty = 3) # Ballistic motion Psi(q) = q. Dotted line
noms <-unique(listExp.urch.null$ID)
for(i in 1:length(noms)){
  indiv <- subset(listExp.urch.null, ID == noms[i])
  lines(x = qs, y = indiv$exponents)
}
mean.exp <- tapply(listExp.urch.null$exponents, listExp.urch.null$num, mean)
sd.exp <- tapply(listExp.urch.null$exponents, listExp.urch.null$num, sd)
conf.int <- tapply(listExp.urch.null$conf.int, listExp.urch.null$num, mean)
lines(x = qs, y = as.numeric(mean.exp), type = "l", xlab = "q", 
     ylab = expression(paste(zeta,"(q)")), xlim = c(0,8), ylim = c(0,8), 
     col = "red", lwd = 4)




# # # 
# Predator cues treatment ----
# # # 
load("RData/urch.pred.RData")
# We now study the predator cues treatment = predator cues
# These urchins are the "control" group, and we'll use them to compare against the rest of treatments

# We want a single data.frame with all the data with the x, y coordinates
urch.pred.MAT <- NULL
for(i in 1:length(urch.pred)){
  x <- urch.pred[[i]]$x
  y <- urch.pred[[i]]$y
  ID <- rep(adehabitatLT::id(urch.pred)[i], length(x))
  time <- urch.pred[[i]]$date
  matriu <- data.frame(x, y, ID, time)
  urch.pred.MAT <- rbind(urch.pred.MAT, matriu)
}

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.pred.MAT <- subset(urch.pred.MAT, ID != "20120613_12" & ID != "20120614_8")

# We'll now calculate the q's exponents for a whole stack of urchins with qmom() function
listExp.urch.pred <- qmom_logbin(urch.pred.MAT)

# We now plot all urchins from the predator cues treatment, individually
qs <- seq(from = 0, to = 8, by=1)
plot(y=qs/2, x = qs, type = "l", xlab = "q", ylab = expression(paste(zeta,"(q)")), 
     lwd = 1.5, lty = 2, ylim = c(0,8), xlim = c(0,8)) # Brownian motion Psi(q) = q/2. Dashed line
lines(y = qs, x = qs, lwd = 1.5, lty = 3) # Ballistic motion Psi(q) = q. Dotted line
noms <-unique(listExp.urch.pred$ID)
for(i in 1:length(noms)){
  indiv <- subset(listExp.urch.pred, ID == noms[i])
  lines(x = qs, y = indiv$exponents)
}
mean.exp <- tapply(listExp.urch.pred$exponents, listExp.urch.pred$num, mean)
sd.exp <- tapply(listExp.urch.pred$exponents, listExp.urch.pred$num, sd)
conf.int <- tapply(listExp.urch.pred$conf.int, listExp.urch.pred$num, mean)
lines(x = qs, y = as.numeric(mean.exp), type = "l", xlab = "q", 
      ylab = expression(paste(zeta,"(q)")), xlim = c(0,8), ylim = c(0,8), 
      col = "red", lwd = 4)


# # # 
# Comparing the exponents for each experimental condition ----
# # # 

indiv.exp.null <- indiv_exp(listExp.urch.null)
indiv.exp.pred <- indiv_exp(listExp.urch.pred)

fin <- rbind(indiv.exp.null, indiv.exp.pred)
fin.id <- c(rep("null", 29), rep("predat", 21))
fin <- cbind(fin, fin.id)
names(fin)[3] <- "treatment"

boxplot(fin$coef~fin$treatment)
testem <- aov(lm(fin$coef~fin$treatment))
summary(testem)
#               Df Sum Sq Mean Sq F value  Pr(>F)   
# fin$treatment  1 0.1053  0.1053    9.15 0.00399 **
# Residuals     48 0.5522  0.0115                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
TukeyHSD(testem)
#               diff        lwr       upr     p adj
# predat-null 0.09296187 0.03117168 0.1547521 0.003987

# Model validation
mcheck(testem) # We see quite some heterogeneity, but OK normality.

# Let's deal with this heterogeneity
library(nlme)
m1 <- gls(coef ~ treatment, data = fin)
m2 <- gls(coef ~ treatment, weights = varIdent(form = ~1|treatment), data = fin)
anova(m1, m2) # Weights are needed

mfinal <- gls(coef ~ treatment, weights = varIdent(form = ~1|treatment), data = fin)
car::Anova(mfinal)
# Analysis of Deviance Table (Type II tests)
# Response: coef
#           Df  Chisq Pr(>Chisq)   
# treatment  1 10.739   0.001049 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
mcheck2(mfinal) # Now, using Pearson's residuals, we see that there is no longer heterogeneity in the residuals.

# Barplot of the exponents for each treatment
mitj <- tapply(fin$coef, fin$treatment, mean)
#     null    predat    
# 0.8211005 0.9140624 
std <- tapply(fin$coef, fin$treatment, std.error)
#     null     predat     
# 0.02337459 0.01607287   
bp <- barplot(mitj, ylim = c(0,1), ylab = "Psi(q) slope", names.arg = c("null", "predators"))
arrows(bp, mitj, bp, mitj + std,  lwd = 1.5, angle = 90, length = 0.1)
arrows(bp, mitj, bp, mitj - std,  lwd = 1.5, angle = 90, length = 0.1)
text(x = bp, y = mitj + 0.05, labels = c("a", "b"))


# Final plot in ggplot
library(tidyverse)
library(dplyr)
fin <- as_tibble(fin)
fin <- fin %>% 
  mutate(treatment = recode(treatment, 
                            null = "Control",
                            predat = "Predators"),)

# Boxplot
ggplot(fin, aes(x = treatment, y = coef)) +
  geom_boxplot(aes(fill = treatment)) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 1, lty = 3) +
  xlab("") +
  ylab(expression(paste(zeta,"(q)"))) +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/boxplot_exponents_vs_treatment.pdf")


# Barplot
fin %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(coef),
            std = std.error(coef)) %>% 
ggplot(aes(x = treatment, y = mean)) +
  geom_bar(aes(fill = treatment), stat = "identity") +
  geom_errorbar(aes(ymin = mean-std, ymax = mean + std), width = 0.1) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 1, lty = 3) +
  xlab("") +
  ylab(expression(paste(zeta,"(q)"))) +
  # coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/barplot_exponents_vs_treatment.pdf")



# # # 
# Comparing the exponents for each experimental condition, with random effect: trial day ----
# # # 

fin2 <- fin %>% 
  mutate(trial_day = str_sub(ID, 1,8))

m1 <- gls(coef ~ treatment, data = fin2)
m2 <- lme(coef ~ treatment, data = fin2, random = ~1|trial_day)
m3 <- lme(coef ~ treatment, data = fin2, random = ~1|trial_day, weights = varIdent(form = ~1|treatment))
anova(m1, m2) # RANDOM EFFECT NOT NEEDED!
anova(m1, m3) # Weights needed.
anova(m2, m3) # Weights needed.
Anova(m1) 
Anova(m2)
Anova(m3)
# All 3 models give more or less the same results
mcheck(m1)
mcheck(m2) # Both heteroscedastic.
mcheck2(m3) # Now solved.

