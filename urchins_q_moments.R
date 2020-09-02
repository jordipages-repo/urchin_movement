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
# We start with the group of urchins we called "null" = Homogeneous light and no predator cues
# These urchins are the "control" group, and we'll use them to compare against the rest of treatments

# We want a single data.frame with all the data with the x, y coordinates
urch.null.MAT <- NULL
for(i in 1:length(urch.null)){
  x <- urch.null[[i]]$x
  y <- urch.null[[i]]$y
  ID <- rep(id(urch.null)[i], length(x))
  time <- urch.null[[i]]$date
  matriu <- data.frame(x, y, ID, time)
  urch.null.MAT <- rbind(urch.null.MAT, matriu)
}

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.null.MAT <- subset(urch.null.MAT, ID != "20120528_4" & ID != "20120528_5" & ID != "20120528_6" & ID != "20120607_5")

# We'll now calculate the q's exponents for a whole stack of urchins with qmom() function
listExp.urch.null <- qmom(urch.null.MAT)

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
# We now study the predator cues treatmetn = Homogeneous light and predator cues
# These urchins are the "control" group, and we'll use them to compare against the rest of treatments

# We want a single data.frame with all the data with the x, y coordinates
urch.pred.MAT <- NULL
for(i in 1:length(urch.pred)){
  x <- urch.pred[[i]]$x
  y <- urch.pred[[i]]$y
  ID <- rep(id(urch.pred)[i], length(x))
  time <- urch.pred[[i]]$date
  matriu <- data.frame(x, y, ID, time)
  urch.pred.MAT <- rbind(urch.pred.MAT, matriu)
}

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.pred.MAT <- subset(urch.pred.MAT, ID != "20120613_12" & ID != "20120614_8")

# We'll now calculate the q's exponents for a whole stack of urchins with qmom() function
listExp.urch.pred <- qmom(urch.pred.MAT)

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
# Shadows treatment ----
# # # 

load("RData/urch.shadows.RData")
# We now study the predator cues treatmetn = Homogeneous light and predator cues
# These urchins are the "control" group, and we'll use them to compare against the rest of treatments

# We want a single data.frame with all the data with the x, y coordinates
urch.shadows.MAT <- NULL
for(i in 1:length(urch.shadows)){
  x <- urch.shadows[[i]]$x
  y <- urch.shadows[[i]]$y
  ID <- rep(id(urch.shadows)[i], length(x))
  time <- urch.shadows[[i]]$date
  matriu <- data.frame(x, y, ID, time)
  urch.shadows.MAT <- rbind(urch.shadows.MAT, matriu)
}

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.shadows.MAT <- subset(urch.shadows.MAT, ID != "20120621_1" & ID != "20120621_2" & ID != "20120621_3" & ID != "20120621_4" & ID != "20120621_5" & ID != "20120621_6" & ID != "20120622_4" & ID != "20120622_5" & ID != "20120626_5" & ID != "20120626_6" & ID != "20120627_9" & ID != "20120629_12")

# We'll now calculate the q's exponents for a whole stack of urchins with qmom() function
listExp.urch.shadows <- qmom(urch.shadows.MAT)


# We now plot all urchins from the shadows treatment, individually
# This first line, is to identify the 3 bearing at which the shadow was moved for each individual
listExp.urch.shadows$indiv <- c(rep(1,9),rep(1,9),rep(1,9),rep(2,9),rep(3,9),rep(3,9),rep(3,9),rep(4,9),rep(5,9),rep(5,9),rep(6,9),rep(9,9),rep(6,9),rep(6,9),rep(7,9),rep(7,9),rep(7,9),rep(8,9),rep(8,9),rep(10,9),rep(10,9),rep(10,9),rep(11,9),rep(11,9),rep(11,9),rep(12,9),rep(12,9),rep(12,9),rep(13,9),rep(16,9),rep(16,9),rep(13,9),rep(13,9),rep(14,9),rep(14,9),rep(14,9),rep(15,9),rep(15,9),rep(15,9))
qs <- seq(from = 0, to = 8, by=1)
plot(y=qs/2, x = qs, type = "l", xlab = "q", ylab = "Psi(q)", lwd = 1.5, lty = 2, ylim = c(0,8), xlim = c(0,8)) # Brownian motion Psi(q) = q/2. Dashed line
lines(y = qs, x = qs, lwd = 1.5, lty = 3) # Ballistic motion Psi(q) = q. Dotted line
nom.indiv <-unique(listExp.urch.shadows$indiv)
for(i in 1:length(nom.indiv)){
  individu <- subset(listExp.urch.shadows, indiv == nom.indiv[i])
  exponents <- as.numeric(tapply(individu$exponents, individu$num, mean))
  lines(x = qs, y = exponents)
}
mean.exp <- tapply(listExp.urch.shadows$exponents, listExp.urch.shadows$num, mean)
sd.exp <- tapply(listExp.urch.shadows$exponents, listExp.urch.shadows$num, sd)
conf.int <- tapply(listExp.urch.shadows$conf.int, listExp.urch.shadows$num, mean)
lines(x = qs, y = as.numeric(mean.exp), type = "l", xlab = "q", 
      ylab = expression(paste(zeta,"(q)")), xlim = c(0,8), ylim = c(0,8), 
      col = "red", lwd = 4)  




# # # 
# Comparing the exponents for each experimental condition ----
# # # 

indiv.exp.null <- indiv_exp(listExp.urch.null)
indiv.exp.pred <- indiv_exp(listExp.urch.pred)
indiv.exp.shadows <- indiv_exp_shadows(listExp.urch.shadows)

fin <- rbind(indiv.exp.null, indiv.exp.pred, indiv.exp.shadows)
fin.id <- c(rep("null", 29), rep("predat", 21), rep("shadows", 16))
fin <- cbind(fin, fin.id)

boxplot(fin$coef~fin$fin.id)
testem <- aov(lm(fin$coef~fin$fin.id))
summary(testem)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# fin$fin.id   2 0.3736  0.1868   4.777 0.0117 *
# Residuals   63 2.4635  0.0391                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
TukeyHSD(testem)
#                       diff         lwr          upr     p adj
# predat-null     0.158786165  0.02278303  0.294789304 0.0182241
# shadows-null   -0.007365565 -0.15518102  0.140449889 0.9921450
# shadows-predat -0.166151730 -0.32366016 -0.008643302 0.0363996

# Barplot of the exponents for each treatment
mitj <- tapply(fin$coef, fin$fin.id, mean)
#     null    predat   shadows 
# 0.6743501 0.8331362 0.6669845
std <- tapply(fin$coef, fin$fin.id, std.error)
# null     predat    shadows 
# 0.04525674 0.02991768 0.04205228 
bp <- barplot(mitj, ylim = c(0,1), ylab = "Psi(q) slope", names.arg = c("null", "predators", "shadows"))
arrows(bp, mitj, bp, mitj + std,  lwd = 1.5, angle = 90, length = 0.1)
arrows(bp, mitj, bp, mitj - std,  lwd = 1.5, angle = 90, length = 0.1)
text(x = bp, y = mitj + 0.15, labels = c("a", "b", "a"))


