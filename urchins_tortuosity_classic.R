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
  nom <- i
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
  nom <- i
  tot <- data.frame(dist, rep(nom, length(urch.pred[[i]]$x)))
  graella.t1 <- rbind(graella.t1, tot)
}
names(graella.t1) <- c("dist", "id")
sum.Lt1 <- tapply(graella.t1$dist, graella.t1$id, sum, na.rm = T)

# We finally calculate straightness index.
tortuosity.pred <- tot.D/sum.Lt1


# # # 
# Straigthness index for sea urchins from the shadows treatment ----
# # #

load("RData/urch.shadows.RData")

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.shadows <- urch.shadows[-which(id(urch.shadows) == "20120621_1" | id(urch.shadows) == "20120621_2" | id(urch.shadows) == "20120621_3" | 
                                      id(urch.shadows) == "20120621_4" | id(urch.shadows) == "20120621_5" | id(urch.shadows) == "20120621_6" | 
                                      id(urch.shadows) == "20120622_4" | id(urch.shadows) == "20120622_5" | id(urch.shadows) == "20120626_5" | 
                                      id(urch.shadows) == "20120626_6" | id(urch.shadows) == "20120627_9" | id(urch.shadows) == "20120629_12")]

# We iteratively calculate D for all individual sea urchins
# D=distance between the two points representing the first and last position of the path
tot.D <- NULL
for(i in 1:length(urch.shadows)){
  D <- sqrt((urch.shadows[[i]]$x[length(urch.shadows[[i]]$x)] - urch.shadows[[i]]$x[1])^2 + 
              (urch.shadows[[i]]$y[length(urch.shadows[[i]]$y)]-urch.shadows[[i]]$y[1])^2)
  tot.D[i] <- D 
}

# We iteratively calculate the length of each movement step for all individual sea urchins, this is to get L, from straightness index formula.
# L=the path of each individual broken into sections per minute of locomotion. The sum of all sections (length of path) is L.
graella.t1 <- NULL
for(i in 1:length(urch.shadows)){
  dist <- urch.shadows[[i]]$dist
  nom <- i
  tot <- data.frame(dist, rep(nom, length(urch.shadows[[i]]$x)))
  graella.t1 <- rbind(graella.t1, tot)
}
names(graella.t1) <- c("dist", "id")
sum.Lt1 <- tapply(graella.t1$dist, graella.t1$id, sum, na.rm = T)

# We now calculate the mean for each sea urchin (given that shadows experiment, involved 3 trials for each sea urchin)
indiv <- c(1,1,1,2,3,3,3,4,5,5,6,9,6,6,7,7,7,8,8,10,10,10,11,11,11,12,12,12,13,16,16,13,13,14,14,14,15,15,15)
taula <- data.frame(as.numeric(sum.Lt1), indiv, tot.D)
sum.Lt1.indiv <- as.numeric(tapply(taula$as.numeric.sum.Lt1., taula$indiv, mean))
tot.D.indiv <- as.numeric(tapply(taula$tot.D, taula$indiv, mean))

# We finally calculate straightness index
tortuosity.shadows <- tot.D.indiv/sum.Lt1.indiv


# # # 
# Testing straightness for each experiment ----
# # #

experiment <- c(rep("null", 29), rep("predator", 21), rep("shadow", 16))
dades <- data.frame(experiment, c(tortuosity.null, tortuosity.pred, tortuosity.shadows))
names(dades) <- c("exp", "tortuosity")
dades$asin.tort <- asin(dades$tortuosity)
boxplot(tortuosity~exp, data = dades)
boxplot(asin.tort~exp, data = dades)
model1 <- lm(tortuosity~exp, data = dades)
anova(model1)
# Analysis of Variance Table
# Response: tortuosity
#           Df Sum Sq  Mean Sq F value  Pr(>F)  
# exp        2 0.5621 0.281051  4.8612 0.01088 *
# Residuals 63 3.6424 0.057815                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
TukeyHSD(aov(lm(tortuosity~exp, data = dades)))

model2 <- lm(asin.tort~exp, data = dades)
anova(model2)
# Analysis of Variance Table
# Response: asin.tort
#           Df Sum Sq Mean Sq F value   Pr(>F)   
# exp        2 1.1603 0.58014  5.3031 0.007437 **
# Residuals 63 6.8920 0.10940                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
TukeyHSD(aov(lm(asin.tort~exp, data = dades)))
# plot(model2) # OK! Certa heterogeneitat, però OK!

# Barplotting it
mitjana <- tapply(dades$tortuosity, dades$exp, mean)
error <- tapply(dades$tortuosity, dades$exp, std.error)

bp <- barplot(mitjana, ylim = c(0,1), ylab = "Tortuosity")
arrows(bp, mitjana, bp, mitjana + error,  lwd = 1.5, angle = 90, length = 0.1)
arrows(bp, mitjana, bp, mitjana - error,  lwd = 1.5, angle = 90, length = 0.1)
text(x = bp, y = mitjana + 0.15, labels = c("a", "b", "a"))


# # # 
# CALCULATING VELOCITY ----
# # # 

# # # 
# Velocity for sea urchins from the controls ----
# # #

load("RData/urch.null.RData")

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.null <- urch.null[-which(id(urch.null) == "20120528_4" | id(urch.null) == "20120528_5" | id(urch.null) == "20120528_6" | id(urch.null) == "20120607_5")]

# Calculating velocity
mean.speed.null <- NULL
for(i in 1:length(urch.null)){
  mean.speed.null[i] <- mean(urch.null[[i]]$dist/0.5, na.rm = T) # velocity is in píxels . minut-1
}


# # # 
# Velocity for sea urchins with predator cues ----
# # #

load("RData/urch.pred.RData")

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.pred <- urch.pred[-which(id(urch.pred) == "20120613_12" | id(urch.pred) == "20120614_8")]

# Calculating velocity
mean.speed.pred <- NULL
for(i in 1:length(urch.pred)){
  mean.speed.pred[i] <- mean(urch.pred[[i]]$dist/0.5, na.rm = T) # en píxels per minut
}


# # # 
# Velocity for sea urchins from the shadows treatment ----
# # #

load("RData/urch.shadows.RData")

# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.shadows <- urch.shadows[-which(id(urch.shadows) == "20120621_1" | id(urch.shadows) == "20120621_2" | id(urch.shadows) == "20120621_3" |
                                      id(urch.shadows) == "20120621_4" | id(urch.shadows) == "20120621_5" | id(urch.shadows) == "20120621_6" |
                                      id(urch.shadows) == "20120622_4" | id(urch.shadows) == "20120622_5" | id(urch.shadows) == "20120626_5" |
                                      id(urch.shadows) == "20120626_6" | id(urch.shadows) == "20120627_9" | id(urch.shadows) == "20120629_12")]

# Calculating velocity
mean.speed.shadows <- NULL
for(i in 1:length(urch.shadows)){
  mean.speed.shadows[i] <- mean(urch.shadows[[i]]$dist/0.5, na.rm = T) # en píxels per minut
}

# We now calculate the mean for each sea urchin (given that shadows experiment, involved 3 trials for each sea urchin)
indiv <- c(1,1,1,2,3,3,3,4,5,5,6,9,6,6,7,7,7,8,8,10,10,10,11,11,11,12,12,12,13,16,16,13,13,14,14,14,15,15,15)
taula <- data.frame(mean.speed.shadows, indiv)
mean.speed.shadows.indiv <- as.numeric(tapply(taula$mean.speed.shadows, taula$indiv, mean))


# # # 
# Testing velocity for each experiment ----
# # #

experiment <- c(rep("null", 29), rep("predator", 21), rep("shadow", 16))
dades <- data.frame(experiment, c(mean.speed.null, mean.speed.pred, mean.speed.shadows.indiv))
names(dades) <- c("exp", "mean.speed")
boxplot(mean.speed~exp, data = dades) # We can see 2 outliers

# Modelling velocity vs. experiment, without deleting outliers
model <- lm(mean.speed~exp, data = dades)
anova(model) 
# Analysis of Variance Table
# Response: mean.speed
#           Df  Sum Sq Mean Sq F value   Pr(>F)   
# exp        2  3158.3 1579.14  7.4224 0.001275 **
# Residuals 63 13403.4  212.75                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

# Modelling velocity vs. experiment, deleting outliers
dades <- dades[c(-44,-52),]
boxplot(mean.speed~exp, data = dades)
model1 <- lm(mean.speed~exp, data = dades)
anova(model1) 
# Analysis of Variance Table
# Response: mean.speed
#           Df  Sum Sq Mean Sq F value    Pr(>F)    
# exp        2  3614.5 1807.26  10.092 0.0001636 ***
# Residuals 61 10923.9  179.08                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# plot(model1) # Looks good

# Barplotting velocities
dades$mean.speed.cm <- dades$mean.speed*0.1834 # els 0.1834 és l'equivalència pixel-cm. Ho he fet amb l'image J i el diàmetre piscina.
mitjana <- tapply(dades$mean.speed.cm, dades$exp, mean)
error <- tapply(dades$mean.speed.cm, dades$exp, std.error)
bp <- barplot(mitjana, ylim = c(0,13), ylab = "Mean speed (cm/minute)")
arrows(bp, mitjana, bp, mitjana + error,  lwd = 1.5, angle = 90, length = 0.1)
arrows(bp, mitjana, bp, mitjana - error,  lwd = 1.5, angle = 90, length = 0.1)
text(x = bp, y = mitjana + 1.5, labels = c("a", "b", "a"))


