# # # # # # # # # # # # # # # # #  #
#     Testing light homo- or       #
#  heterogeneity in the different  #
#     experimental treatments      #
#                                  #
#          Jordi F. Pagès          #
#          September 2020          #
# # # # # # # # # # # # # # # # #  # 


library(tidyverse)

# Loading the data set
light <- read.table(file = "RData/Light_conditions.txt", header = T)

# Checking the effect of location (in the aquarium) on light conditions

# HOMOGENEOUS LIGHT EXPERIMENT -----
m.lloc <- lm(PAR~Site, data = light[which(light[,1] == 1), ])
summary(m.lloc)
anova(m.lloc)

# Post-hoc analysis
m.lloc2 <- aov(PAR~Site, data = light[which(light[,1] == 1), ])
tukey <- TukeyHSD(m.lloc2) 
plot(tukey)

# Barplotting the data
std.error <- function(x, na.rm = FALSE) {
  sd(x, na.rm = T)/sqrt(length(x))
}
mitj <- tapply(exp1$PAR, exp1$Site, mean)
std <- tapply(exp1$PAR, exp1$Site, std.error)
bp <- barplot(mitj, ylim = c(0,11), ylab = "PAR (mmols photons s)")
arrows(bp, mitj, bp, mitj + std,  lwd = 1.5, angle = 90, length = 0.1)
arrows(bp, mitj, bp, mitj - std,  lwd = 1.5, angle = 90, length = 0.1)
text(x = bp, y = mitj + 1, labels = c("a", "b", "a", "a", "c"))


# HETEROGENEOUS LIGHT EXPERIMENT (Shadows experiment) -----
m.heterolight <- lm(PAR~Condition, data = light[which(light[,1] == 2), ])
summary(m.heterolight)
anova(m.heterolight)

m.heterolight2 <- aov(PAR~Condition, data = light[which(light[,1] == 2), ])
tukey <- TukeyHSD(m.heterolight2) 
plot(tukey)

exp2 <- light[which(light[,1] == 2), ]
exp2$photons <- (exp2$PAR*6.022)/10
exp2$Condition <- as.character(exp2$Condition)
exp2$Condition <- as.factor(exp2$Condition)

# Barplotting the data 
std.error <- function(x, na.rm = FALSE) {
  sd(x, na.rm = T)/sqrt(length(x))
}
mitj <- tapply(exp2$photons, exp2$Condition, mean)
std <- tapply(exp2$photons, exp2$Condition, std.error)
bp <- barplot(mitj, ylim = c(0,8), ylab = "PAR (x 1018 photons m-2 s-1)")
arrows(bp, mitj, bp, mitj + std,  lwd = 1.5, angle = 90, length = 0.1)
arrows(bp, mitj, bp, mitj - std,  lwd = 1.5, angle = 90, length = 0.1)
text(x = bp, y = mitj + 1, labels = c("a", "a", "b"))


# Means for each treatment
light %>% 
  filter(Site != "Nmid") %>%
  group_by(Condition) %>% 
  summarise(light = mean(PAR))



