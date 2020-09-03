# # # # # # # # # # # # # # # # #  #
#     Testing light conditions     #
#         in the different         #
#     experimental treatments      #
#                                  #
#          Jordi F. Pagès          #
#          September 2020          #
# # # # # # # # # # # # # # # # #  # 


library(tidyverse)

# Loading the data set
light <- read.table(file = "RData/Light_conditions.txt", header = T)
light <- light %>% 
  filter(Experiment == 1) %>% 
  mutate(Site = droplevels(light$Site))

# Checking the effect of location (in the aquarium) on light conditions
m.lloc <- lm(PAR~Site, data = light)
anova(m.lloc)

# Post-hoc analysis
m.lloc2 <- aov(PAR~Site, data = light)
tukey <- TukeyHSD(m.lloc2) 
plot(tukey)

# Barplotting the data
std.error <- function(x, na.rm = FALSE) {
  sd(x, na.rm = T)/sqrt(length(x))
}
mitj <- tapply(light$PAR, light$Site, mean)
std <- tapply(light$PAR, light$Site, std.error)
bp <- barplot(mitj, ylim = c(0,11), ylab = "PAR (mmols photons s)")
arrows(bp, mitj, bp, mitj + std,  lwd = 1.5, angle = 90, length = 0.1)
arrows(bp, mitj, bp, mitj - std,  lwd = 1.5, angle = 90, length = 0.1)
text(x = bp, y = mitj + 1, labels = c("a", "b", "a", "a", "c"))

# Mean light irradiance
mitj
#   E      mid        N       S       W 
# 7.4      9.3      7.5     7.7     5.1 

# Std error of light irradiance
std
#   E      mid        N       S       W 
# 0.3      0.4      0.5     0.4     0.3 

# Difference in light between the area with max and each position
#  E      mid        N       S       W  
# 20.3    0.0     19.0    17.6    45.1
# West area had 45% less irradiance compared to the middle of the pool.

# Difference in light between the mean irradiance in the arena, and each position
#  E      mid        N       S       W  
# -0.1   -25.6    -1.8    -3.5    31.1 
# West area had 31% less irradiance than the mean irradiance of the arena.




