# # # # # # # # # # # # # # # # #  #
#     Testing light conditions     #
#         in the different         #
#     experimental treatments      #
#                                  #
#          Jordi F. Pagès          #
#          September 2020          #
# # # # # # # # # # # # # # # # #  # 


library(tidyverse)
library(ggsci)
library(cowplot)

source("qmomentsFunctions.R")

# Loading the data set
light <- read.table(file = "RData/Light_conditions.txt", header = T)

# We filter the data set to include only data from the experiment with homogeneous light
light <- light %>% 
  filter(Experiment == 1) %>% 
  mutate(Site = droplevels(Site))

# Checking the effect of location (in the aquarium) on light conditions
m.lloc <- lm(PAR~Site, data = light)
car::Anova(m.lloc)
mcheck(m.lloc)

# Post-hoc analysis
m.lloc2 <- aov(PAR~Site, data = light)
tukey <- TukeyHSD(m.lloc2) 
plot(tukey)

# Barplotting the data
light %>% 
  group_by(Site) %>% 
  summarise(mean = mean(PAR),
            std.error = std.error(PAR)) %>% 
  ggplot(aes(y = mean, x = Site)) +
  geom_bar(aes(fill = Site),stat = "identity") +
  scale_fill_d3(palette = "category20") +
  scale_x_discrete(labels = c("0º", "centre", "90º", "270º", "180º")) +
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2) +
  geom_text(aes(y = mean + std.error, x = Site, label = c("a", "b", "a", "a", "c")), nudge_y = 1) +
  xlab("") +
  ylab(expression(paste("PAR (µmol photons s"^"-1"," m"^-2,")"))) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave2(filename = "Figs/barplot_light.pdf")


# Boxplotting
maxlight <- light %>% 
  group_by(Site) %>% 
  summarise(max = max(PAR))

light %>% 
  ggplot(aes(y = PAR, x = Site)) +
  geom_boxplot(aes(fill = Site)) +
  scale_fill_d3(palette = "category20") +
  scale_x_discrete(labels = c("0º", "centre", "90º", "270º", "180º")) +
  geom_text(data = maxlight, aes(y = max, x = Site, label = c("a", "b", "a", "a", "c")), nudge_y = 0.5) +
  xlab("") +
  ylab(expression(paste("PAR (µmol photons s"^"-1"," m"^-2,")"))) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave2(filename = "Figs/boxplot_light.pdf")












# Mean light irradiance
#   E      mid        N       S       W 
# 7.4      9.3      7.5     7.7     5.1 

# Std error of light irradiance
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




