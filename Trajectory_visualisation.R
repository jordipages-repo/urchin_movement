# # # # # # # # # # # # # # # # # # # # #
#   Visualisation of raw trajectories   #
#                                       #
#           Jordi F. Pagès              #
#           September 2020              # 
# # # # # # # # # # # # # # # # # # # # # 

library(adehabitatLT)
# library(tidyverse)

# # #
# LOADING DATA FOR CONTROL AND PREDATOR URCHINS ----
# # # 

# We want a single data.frame with all the data with the x, y coordinates
load("RData/urch.null.RData")
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
urch.null.MAT$ID <- droplevels(urch.null.MAT$ID)

# We now study the predator cues treatment = predator cues
# We want a single data.frame with all the data with the x, y coordinates
load("RData/urch.pred.RData")
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
urch.pred.MAT$ID <- droplevels(urch.pred.MAT$ID)



# # # 
# GGPPLOTS CONTROL URCHINS ----
# # # 
library(tidyverse)

# Facet plots per each individual sea urchin
ggplot(data = urch.null.MAT) +
  geom_path(aes(x = x, y = y)) +
  facet_wrap(~ID) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/facet_control_raw_paths.pdf")


# 1 plot for all sea urchins
ggplot(data = urch.null.MAT) +
  geom_path(aes(x = x, y = y, colour = ID)) +
  xlim(0, 1800) +
  ylim(0, 1300) +
  ggtitle("Control treatment (n = 29)") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/control_paths.pdf")



# # # 
# GGPPLOTS PREDATOR URCHINS ----
# # # 

# Facet plots per each individual sea urchin
ggplot(data = urch.pred.MAT) +
  geom_path(aes(x = x, y = y)) +
  facet_wrap(~ID) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/facet_predators_raw_paths.pdf")

# 1 plot for all sea urchins
ggplot(data = urch.pred.MAT) +
  geom_path(aes(x = x, y = y, colour = ID)) +
  xlim(0, 1800) +
  ylim(0, 1300) +
  ggtitle("Predator treatment (n = 21)") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/predators_raw_paths.pdf")





# # # 
# GGPLOT PREDATOR vs CONTROL SUBSAMPLING ----
# # # 
library(tidyverse)
library(cowplot)
library(ggsci)

p1 <- urch.null.MAT %>% 
  # filter(ID %in% sample(unique(ID), 21)) %>% 
  ggplot() +
  geom_path(aes(x = x, y = y, colour = ID)) +
  # geom_path(aes(x = x, y = y, alpha = ID), colour = "#1F77B4FF") +
  xlim(0, 1800) +
  ylim(0, 1300) +
  # ggtitle(label = "Control treatment", subtitle = "(subsample of n = 21)") +
  # ggtitle(label = "Control treatment", subtitle = "(n = 29)") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))

p2 <- ggplot(data = urch.pred.MAT) +
  geom_path(aes(x = x, y = y, colour = ID)) +
  # geom_path(aes(x = x, y = y, alpha = ID), colour = "#FF7F0EFF") +
  xlim(0, 1800) +
  ylim(0, 1300) +
  # ggtitle(label = "Predator treatment", subtitle = "(n = 21)") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))

plot_grid(p1, p2, ncol = 2, align = 'h', labels = "AUTO")
# ggsave2("Figs/Cowplot_controlsubsampleVSpredatorsOK.pdf", width = 300, height = 150, units = "mm")

