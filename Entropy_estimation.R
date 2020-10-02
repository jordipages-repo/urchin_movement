# # # # # # # # # # # # # # # # # # # # # # # # # #
#   Estimating entropy of individual trajectories #
#            to measure unpredictability          #
#                                                 #
#                  Jordi F. Pagès                 #
#                  September 2020                 # 
# # # # # # # # # # # # # # # # # # # # # # # # # # 
library(adehabitatLT)
library(entropy)
# # Mock trial
# y <- c(4, 2, 3, 0, 2, 4, 0, 0, 2, 1, 1)
# y <- c(1000,0,0,0,0,0,0)
# entropy(y, method="ML")

source("qmomentsFunctions.R")

# # # 
# LOADING LTRAJ DATA FOR CONTROL URCHINS -----
# # # 

# We want a single data.frame with all the data with the x, y coordinates
load("RData/urch.null.RData")
summary(urch.null)
urch.null.MAT <- NULL
for(i in 1:length(urch.null)){
  x <- urch.null[[i]]$x
  y <- urch.null[[i]]$y
  dist <- urch.null[[i]]$dist
  R2n <- urch.null[[i]]$R2n
  abs.angle <- urch.null[[i]]$abs.angle
  rel.angle <- urch.null[[i]]$rel.angle
  ID <- rep(id(urch.null)[i], length(x))
  time <- urch.null[[i]]$date
  matriu <- data.frame(x, y, ID, time, dist, R2n, abs.angle, rel.angle)
  urch.null.MAT <- rbind(urch.null.MAT, matriu)
}
# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.null.MAT <- subset(urch.null.MAT, ID != "20120528_4" & ID != "20120528_5" & ID != "20120528_6" & ID != "20120607_5")
urch.null.MAT$ID <- droplevels(urch.null.MAT$ID)


# # # 
# LOADING LTRAJ DATA FOR URCHINS WITH PREDATOR CUES -----
# # # 

# We now study the predator cues treatment = predator cues
# We want a single data.frame with all the data with the x, y coordinates
load("RData/urch.pred.RData")
urch.pred.MAT <- NULL
for(i in 1:length(urch.pred)){
  x <- urch.pred[[i]]$x
  y <- urch.pred[[i]]$y
  dist <- urch.pred[[i]]$dist
  R2n <- urch.pred[[i]]$R2n
  abs.angle <- urch.pred[[i]]$abs.angle
  rel.angle <- urch.pred[[i]]$rel.angle
  ID <- rep(id(urch.pred)[i], length(x))
  time <- urch.pred[[i]]$date
  matriu <- data.frame(x, y, ID, time, dist, R2n, abs.angle, rel.angle)
  urch.pred.MAT <- rbind(urch.pred.MAT, matriu)
}
# We delete some urchins that for different reasons had problems (e.g. because they were not healthy, 
# because the automatic image detection method [in Matlab] produced many errors, etc.)
urch.pred.MAT <- subset(urch.pred.MAT, ID != "20120613_12" & ID != "20120614_8")
urch.pred.MAT$ID <- droplevels(urch.pred.MAT$ID)



# # #
# POPULATION-LEVEL ENTROPY ----
# # # 

library(tidyverse)

# Population-level entropy - control urchins
x_binned_urch.null <- urch.null.MAT %>% 
  mutate(bin.x = cut(x = x, 
                     breaks = seq(from = min(x, na.rm = T), to = max(x, na.rm = T) + 4, by = 4), 
                     right = FALSE)) %>% 
  group_by(bin.x) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(bin.x)) %>% 
  print(n = Inf)
x_control_entropy <- entropy(x_binned_urch.null$n)

y_binned_urch.null <- urch.null.MAT %>% 
  mutate(bin.y = cut(x = y, 
                     breaks = seq(from = min(y, na.rm = T), to = max(y, na.rm = T) + 4, by = 4), 
                     right = FALSE)) %>%  
  group_by(bin.y) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(bin.y)) %>% 
  print(n = Inf)
y_control_entropy <- entropy(y_binned_urch.null$n)  

rel.angle_binned_urch.null <- urch.null.MAT %>% 
  mutate(bin.rel.angle = cut(x = rel.angle, 
                             breaks = seq(from = min(rel.angle, na.rm = T), to = max(rel.angle, na.rm = T) + 4, by = 0.05), 
                             right = FALSE)) %>% 
  group_by(bin.rel.angle) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(bin.rel.angle)) %>% 
  print(n = Inf)
rel.angle_control_entropy <- entropy(rel.angle_binned_urch.null$n)  


# Population-level entropy - predator cues
x_binned_urch.pred <- urch.pred.MAT %>% 
  mutate(bin.x = cut(x = x, 
                     breaks = seq(from = min(x, na.rm = T), to = max(x, na.rm = T) + 4, by = 4), 
                     right = FALSE)) %>% 
  group_by(bin.x) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(bin.x))
x_pred_entropy <- entropy(x_binned_urch.pred$n)  

y_binned_urch.pred <- urch.pred.MAT %>% 
  mutate(bin.y = cut(x = y, 
                     breaks = seq(from = min(y, na.rm = T), max(y, na.rm = T) + 4, by = 4),
                     right = FALSE)) %>% 
  group_by(bin.y) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(bin.y))
y_pred_entropy <- entropy(y_binned_urch.pred$n)  

rel.angle_binned_urch.pred <- urch.pred.MAT %>% 
  mutate(bin.rel.angle = cut(x = rel.angle, 
                             breaks = seq(from = min(rel.angle, na.rm = T), max(rel.angle, na.rm = T) + 4, by = 0.05),
                             right = FALSE)) %>% 
  group_by(bin.rel.angle) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(bin.rel.angle)) %>% 
  print(n = Inf)
rel.angle_pred_entropy <- entropy(rel.angle_binned_urch.pred$n)  



# # #
# INDIVIDUAL-LEVEL ENTROPY -> X POSITIONS ----
# # #

library(tidyverse)
library(dplyr)
library(ggsci)

# For control urchins
ids <- as.character(unique(urch.null.MAT$ID))
control_entropiesALL <- NULL
for(i in 1:length(ids)){
  counts <- urch.null.MAT %>% 
    filter(ID == ids[i]) %>% 
    mutate(bin.dist = cut(x, seq(min(x, na.rm = T), max(x, na.rm = T) + 4, 4), right = FALSE)) %>% 
    group_by(bin.dist) %>% 
    summarise(n = n()) %>% 
    filter(!is.na(bin.dist))
  entropies <- data.frame(ids[i], entropy(counts$n))
  control_entropiesALL <- rbind(control_entropiesALL, entropies)
}
control_entropiesALL

# For urchins with predator cues
ids <- as.character(unique(urch.pred.MAT$ID))
pred_entropiesALL <- NULL
for(i in 1:length(ids)){
  counts <- urch.pred.MAT %>% 
    filter(ID == ids[i]) %>% 
    mutate(bin.dist = cut(x, seq(min(x, na.rm = T), max(x, na.rm = T) + 4, 4), right = FALSE)) %>% 
    group_by(bin.dist) %>% 
    summarise(n = n()) %>% 
    filter(!is.na(bin.dist))
  entropies <- data.frame(ids[i], entropy(counts$n))
  pred_entropiesALL <- rbind(pred_entropiesALL, entropies)
}
pred_entropiesALL

# Plotting
control_entropiesALL$treatment <- "Control"
control_entropiesALL$treatment <- as.factor(control_entropiesALL$treatment)
pred_entropiesALL$treatment <- "Predators"
pred_entropiesALL$treatment <- as.factor(pred_entropiesALL$treatment)

x.all <- rbind(control_entropiesALL, pred_entropiesALL)

model <- lm(entropy.counts.n. ~ treatment, data = x.all)
car::Anova(model)
summary(model)
mcheck(model) # OK!

p1 <- x.all %>% 
  mutate(labels = ifelse(treatment == "Control", "a", "b")) %>% 
  ggplot(aes(x = treatment, y = entropy.counts.n.)) +
  geom_violin(aes(fill = treatment)) + 
  geom_boxplot(width = 0.1) + 
  scale_fill_d3(palette = "category20") +
  geom_text(aes(y = max(entropy.counts.n.) + 0.1, label = labels), check_overlap = T) +
  xlab("") +
  ylab("Entropy") +
  # ggtitle("Positions on the x axis") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/positionsX.entropy.pdf")



# # #
# INDIVIDUAL-LEVEL ENTROPY -> REL.ANGLE POSITIONS ----
# # #

library(tidyverse)
library(ggsci)
library(dplyr)

# For control urchins
ids <- as.character(unique(urch.null.MAT$ID))
control_entropiesALL <- NULL
for(i in 1:length(ids)){
  counts <- urch.null.MAT %>% 
    filter(ID == ids[i]) %>% 
    mutate(bin.rel.angle = cut(rel.angle, seq(min(rel.angle, na.rm = T), max(rel.angle, na.rm = T) + 4, 0.05), right = FALSE)) %>% 
    group_by(bin.rel.angle) %>% 
    summarise(n = n()) %>% 
    filter(!is.na(bin.rel.angle))
  entropies <- data.frame(ids[i], entropy(counts$n))
  control_entropiesALL <- rbind(control_entropiesALL, entropies)
}
control_entropiesALL

# For urchins with predator cues
ids <- as.character(unique(urch.pred.MAT$ID))
pred_entropiesALL <- NULL
for(i in 1:length(ids)){
  counts <- urch.pred.MAT %>% 
    filter(ID == ids[i]) %>% 
    mutate(bin.rel.angle = cut(rel.angle, seq(min(rel.angle, na.rm = T), max(rel.angle, na.rm = T) + 4, 0.05), right = FALSE)) %>% 
    group_by(bin.rel.angle) %>% 
    summarise(n = n()) %>% 
    filter(!is.na(bin.rel.angle))
  entropies <- data.frame(ids[i], entropy(counts$n))
  pred_entropiesALL <- rbind(pred_entropiesALL, entropies)
}
pred_entropiesALL

# Plotting
control_entropiesALL$treatment <- "Control"
control_entropiesALL$treatment <- as.factor(control_entropiesALL$treatment)
pred_entropiesALL$treatment <- "Predators"
pred_entropiesALL$treatment <- as.factor(pred_entropiesALL$treatment)

rel.angles.all <- rbind(control_entropiesALL, pred_entropiesALL)

model <- lm(entropy.counts.n. ~ treatment, data = rel.angles.all)
car::Anova(model)
summary(model)
mcheck(model) # OK!

p2 <- rel.angles.all %>% 
  mutate(labels = ifelse(treatment == "Control", "a", "b")) %>% 
  ggplot(aes(x = treatment, y = entropy.counts.n.)) +
  geom_violin(aes(fill = treatment)) + 
  geom_boxplot(width = 0.1) + 
  scale_fill_d3(palette = "category20") +
  geom_text(aes(y = max(entropy.counts.n.) + 0.1, label = labels), check_overlap = T) +
  xlab("") +
  ylab("Entropy") +
  # ggtitle("Positions on the x axis") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/rel.angle.entropy.pdf")


# Cowplot panels
library(cowplot)

plot_grid(p1, p2, ncol = 2, align = 'h', labels = "AUTO")
# ggsave2("Figs/Cowplot_entropy.pdf", width = 300, height = 150, units = "mm")



# # # 
# Checking relationship between qmoments and rel.angles entropy ----
# # #

detach("package:dplyr")
source("urchins_q_moments.R")

library(tidyverse)
library(ggsci)

names(x.all) <- c("ID", "x.entropy", "treatment")
x.all <- as_tibble(x.all)

names(rel.angles.all) <- c("ID", "rel.angles.entropy", "treatment")
rel.angles.all <- as_tibble(rel.angles.all)
fin

x.all %>% 
  left_join(rel.angles.all, by = "ID") %>% 
  left_join(fin, by = "ID") %>%
  # filter(treatment == "Control")
  # ggplot(aes(x = coef, y = x.entropy)) +
  ggplot(aes(x = coef, y = rel.angles.entropy)) +
  # geom_smooth(colour = "black", method = "lm", formula = y ~ exp(x)) +
  geom_smooth(colour = "black", span = 0.9) +
  # geom_smooth(aes(group = treatment, colour = treatment, fill = treatment), method = "lm", formula = y ~ exp(x)) +
  # geom_smooth(aes(group = treatment, colour = treatment, fill = treatment), span = 0.9) +
  geom_point(aes(colour = treatment)) + 
  scale_colour_d3("category20") +
  scale_fill_d3("category20") +
  xlab(expression(paste(zeta,"(q)"))) +
  ylab("Entropy") +
  theme_bw() +
  theme(legend.position = c(0.25,0.1),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
# ggsave("Figs/EntropyVScoefqmoments_by_treatments_loess2.pdf")


