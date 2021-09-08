library(tidyverse)
library(ggsci)
library(ggrepel)
library(cowplot)

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
# unique(urch.null.MAT$ID)[10]

# Getting the data needed
graella <- loglogdata(urch.null.MAT, unique(urch.null.MAT$ID)[9])

tgraella <- as_tibble(graella)
tgraella

p5 <- tgraella %>% 
  group_by(tau) %>% 
  summarise(`mean_q = 0` = mean(q0),
            `mean_q = 1` = mean(q1),
            `mean_q = 2` = mean(q2),
            `mean_q = 3` = mean(q3),
            `mean_q = 4` = mean(q4),
            `mean_q = 5` = mean(q5),
            `mean_q = 6` = mean(q6),
            `mean_q = 7` = mean(q7),
            `mean_q = 8`= mean(q8)) %>%
  pivot_longer(cols = starts_with("mean")) %>% 
  mutate(label = if_else(tau == max(tau), str_split(string = as.character(name), pattern = "_", simplify = TRUE)[,2], NA_character_)) %>%
  ggplot(aes(x = log10(tau), y = log10(value))) +
    geom_point(aes(colour = name), alpha = 0.2) +
    geom_smooth(method = "lm", aes(group = name, colour = name), lwd = 0.5, se = F) + 
    geom_text_repel(aes(label = label), nudge_x = 0.15, na.rm = TRUE, size = 5, segment.color = NA) +
    scale_colour_d3("category20") +
    coord_cartesian(xlim = c(0.5,2.5)) + 
    xlab(expression(log(tau))) + 
    # ylab(expression(paste("log(<", Delta, "X", tau^q, ">)"))) +
    ylab("") +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          text = element_text(size = 18))
# ggsave(filename = "Documents/FEINA/POSTDOCTORAT/ARTICLES/Moviment_Garotes_Fede/WORDS/Figures_finals/Fig.S3_new.pdf",  width = 9, height = 8)


exponents <- qmom1(urch.null.MAT, unique(urch.null.MAT$ID)[9])

p6 <- ggplot(data = exponents) + 
  geom_line(aes(x = num, y = exponents), colour = "#1F77B4FF", lwd = 0.6) +
  geom_line(aes(x = num, y = num), lwd = 1, lty = 3) +
  geom_line(aes(x = num, y = num/2), lwd = 1, lty = 2) +
  xlab("q") +
  # ylab(expression(paste(zeta,"(q)"))) +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))


p4 <- ggplot(data = urch.null.MAT %>% filter(ID == unique(urch.null.MAT$ID)[9])) +
  geom_path(aes(x = x, y = y), colour = "#1F77B4FF") +
  ylab("") +
  coord_cartesian(xlim = c(0,1800), ylim = c(0,1300)) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))


granplot1 <- plot_grid(p1, p2, p3, ncol = 1, align = "v", labels = c("A", "C", "E"))
granplot2 <- plot_grid(p4, p5, p6, ncol = 1, align = "v", labels = c("B", "D", "F"))

plot_grid(granplot1, granplot2, ncol = 2)
ggsave2(filename = "Figs/Revision_loglogPlots.png", width = 9.5, height = 9)
