library(tidyverse)
library(dplyr)
library(ggsci)
library(cowplot)

# Running other scripts before plotting:
source("urchins_tortuosity_classic.R")
rm(list = setdiff(ls(), c("tort", "speed")))
source("urchins_q_moments.R")
rm(list = setdiff(ls(), c("tort", "speed", "fin")))

# Running script with some of the functions needed
source("qmomentsFunctions.R")

# Adding labels to each data set
fin$labels <- ifelse(fin$treatment== "Control", "a", "b")
speed$labels <- ifelse(speed$exp== "Control", "a", "b")
tort$labels <- ifelse(tort$exp== "Control", "a", "b")
  

# # # 
# Boxplots ----
# # # 

box.straightness <- ggplot(tort, aes(x = exp, y = tortuosity)) +
  geom_boxplot(aes(fill = exp)) +
  geom_text(aes(y = max(tortuosity) + 0.1, label = labels), check_overlap = T) +
  scale_fill_d3(palette = "category20") +
  scale_y_continuous(breaks = c(0,0.25,0.5, 0.75,1), labels = c("0.00","0.25","0.50","0.75", "1.00")) +
  coord_cartesian(ylim = c(0,1.1)) +
  # scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  xlab("") +
  ylab("Straightness index") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))

box.velocity <- ggplot(speed, aes(x = exp, y = mean.speed.cm)) +
  geom_boxplot(aes(fill = exp)) +
  geom_text(aes(y = max(mean.speed.cm) + 1, label = labels), check_overlap = T) +
  scale_fill_d3(palette = "category20") +
  # scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  xlab("") +
  ylab(expression(paste("Mean speed (cm minute"^"-1",")"))) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))

box.exponents <- ggplot(fin, aes(x = treatment, y = coef)) +
  geom_boxplot(aes(fill = treatment)) +
  geom_text(aes(y = max(coef) + 0.1, label = labels), check_overlap = T) +
  scale_fill_d3(palette = "category20") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 1, lty = 3) +
  scale_y_continuous(breaks = c(0,0.25,0.5, 0.75,1), labels = c("0.00","0.25","0.50","0.75", "1.00")) +
  # scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  xlab("") +
  ylab(expression(paste(zeta,"(q)"))) +
  coord_cartesian(ylim = c(0,1.1)) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))

plot_grid(box.straightness, box.velocity, box.exponents, ncol = 1, align = 'v', labels = "AUTO")
# ggsave2("Figs/Cowplot_boxplots.pdf")
# ggsave2("Figs/Cowplot_boxplots.pdf", width = 130, height = 300, units = "mm")


# # # 
# Barplots----
# # # 
bar.straightness <- tort %>% 
  group_by(exp) %>% 
  summarise(mean = mean(tortuosity),
            std = std.error(tortuosity),
            labels = first(labels)) %>% 
  ggplot(aes(x = exp, y = mean)) +
  geom_bar(aes(fill = exp), stat = "identity") +
  geom_errorbar(aes(ymin = mean-std, ymax = mean + std), width = 0.1) +
  geom_text(aes(y = (mean + std) + 0.1, label = labels)) +
  scale_fill_d3(palette = "category20") +
  # scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0,1)) +
  xlab("") +
  ylab("Straightness index") +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))

bar.velocity <- speed %>% 
  group_by(exp) %>% 
  summarise(mean = mean(mean.speed.cm),
            std = std.error(mean.speed.cm),
            labels = first(labels)) %>% 
  ggplot(aes(x = exp, y = mean)) +
  geom_bar(aes(fill = exp), stat = "identity") +
  geom_errorbar(aes(ymin = mean-std, ymax = mean + std), width = 0.1) +
  geom_text(aes(y = (mean + std) + 1, label = labels)) +
  scale_fill_d3(palette = "category20") +
  # scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  xlab("") +
  ylab(expression(paste("Mean speed (cm minute"^"-1",")"))) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))


bar.exponents <- fin %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(coef),
            std = std.error(coef),
            labels = first(labels)) %>% 
  ggplot(aes(x = treatment, y = mean)) +
  geom_bar(aes(fill = treatment), stat = "identity") +
  geom_errorbar(aes(ymin = mean-std, ymax = mean + std), width = 0.1) +
  geom_text(aes(y = (mean + std) + 0.1, label = labels)) +
  scale_fill_d3(palette = "category20") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 1, lty = 3) +
  # scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  xlab("") +
  ylab(expression(paste(zeta,"(q)"))) +
  # coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))

plot_grid(bar.straightness, bar.velocity, bar.exponents, ncol = 1, align = 'v', labels = "AUTO")
# ggsave2("Figs/Cowplot_barplots.pdf")
# ggsave2("Figs/Cowplot_barplots.pdf", width = 130, height = 300, units = "mm")
