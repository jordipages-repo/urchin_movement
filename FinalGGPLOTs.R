# Running other scripts before plotting:
source("urchins_tortuosity_classic.R")
rm(list = setdiff(ls(), c("tort", "speed")))
# detach("package:dplyr")
source("urchins_q_moments.R")
rm(list = setdiff(ls(), c("tort", "speed", "fin", "listExp.urch.null", "listExp.urch.pred")))

# Running script with some of the functions needed
source("qmomentsFunctions.R")

library(tidyverse)
library(dplyr)
library(ggsci)
library(cowplot)

# Adding labels to each data set
fin$labels <- ifelse(fin$treatment== "Control", "a", "b")
speed$labels <- ifelse(speed$exp== "Control", "a", "b")
tort$labels <- ifelse(tort$exp== "Control", "a", "b")
  

# # # 
# Boxplots. Comparing all response variables ----
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
# VIOLINplots. Comparing all response variables ----
# # # 
violin.straightness <- ggplot(tort, aes(x = exp, y = tortuosity)) +
  geom_violin(aes(fill = exp)) +
  geom_boxplot(width=0.05, outlier.colour = "transparent") +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2), colour = "#404040") +
  # stat_summary(fun=median, geom="point", size=3, color="black", pch = 15) +
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

violin.velocity <- ggplot(speed, aes(x = exp, y = mean.speed.cm)) +
  geom_violin(aes(fill = exp)) +
  geom_boxplot(width=0.05, outlier.colour = "transparent") +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2), colour = "#404040") +
  # stat_summary(fun=median, geom="point", size=3, color="black", pch = 15) +
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

violin.exponents <- ggplot(fin, aes(x = treatment, y = coef)) +
  geom_violin(aes(fill = treatment)) +
  geom_boxplot(width=0.05, outlier.colour = "transparent") +
  geom_jitter(position = position_jitter(seed = 1, width = 0.2), colour = "#404040") +
  # stat_summary(fun=median, geom="point", size=3, color="black", pch = 15) +
  geom_text(aes(y = max(coef) + 0.05, label = labels), check_overlap = T) +
  scale_fill_d3(palette = "category20") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 1, lty = 3) +
  scale_y_continuous(breaks = c(0,0.25,0.5, 0.75,1), labels = c("0.00","0.25","0.50","0.75", "1.00")) +
  # scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  xlab("") +
  ylab(expression(paste(zeta,"(q)"))) +
  coord_cartesian(ylim = c(0.5,1.05)) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))

plot_grid(violin.straightness, violin.velocity, violin.exponents, ncol = 1, align = 'v', labels = "AUTO")
# ggsave2("Figs/Cowplot_violinplots.pdf")
ggsave2("Figs/Cowplot_violinplots.pdf", width = 130, height = 300, units = "mm")


# # # 
# Barplots. Comparing all response variables ----
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




# # # 
# GGPLOTTING INDIVIDUAL EXPONENTS URCH.NULL ----
# # # 
qs <- seq(from = 0, to = 8, by=1)
q1 <- as_tibble(listExp.urch.null) %>% 
  mutate(qs = rep(qs, length(unique(listExp.urch.null$ID)))) %>%
  left_join(select(fin, coef, ID), by = "ID") %>% 
  ggplot() + 
  geom_line(aes(x = qs, y = exponents, group = ID, alpha = coef), colour = "#1F77B4FF", lwd = 0.6) +
  scale_alpha(range = c(0.2,1)) + 
  # geom_line(aes(x = qs, y = exponents, group = ID, alpha = ID), colour = "#1F77B4FF", lwd = 0.6) +
  # geom_line(aes(x = qs, y = exponents, colour = ID)) +
  # geom_line(aes(x = qs, y = exponents, group = ID), alpha = 0.33, lwd = 0.6) +
  # geom_line(aes(x = qs, y = qs), colour = "#1F77B4FF", lwd = 2, alpha = 0.7) +
  # geom_line(aes(x = qs, y = qs/2),  colour = "#FF7F0EFF", lwd = 2, alpha = 0.7) +
  geom_line(aes(x = qs, y = qs), lwd = 1, lty = 3) +
  geom_line(aes(x = qs, y = qs/2), lwd = 1, lty = 2) +
  xlab("q") +
  ylab(expression(paste(zeta,"(q)"))) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
q1
# ggsave2(filename = "Figs/indiv.exponents.controls.pdf")


# # # 
# GGPLOTTING INDIVIDUAL EXPONENTS URCH.PRED ----
# # # 

q2 <- as_tibble(listExp.urch.pred) %>% 
  mutate(qs = rep(qs, length(unique(listExp.urch.pred$ID)))) %>% 
  left_join(select(fin, coef, ID), by = "ID") %>% 
  ggplot() + 
  geom_line(aes(x = qs, y = exponents, group = ID, alpha = coef), colour = "#FF7F0EFF", lwd = 0.6) +
  scale_alpha(range = c(0.2,1)) + 
  # geom_line(aes(x = qs, y = exponents, group = ID, alpha = ID), colour = "#1F77B4FF", lwd = 0.6) +
  # geom_line(aes(x = qs, y = exponents, colour = ID)) +
  # geom_line(aes(x = qs, y = exponents, group = ID), alpha = 0.33, lwd = 0.6) +
  # geom_line(aes(x = qs, y = qs), colour = "#1F77B4FF", lwd = 2, alpha = 0.7) +
  # geom_line(aes(x = qs, y = qs/2),  colour = "#FF7F0EFF", lwd = 2, alpha = 0.7) +
  geom_line(aes(x = qs, y = qs), lwd = 1, lty = 3) +
  geom_line(aes(x = qs, y = qs/2), lwd = 1, lty = 2) +
  xlab("q") +
  ylab(expression(paste(zeta,"(q)"))) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))
q2
# ggsave2(filename = "Figs/indiv.exponents.predators.pdf")




# # #
# PANEL PLOT OF TRAJECTORIES AND CORRESPONDING QMOMENTS
# # #
source("Trajectory_visualisation.R")

plot_grid(p1, q1, p2, q2, ncol = 2, nrow = 2, labels = "AUTO")
# ggsave("Figs/trajectoriesANDqmomentsALPHAforQExponents.pdf", width = 10.11, height = 7.89, units = "in")
