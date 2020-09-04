library(tidyverse)
library(dplyr)
library(ggsci)
library(cowplot)

rm(list = setdiff(ls(), c("tort", "speed", "fin")))
std.error <- function(x, na.rm = FALSE) {
  sd(x, na.rm = T)/sqrt(length(x))
}

# 
# fin.long <- fin %>% 
#   rename(exp = treatment) %>% 
#   select(exp, coef) %>% 
#   pivot_longer(cols = coef, names_to = "variable", values_to = "value")
#   
# speed.long <- speed %>% 
#   select(exp, mean.speed.cm) %>% 
#   pivot_longer(cols = mean.speed.cm, names_to = "variable", values_to = "value")
# 
# tort.long <- tort %>% 
#   select(exp, tortuosity) %>% 
#   pivot_longer(cols = tortuosity, names_to = "variable", values_to = "value")
# 
# all.long <- bind_rows(fin.long, speed.long, tort.long)
  
# # # 
# Boxplots ----
# # # 

box.straightness <- ggplot(tort, aes(x = exp, y = tortuosity)) +
  geom_boxplot(aes(fill = exp)) +
  scale_fill_d3(palette = "category20") +
  coord_cartesian(ylim = c(0,1)) +
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
  scale_fill_d3(palette = "category20") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 1, lty = 3) +
  # scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  xlab("") +
  ylab(expression(paste(zeta,"(q)"))) +
  coord_cartesian(ylim = c(0,1)) +
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
            std = std.error(tortuosity)) %>% 
  ggplot(aes(x = exp, y = mean)) +
  geom_bar(aes(fill = exp), stat = "identity") +
  geom_errorbar(aes(ymin = mean-std, ymax = mean + std), width = 0.1) +
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
            std = std.error(mean.speed.cm)) %>% 
  ggplot(aes(x = exp, y = mean)) +
  geom_bar(aes(fill = exp), stat = "identity") +
  geom_errorbar(aes(ymin = mean-std, ymax = mean + std), width = 0.1) +
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
            std = std.error(coef)) %>% 
  ggplot(aes(x = treatment, y = mean)) +
  geom_bar(aes(fill = treatment), stat = "identity") +
  geom_errorbar(aes(ymin = mean-std, ymax = mean + std), width = 0.1) +
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
# ggsave2("Figs/Cowplot_boxplots.pdf")
# ggsave2("Figs/Cowplot_barplots.pdf", width = 130, height = 300, units = "mm")
