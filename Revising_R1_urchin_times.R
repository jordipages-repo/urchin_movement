mean.speed.null$Treatment <- "control"
mean.speed.pred$Treatment <- "predators"

a <- rbind(mean.speed.null, mean.speed.pred)


ggplot(a, aes(x = Treatment, y = mean.speed)) +
  geom_violin() +
  geom_boxplot() 


library(tidyverse)
control <- read_csv("control_times.csv")
predators <- read_csv("pred_times.csv")

tot <- bind_rows(control, predators)
tot$time <- c(control$`difftime(min)`, predators$`difftime (min)`)
summary(tot$time)
mean(tot$time)
std.error(tot$time)
