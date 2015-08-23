#  GGPLOT double plots

data("ToothGrowth")

library(dplyr)
library(ggplot2)

se <- function(x) sqrt(var(x)/length(x))

tg <- ToothGrowth %>%
    group_by(dose, supp) %>%
    summarise_each(funs(mean, sd, se)) %>%
    rename(tg_mean = mean, tg_sd = sd, tg_se = se)

pl1 <- ggplot(ToothGrowth, aes(factor(dose), y=len, fill = supp)) +
    geom_boxplot(stat="boxplot", outlier.colour = "blue", notch = FALSE) +
    ggtitle("ToothGrowth data: length vs dose, given type of supplement")
#print(pl1)


tg_limits <- aes(ymax = tg_mean + tg_se, ymin = tg_mean - tg_se)
pl2 <- ggplot(tg, aes(supp, tg_mean, colour=dose)) +
    geom_point(stat="identity", size=3) +
    geom_errorbar( tg_limits, width = 0.1) +
    ggtitle("ToothGrowth data: length vs dose, given type of supplement") +
    facet_grid(.~dose)
#print(pl2)

pl1 <- ggplot(ToothGrowth, aes(x = dose, y=len, color = supp)) +
    geom_point() +
    scale_x_log10() +
    ggtitle("ToothGrowth data: length vs dose, given type of supplement")
print(pl1)

