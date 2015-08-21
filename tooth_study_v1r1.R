# teeth study

# 1. Load the ToothGrowth data and perform some basic exploratory data analyses
# 2. Provide a basic summary of the data.
# 3. Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class
# 4. State your conclusions and the assumptions needed for your conclusions.
#
# Some criteria that you will be evaluated on
# * Did you  perform an exploratory data analysis of at least a single plot or table highlighting basic features of the data?
# * Did the student perform some relevant confidence intervals and/or tests?
# * Were the results of the tests and/or intervals interpreted in the context of the problem correctly?
# * Did the student describe the assumptions needed for their conclusions?

data("ToothGrowth")

library(dplyr)
library(ggplot2)
library(knitr)

se <- function(x) sqrt(var(x)/length(x))

tg <- ToothGrowth %>%
    group_by(dose, supp) %>%
    summarise_each(funs(mean, sd, se)) %>%
    rename(tg_mean = mean, tg_sd = sd, tg_se = se)

kable(summary(tg))


pl1 <- ggplot(ToothGrowth, aes(factor(dose), y=len, fill = supp)) +
    geom_boxplot(stat="boxplot", outlier.colour = "blue", notch = FALSE) +
    ggtitle("ToothGrowth data: length vs dose, given type of supplement")
print(pl1)


pl2 <- ggplot(ToothGrowth, aes(factor(dose), y=len, fill = supp)) +
    geom_violin(stat="ydensity", position = "dodge") +
    ggtitle("ToothGrowth data: length vs dose, given type of supplement")
print(pl2)


tg_limits <- aes(ymax = tg_mean + tg_se, ymin = tg_mean - tg_se)
pl3 <- ggplot(tg, aes(supp, tg_mean, colour=dose)) +
    geom_point(stat="identity", size=3) +
    geom_errorbar( tg_limits, width = 0.1) +
    ggtitle("ToothGrowth data: length vs dose, given type of supplement") +
    facet_grid(.~dose)
print(pl3)
