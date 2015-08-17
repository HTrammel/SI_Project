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

# require(graphics)
# coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
#        xlab = "ToothGrowth data: length vs dose, given type of supplement")

tg <- ToothGrowth %>% group_by(supp, dose) %>% summarise_each(funs(mean, sd))
print(tg)

tg_pl <- ggplot(ToothGrowth, aes(dose, len)) +
    geom_dotplot() +
    facet_grid(. ~ supp)

print(tg_pl)
