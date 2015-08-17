# course project
#
library(ggplot2)

r_exp <- rexp(1000,0.2)
r_mn <- mean(r_exp)
r_sd <- sd(r_exp)

base_pl <- qplot(r_exp) +
    stat_bin() +
    geom_vline(xintercept = mean(r_exp), colour = "red")

print(base_pl)



#p + annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21, alpha = .2)

# mns = NULL
# for (i in 1:1000) mns <- c(mns, mean(runif(40)))
# sd_mean <- sd(mns)


