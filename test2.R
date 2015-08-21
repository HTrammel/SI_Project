# course project
#
library(ggplot2)
library(dplyr)
library(MASS)

num_sim <- 1000
num_samp <- 40

lambda <- 0.2
theo_mean <- 1/lambda
theo_sd <- 1/lambda
theo_var <- theo_sd^2

set.seed(4)
dat <- data.frame(replicate(num_sim, rexp(num_samp, 0.2)))
pop_df <- data.frame(pop_mean = c(apply(dat, 2, mean)), pop_sd = c(apply(dat, 2, sd)))
d_mean <- mean(pop_df$pop_mean)
d_sd <- sd(pop_df$pop_mean)

g2 <- ggplot(pop_df, aes(x = pop_mean)) +
        geom_histogram(alpha = .20, binwidth=.05, colour = "black", aes(y = ..density..)) +
        ggtitle("Plot of Means from One Thousand Exponential Distributions")  +
        stat_function(fun = dnorm, colour = "blue", size = 2, linetype = 1,
                args = list(mean = d_mean, sd = d_sd)) +
        annotate("text", x = d_mean+1.5, y=.3,
                label="Population Mean Distribution",
                colour="blue") +
        geom_vline(stat = "vline", xintercept = d_mean,
                   size = 2, color = "blue", linetype = 2) +
        annotate("text", x = d_mean+.65, y=0.57,
                 label=paste("Population Mean =", round(d_mean,3)),
                 colour="blue")
print(g2)

