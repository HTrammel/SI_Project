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
        geom_vline(stat = "vline", xintercept = theo_mean,
            size = 2, color = "red", linetype = 1) +
        annotate("text", x = theo_mean + 0.6, y = 0.6, colour="red",
            label="Theoretical Mean = 5") +
        geom_vline(stat = "vline", xintercept = d_mean, size = 2, linetype = 2) +
        annotate("text", x = d_mean + 0.65, y = 0.57,
            label = paste("Population Mean =", round(d_mean,3))) +
        stat_function(fun = dnorm, size = 2, linetype = 1,
            args = list(mean = d_mean, sd = d_sd)) +
        annotate("text", x = d_mean+1.5, y=.3, label="Population Mean Distribution")

print(g2)

set.seed(4)
idx <- sample(1:nrow(pop_df), 40)
samp_df <- data.frame(pop_df[idx,])
names(samp_df) <- c("samp_mean","samp_sd")
s_mean <- mean(samp_df$samp_mean)
s_sd <- sd(samp_df$samp_mean)

g3 <- ggplot(samp_df, aes(x = samp_mean)) +
    geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
    ggtitle("Plot of Means of 40 Samples form Population Exponential Distribution") +
    geom_vline(stat = "vline", xintercept = theo_mean,
               size = 2, color = "red", linetype = 1) +
    annotate("text", x = theo_mean + 0.6, y = 0.65, colour="red",
            label="Theoretical Mean = 5") +
            geom_vline(stat = "vline", xintercept = s_mean,
            size = 2, color = "blue", linetype = 2) +
        annotate("text", x = s_mean + 0.59, y = 0.6, colour="blue",
            label = paste("Sample Mean =", round(s_mean,3))) +
        stat_function(fun = dnorm, colour = "blue", size = 2, linetype = 1,
            args = list(mean = s_mean, sd = s_sd)) +
        annotate("text", x = s_mean+1.4, y=.3, colour="blue",
            label="Sample Mean Distribution") +
    geom_vline(stat = "vline", xintercept = d_mean, size = 2, linetype = 2) +
    annotate("text", x = d_mean + 0.65, y = 0.55,
             label = paste("Population Mean =", round(d_mean,3))) +
    stat_function(fun = dnorm, size = 2, linetype = 1,
                  args = list(mean = d_mean, sd = d_sd)) +
    annotate("text", x = d_mean+1.57, y=0.22,
             label="Population Mean Distribution")

print(g3)
