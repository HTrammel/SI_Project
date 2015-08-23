# course project
#
library(ggplot2)
library(dplyr)
library(MASS)

num_sim <- 1000
num_samp <- 40
fig <- 0

lambda <- 0.2
theo_mean <- 1/lambda
theo_sd <- 1/lambda
theo_var <- theo_sd^2

set.seed(4)
dat <- data.frame(replicate(num_sim, rexp(num_samp, 0.2)))
pop_df <- data.frame(
    pop_mean = c(apply(dat, 2, mean)),
    pop_sd = c(apply(dat, 2, sd)),
    pop_var = c(apply(dat, 2, var))
)
samp_mean <- mean(pop_df$pop_mean)
samp_sd <- sd(pop_df$pop_mean)
samp_var <- mean(pop_df$pop_var)

fig <- fig + 1
g1 <- ggplot(pop_df, aes(x = pop_mean)) +
        geom_histogram(alpha = .20, binwidth=.05, colour = "black", aes(y = ..density..)) +
        ggtitle(paste("Figure ", fig,
                      "Plot of Means from One Thousand Exponential Distributions"))  +
        geom_vline(stat = "vline", xintercept = theo_mean,
            size = 2, color = "red", linetype = 1) +
        annotate("text", x = theo_mean + 0.77, y = 0.6, colour="red",
            label="Theoretical Mean = 5") +
        geom_vline(stat = "vline", xintercept = samp_mean, size = 2, linetype = 2) +
        annotate("text", x = samp_mean + 0.8, y = 0.57,
            label = paste("Sample Mean =", round(samp_mean,3))) +
        stat_function(fun = dnorm, size = 2, linetype = 1,
            args = list(mean = samp_mean, sd = samp_sd)) +
        annotate("text", x = samp_mean+1.9, y=.3, label="Distribution of Sample Mean")
print(g1)


var_fit <- fitdistr(pop_df$pop_var, "normal")
fig <- fig + 1
g2 <- ggplot(pop_df, aes(x = pop_var)) +
    geom_histogram(alpha = .20, binwidth=.5, colour = "black", aes(y = ..density..)) +
    ggtitle(paste("Figure", fig,
        "Plot of Variance of the Means of One Thousand Exponential Distributions")) +
    stat_function(fun = dnorm, size = 2, linetype = 1, size = 2,
                  args = list(mean = var_fit$estimate[1], sd = var_fit$estimate[2])) +
    annotate("text", x = 65, y=0.01, label="Variance of Mean Distribution") +
    geom_vline(stat = "vline", xintercept = theo_var, size = 2, color = "red", linetype = 1) +
    annotate("text", x = theo_var + 18, y = 0.04, colour="red",
                label="Theoretical Variance = 25") +
    geom_vline(stat = "vline", xintercept = samp_var, size = 2, linetype = 2) +
    annotate("text", x = samp_var + 23, y = 0.06,
                label = paste("Variance of Sample Means =", round(samp_var,3)))
print(g2)
