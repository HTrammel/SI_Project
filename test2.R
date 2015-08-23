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
pop_df <- data.frame(
    pop_mean = c(apply(dat, 2, mean)),
    pop_sd = c(apply(dat, 2, sd)),
    pop_var = c(apply(dat, 2, var))
)
samp_mean <- mean(pop_df$pop_mean)
samp_sd <- sd(pop_df$pop_mean)

set.seed(4)
samp <- data.frame(rexp(num_sim, 0.2))
names(samp) <- "x"
samp_fit <- fitdistr(samp$x, "exponential")
samp_rate <- samp_fit$estimate[1]

g3 <- ggplot(NULL) +
    geom_histogram(data=samp,
                   alpha = .20,
                   binwidth=.3,
                   colour = "black",
                   aes(x, y = ..density..)) +
    geom_histogram(data=pop_df,
                   alpha = .20,
                   binwidth=.3,
                   colour = "blue",
                   aes(pop_mean, y = ..density..)) +
    ggtitle("Expoential Distribution versus Distribution of Means")

print(g3)
