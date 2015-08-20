# course project
#
library(ggplot2)
library(dplyr)
library(MASS)

num_sim <- 1000
num_samp <- 40
theo_mean <- 1/0.2
theo_sd <- 1/0.2
theo_var <- theo_sd^2

# generate the data
samp <- data.frame(rexp(num_sim, 0.2))
names(samp) <- "x"
samp_fit <- fitdistr(samp$x, "exponential")
samp_rate <- samp_fit$estimate[1]

diff_rate <- round(abs(0.2 - samp_rate), 3)

g1 <- ggplot(samp, aes(x = x)) +
    geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
    ggtitle("Sample of Expoential Distribution") +
    stat_function(fun = dexp,
            args = list(rate = samp_rate),
            size = 2)
print(g1)

# get the column means and graph
set.seed(4)
dat <- data.frame(replicate(num_sim, rexp(num_samp, 0.2)))
mn_df <- data.frame(colMeans(dat))
names(mn_df) <- "x"

mn_fit <- fitdistr(mn_df$x, "normal")
est_mean <- mn_fit$estimate[1]
est_sd <- mn_fit$estimate[2]

g2 <- ggplot(mn_df, aes(x)) +
    geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
    geom_vline(stat = "vline", xintercept = theo_mean,
               size = 2, color = "red", linetype = 1, show_guide = T) +
    ggtitle("Plot of Means from Simulated Exponential Distribution") +
    stat_function(fun = dnorm,
                args = list(mean = est_mean, sd = est_sd),
                size = 2)
print(g2)

# CLT formula for Exponential Distribution
clt_func <- function(x, n) (mean(x) - 0.5 / (5 / sqrt(n)))
set.seed(40)
idx <- sample(1:nrow(mn_df), 40)
clt_dat <- mn_df[idx,]
clt_df = NULL
for (i in clt_dat) clt_df <- rbind(clt_df, (clt_func(i,40)))

clt_df <- data.frame(clt_df)
names(clt_df) <- "x"


clt_fit <- fitdistr(clt_df$x, "normal")

g3 <- ggplot(clt_df, aes(x = x)) +
    geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
    geom_vline(stat = "vline", xintercept = theo_mean,
               size = 2, color = "red", linetype = 1, show_guide = T) +
    ggtitle("Plot of 40 Sample Means from Simulated Exponential Distribution") +
    stat_function(fun = dnorm,
                  args = list(mean = clt_fit$estimate[1], sd = clt_fit$estimate[2]),
                  size = 2)
print(g3)
