# course project
#
library(ggplot2)
library(dplyr)
library(MASS)

num_sim <- 1000
num_samp <- 40

# generate the data
dat <- data.frame(replicate(num_sim, rexp(num_samp, 0.2)))

# show graph of random sample of the data
samp <- sample(dat, 1)
names(samp) <- "x"
g1 <- ggplot(samp, aes(x = x)) +
    geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
    stat_function(fun = dexp, size = 2)
#print(g1)

# get the column means and graph
mn_df <- data.frame(colMeans(dat))
names(mn_df) <- "x"

mn_fit <- fitdistr(mn_df$x, "normal")

g2 <- ggplot(mn_df, aes(x)) +
    geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
    stat_function(fun = dexp,
                  args = list(mean = mn_fit$estimate[1], sd = mn_fit$estimate[2]),
                  size = 2)
print(g2)

# CLT formula for Exponential Distribution
clt_func <- function(x, n) (mean(x) - 0.5 / (5 / sqrt(n)))

clt_dat = NULL
for (i in mn_df) clt_dat <- rbind(clt_dat, (clt_func(i,40)))
clt_df <- as.data.frame(clt_dat)
names(clt_df) <- "x"

clt_fit <- fitdistr(clt_df$x, "normal")

g3 <- ggplot(clt_df, aes(x = x)) +
    geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
    stat_function(fun = dnorm,
                  args = list(mean = clt_fit$estimate[1], sd = clt_fit$estimate[2]),
                  size = 2)
print(g3)
