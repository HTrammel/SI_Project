# Simulation code
library(ggplot2)
library(dplyr)

num_sim <- 1000
theo_mean <- 1/.2
theo_sd <- 1/.2
theo_var <- (1/.2)^2

r_exp <- rexp(1000,0.2)
r_mn <- mean(r_exp)
r_sd <- sd(r_exp)
hist(r_exp)

t_r_mn_dif <- theo_mean - r_mn
t_r_sd_dif <- theo_sd - r_sd

num_exp <- 40

s_mns <- NULL
s_sd <- NULL
for (i in 1 : 1000) {
    s <- rexp(num_exp, 0.2)
    s_mns <- c(s_mns, mean(s))
    s_sd <- c(s_sd, sd(s))
}
hist(s_mns)

s_mn <- mean(s_mns)
s_sd <- mean(s_sd)
s_var <- s_sd^2/num_exp

t_s_mn_dif <- theo_mean - s_mn
t_s_sd_dif <- theo_sd - s_sd


## Law of large numbers in action
n <- 10000; means <- cumsum(rnorm(n)) / (1  : n)
g <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
print(g)



## Law of large numbers in action, coin flip
means <- cumsum(sample(0 : 1, n , replace = TRUE)) / (1  : n)
g <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0.5) + geom_line(size = 2)
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
print(g)


## Result of our die rolling experiment
nosim <- 1000
cfunc <- function(x, n) sqrt(n) * (mean(x) - 3.5) / 1.71
dat <- data.frame(
  x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE),
                     nosim), 1, cfunc, 10),
        apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE),
                     nosim), 1, cfunc, 20),
        apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE),
                     nosim), 1, cfunc, 30)
        ),
  size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..))
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)
print(g)

## Simulation results
nosim <- 1000
cfunc <- function(x, n) 2 * sqrt(n) * (mean(x) - 0.5)
dat <- data.frame(
  x = c(apply(matrix(sample(0:1, nosim * 10, replace = TRUE),
                     nosim), 1, cfunc, 10),
        apply(matrix(sample(0:1, nosim * 20, replace = TRUE),
                     nosim), 1, cfunc, 20),
        apply(matrix(sample(0:1, nosim * 30, replace = TRUE),
                     nosim), 1, cfunc, 30)
        ),
  size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..))
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)
print(g)

## Simulation results, $p = 0.9$
nosim <- 1000
cfunc <- function(x, n) sqrt(n) * (mean(x) - 0.9) / sqrt(.1 * .9)
dat <- data.frame(
  x = c(apply(matrix(sample(0:1, prob = c(.1,.9), nosim * 10, replace = TRUE),
                     nosim), 1, cfunc, 10),
        apply(matrix(sample(0:1, prob = c(.1,.9), nosim * 20, replace = TRUE),
                     nosim), 1, cfunc, 20),
        apply(matrix(sample(0:1, prob = c(.1,.9), nosim * 30, replace = TRUE),
                     nosim), 1, cfunc, 30)
        ),
  size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..))
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)
print(g)

