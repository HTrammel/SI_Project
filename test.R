# course project
#
library(ggplot2)

# CLT formulat for Exponential Distribution
clt_func <- function(x, n) 5 * sqrt(n) * (mean(x) - 0.5)

num_sim <- 1000
num_samp <- 40

# CLT formulat for Exponential Distribution
dat <- data.frame(
    x = c(apply(matrix(sample(0:1, nosim * nosamp, replace = TRUE),
                       nosim), 1, cfunc, nosamp
                ),
          apply(matrix(sample(nosim * nosamp, replace = TRUE),
                       nosim), 1, rexp, c(nosamp, 0.2)
                )
          )
    )

g <- ggplot(dat, aes(x = x)) +
    geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
    stat_function(fun = dexp)

print(g)

