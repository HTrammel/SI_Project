library(ggplot2)

num_sim <- 1000
num_samp <- 40

# CLT formulat for Exponential Distribution
cfunc <- function(x, n) 5 * sqrt(n) * (mean(x) - 0.5)

# given lambda of 0.2
theo_mean <- 1/.2
theo_sd <- 1/.2
theo_var <- (1/.2)^2

for (i in 1 : 1000) {
    r_exp <- cbind(r_exp, i, rexp(num_samp,0.2), cfunc(r_exp[i,2], num_samp))
}


# g <- ggplot(r_exp, aes(x=x)) +
#     geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
#     stat_function(fun = dexp, size = 1, colour = "red")
#
# print(g)

# nosim <- 1000
# cfunc <- function(x, n) 2 * sqrt(n) * (mean(x) - 0.5)
# dat <- data.frame(
#     x = c(apply(matrix(sample(0:1, nosim * 10, replace = TRUE),
#                        nosim), 1, cfunc, 10),
#           apply(matrix(sample(0:1, nosim * 20, replace = TRUE),
#                        nosim), 1, cfunc, 20),
#           apply(matrix(sample(0:1, nosim * 30, replace = TRUE),
#                        nosim), 1, cfunc, 30)
#     ),
#     size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
# g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..))
# g <- g + stat_function(fun = dnorm, size = 2)
# g + facet_grid(. ~ size)
# print(g)

