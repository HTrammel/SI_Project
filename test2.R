library(ggplot2)
library(MASS)

num_sim <- 1000
num_samp <- 40

# generate the data
dat <- data.frame(replicate(num_sim, rexp(num_samp, 0.2)))
mn_df <- data.frame(colMeans(dat))
names(mn_df) <- "x"

# CLT formula for Exponential Distribution
clt_func <- function(x, n) (mean(x) - 0.5 / (5 / sqrt(n)))

set.seed(40)
idx <- sample(1:nrow(mn_df), 50)
clt_dat <- mn_df[idx,]

clt_df = NULL
for (i in clt_dat) clt_df <- rbind(clt_df, (clt_func(i,50)))

clt_df <- data.frame(clt_df)
names(clt_df) <- "x"

clt_fit <- fitdistr(clt_df$x, "normal")

g3 <- ggplot(clt_df, aes(x = x)) +
    geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) +
    stat_function(fun = dnorm,
                  args = list(mean = clt_fit$estimate[1], sd = clt_fit$estimate[2]),
                  size = 2)
print(g3)
