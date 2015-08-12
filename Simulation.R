# Simulation code

num_sim <- 1000
num_exp <- 40

smpl_exp <- apply(matrix(num_sim, num_exp), c(1,2), rexp, c(num_sim * num_exp, 0.2))

# mns <- NULL
# for (i in 1:1000) mns = c(mns, mean(smpl_exp))
# hist(mns)
