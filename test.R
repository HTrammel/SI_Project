# course project
#

mns = NULL

for (i in 1:1000) mns = c(mns, mean(rexp(40, 0.2)))
hist(mns)
