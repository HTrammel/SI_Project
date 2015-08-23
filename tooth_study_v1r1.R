# teeth study

# 1. Load the ToothGrowth data and perform some basic exploratory data analyses
# 2. Provide a basic summary of the data.
# 3. Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class
# 4. State your conclusions and the assumptions needed for your conclusions.
#
# Some criteria that you will be evaluated on
# * Did you  perform an exploratory data analysis of at least a single plot or table highlighting basic features of the data?
# * Did the student perform some relevant confidence intervals and/or tests?
# * Were the results of the tests and/or intervals interpreted in the context of the problem correctly?
# * Did the student describe the assumptions needed for their conclusions?

data("ToothGrowth")

library(dplyr)
library(ggplot2)
library(knitr)
tbl <- 0
fig <- 0
se <- function(x) sqrt(var(x)/length(x))

#-----------------------

fig <- fig + 1
pl0 <- ggplot(ToothGrowth, aes(factor(supp), y=len, fill = supp)) +
    geom_boxplot(stat="boxplot", outlier.colour = "blue") +
    ggtitle(paste("Figure", fig, "Odontobast Growth by Supplement Across All Doses")) +
    xlab("Supplement Received") +
    ylab("Changes in Odontoblast Length")
print(pl0)

fig <- fig + 1
pl1 <- ggplot(ToothGrowth, aes(factor(dose), y=len, fill = supp)) +
    geom_boxplot(stat="boxplot", outlier.colour = "blue", notch = FALSE) +
    ggtitle(paste("Figure", fig, "Odontobast Growth by Supplement and Dose")) +
    xlab("Supplement Received") +
    ylab("Changes in Odontoblast Length")

print(pl1)

#-----------------------

tg <- ToothGrowth %>%
    group_by(supp, dose) %>%
    summarise_each(funs(mean, sd, se)) %>%
    rename(tg_mean = mean, tg_sd = sd, tg_se = se)

tg_stats <- rbind(tg[4,], tg[1,], tg[5,], tg[2,], tg[6,], tg[3,])
names(tg_stats) <- c("Supplement","Dose","Mean","Standard Deviation","Standard Error")
tg_stats$Supplement <- as.character(tg_stats$Supplement)

for (i in c(1,3,5)) tg_stats[i,1] <-"Vitamin C"
for (i in c(2,4,6)) tg_stats[i,1] <-"Orange Juice"

#-----------------------
t_all <- t.test(ToothGrowth$len~ToothGrowth$supp, conf=0.95)

t_all_stat <- NULL
t_all_stat <- cbind(t_all_stat, c("t-statistic",
	"degrees of freedom",
	"p-value",
	"confidence interval",
	"mean in orange juice group",
	"mean in vitamin C group"))

t_all_stat <- cbind(t_all_stat, c(
    round(t_all$statistic, 3),
    round(t_all$parameter, 1),
    round(t_all$p.value, 3),
    paste(round(t_all$conf.int[1], 3)," ", round(t_all$conf.int[2], 3)),
	round(t_all$estimate[1], 3),
	round(t_all$estimate[2], 3)))


kable(t_all_stat, format="markdown", row.names = F, col.names = c("Attribute","Value"))


tbl <- tbl + 1
kable(tg_stats, format="markdown", caption = paste("Table", tbl, "Summary of Statistics"))


#-----------------------
vc05 <- ToothGrowth$len[ 1 : 10]
vc1  <- ToothGrowth$len[11 : 20]
vc2  <- ToothGrowth$len[21 : 30]
oj05 <- ToothGrowth$len[31 : 40]
oj1  <- ToothGrowth$len[41 : 50]
oj2  <- ToothGrowth$len[51 : 60]

dose_rows <- as.character(t_all_stat[1:6,1])
dose_rows[6] <- "mean of x"

d05_diff <- oj05 - vc05
t05 <-  t.test(d05_diff)
t05_stat <- dose_rows
t05_stat <- cbind(t05_stat, c(
                    round(t05$statistic, 3),
                    round(t05$parameter,1),
                    round(t05$p.value, 3),
                    round(t05$conf.int[1],3),
                    round(t05$conf.int[2],3),
                    round(t05$estimate[1], 3)))


kable(t05_stat, format="markdown", row.names = F, col.names = c("Attribute","Value"))


d1_diff <- oj1 - vc1
t1 <-  t.test(d1_diff)
t1_stat <- dose_rows
t1_stat <- cbind(t1_stat, c(
                    round(t1$statistic, 3),
                    round(t1$parameter,1),
                    round(t1$p.value, 3),
                    round(t1$conf.int[1],3),
                    round(t1$conf.int[2],3),
                    round(t1$estimate[1], 3)))

kable(t1_stat, format="markdown", row.names = F, col.names = c("Attribute","Value"))

d2_diff <- oj2 - vc2
t2 <-  t.test(d2_diff)
t2_stat <- dose_rows
t2_stat <- cbind(t2_stat, c(
                    round(t2$statistic, 3),
                    round(t2$parameter,1),
                    round(t2$p.value, 3),
                    round(t2$conf.int[1],3),
                    round(t2$conf.int[2],3),
                    round(t2$estimate[1], 3)))

kable(t2_stat, format="markdown", row.names = F, col.names = c("Attribute","Value"))

