####PREPARATION

#Install libraries
libraries <- c("plyr", "MASS", "haven", "stringr", "tidyverse", "dplyr", 
               "tibble", "stats", "vtable", "ppcor", "rio", "stringi", 
               "tidyr", "lubridate", "ggplot2", "pscl", "boot", "epiDisplay", 
               "DescTools", "pastecs", "qwraps2", "sjmisc", "ggpubr", "effsize", 
               "fitdistrplus")

to_install <- libraries[!libraries %in% installed.packages()]

if (length(to_install) > 0) {
    install.packages(to_install)
} else {
    print("All required packages are already installed.")
}



#Load libraries
library(plyr)
library(MASS)
library(haven)
library(stringr)
library(tidyverse)
library(dplyr)
library(tibble)
library(stats)
library(vtable)
library(ppcor)
library(rio)
library(stringi)
library(tidyr)
library(lubridate)
library(ggplot2)
library(pscl)
library(boot)
library(lattice)
library(foreign)
library(survival)
library(MASS)
library(nnet)
library(epiDisplay)
library(DescTools)
library(pastecs)
library(dplyr)
library(qwraps2)
library(sjmisc)
library(ggpubr)
library(effsize)
library(rstatix)
library(fitdistrplus)


####DATA ANALYSIS

ggplot(dt_survey_EST_1, aes(filter_or_loop_change)) + geom_histogram() + scale_x_log10()
summary(m1 <- zeroinfl(filter_or_loop_change ~ age + totalchildren + wel01 + wel08 + count_updatepage.x + totalchildren*count_updatepage.x, data = dt_survey_EST_1))
cor.test(dt_survey_EST_1$totalchildren, dt_survey_EST_1$count_updatepage.x, method = 'pearson')

table(dt_survey_EST_1$wrk02, dt_survey_EST_1$count_updatepage.x)
chisq.test(dt_survey_EST_1$wrk02, dt_survey_EST_1$count_updatepage.x, correct=FALSE)

boxplot(dt_survey_EST_1$count_updatepage.x~factor(dt_survey_EST_1$wrk02),
        xlab = "wrk02", ylab = "count_updatepage.x")
tapply(dt_survey_EST_1$count_updatepage.x, dt_survey_EST_1$wrk02, summary)
summary(aov(dt_survey_EST_1$count_updatepage.x~factor(dt_survey_EST_1$wrk02)))


dt_survey_EST_1$dem21_recoded <- recode(dt_survey_EST_1$dem21, '2' = '0', '1' = '1')

summary(m2 <- zeroinfl(change_immediate_filter_1 ~ age + dem07 + dem21_recoded + totalchildren + count_updatepage.x + totalchildren*count_updatepage.x, data = dt_survey_EST_1))
summary(m3 <- zeroinfl(change_loop_1 ~ age + dem07 + dem21_recoded + totalchildren + count_updatepage.x + totalchildren*count_updatepage.x, data = dt_survey_EST_1))

dt_survey_EST_wentback <- filter(dt_survey_EST_1, dt_survey_EST_1$previous_present == "TRUE")
summary(dt_survey_EST_wentback$change_filter_or_loop_present)



##DESCRIPTIVE STATISTICS for dependent variables


#All previous
mean(dt_survey_EST_1$counts_previous.y, na.rm = TRUE)
median(dt_survey_EST_1$counts_previous.y, na.rm = TRUE)
sd(dt_survey_EST_1$counts_previous.y, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_previous.y))
sum(na.omit(dt_survey_EST_1$counts_previous.y) > 0)/length(na.omit(dt_survey_EST_1$counts_previous.y))
min(dt_survey_EST_1$counts_previous.y, na.rm = TRUE)
max(dt_survey_EST_1$counts_previous.y, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_previous.y, na.rm = TRUE)
length(dt_survey_EST_1$counts_previous.y[dt_survey_EST_1$counts_previous.y>0])

#To immediate filter
mean(dt_survey_EST_1$counts_to_immediate_filter.y, na.rm = TRUE)
median(dt_survey_EST_1$counts_to_immediate_filter.y, na.rm = TRUE)
sd(dt_survey_EST_1$counts_to_immediate_filter.y, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_to_immediate_filter.y))
sum(na.omit(dt_survey_EST_1$counts_to_immediate_filter.y) > 0)/length(na.omit(dt_survey_EST_1$counts_to_immediate_filter.y))
min(dt_survey_EST_1$counts_to_immediate_filter.y, na.rm = TRUE)
max(dt_survey_EST_1$counts_to_immediate_filter.y, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_to_immediate_filter.y, na.rm = TRUE)
length(dt_survey_EST_1$counts_to_immediate_filter.y[dt_survey_EST_1$counts_to_immediate_filter.y>0])

#To regular
mean(dt_survey_EST_1$counts_to_regular_2, na.rm = TRUE)
median(dt_survey_EST_1$counts_to_regular_2, na.rm = TRUE)
sd(dt_survey_EST_1$counts_to_regular_2, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_to_regular_2))
sum(na.omit(dt_survey_EST_1$counts_to_regular_2) > 0)/length(na.omit(dt_survey_EST_1$counts_to_regular_2))
min(dt_survey_EST_1$counts_to_regular_2, na.rm = TRUE)
max(dt_survey_EST_1$counts_to_regular_2, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_to_regular_2, na.rm = TRUE)
length(dt_survey_EST_1$counts_to_regular_2[dt_survey_EST_1$counts_to_regular_2>0])


#To loop
mean(dt_survey_EST_1$counts_to_loop, na.rm = TRUE)
median(dt_survey_EST_1$counts_to_loop, na.rm = TRUE)
sd(dt_survey_EST_1$counts_to_loop, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_to_loop))
sum(na.omit(dt_survey_EST_1$counts_to_loop) > 0)/length(na.omit(dt_survey_EST_1$counts_to_loop))
min(dt_survey_EST_1$counts_to_loop, na.rm = TRUE)
max(dt_survey_EST_1$counts_to_loop, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_to_loop, na.rm = TRUE)
length(dt_survey_EST_1$counts_to_loop[dt_survey_EST_1$counts_to_loop>0])


#To nonimmediate filter
mean(dt_survey_EST_1$counts_to_nonimmediate_filter.y, na.rm = TRUE)
median(dt_survey_EST_1$counts_to_nonimmediate_filter.y, na.rm = TRUE)
sd(dt_survey_EST_1$counts_to_nonimmediate_filter.y, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_to_nonimmediate_filter.y))
sum(na.omit(dt_survey_EST_1$counts_to_nonimmediate_filter_1) > 0)/length(na.omit(dt_survey_EST_1$counts_to_nonimmediate_filter_1))
min(dt_survey_EST_1$counts_to_nonimmediate_filter.y, na.rm = TRUE)
max(dt_survey_EST_1$counts_to_nonimmediate_filter.y, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_to_nonimmediate_filter.y, na.rm = TRUE)
length(dt_survey_EST_1$counts_to_nonimmediate_filter.y[dt_survey_EST_1$counts_to_nonimmediate_filter.y>0])

#Distributions
#All previous
ggplot(dt_survey_EST_1, aes(x=counts_previous.y)) + geom_histogram(binwidth=5)
ggplot(subset(dt_survey_EST_1, counts_previous.y>0), aes(x=counts_previous.y)) + geom_histogram(binwidth=5)
ggplot(subset(dt_survey_EST_1, counts_previous.y<200), aes(x=counts_previous.y)) + geom_histogram(binwidth=5)

#To immediate filter
ggplot(dt_survey_EST_1, aes(x=counts_to_immediate_filter.y)) + geom_histogram(binwidth=2.5)
ggplot(subset(dt_survey_EST_1, counts_to_immediate_filter.y>0), aes(x=counts_to_immediate_filter.y)) + geom_histogram(binwidth=5)

#To regular
ggplot(dt_survey_EST_1, aes(x=counts_to_regular_2)) + geom_histogram(binwidth=2.5)
ggplot(subset(dt_survey_EST_1, counts_to_regular_2>0), aes(x=counts_to_regular_2)) + geom_histogram(binwidth=5)

#To loop
ggplot(dt_survey_EST_1, aes(x=counts_to_loop)) + geom_histogram(binwidth=.5)
ggplot(subset(dt_survey_EST_1, counts_to_loop>0), aes(x=counts_to_loop)) + geom_histogram(binwidth=.5)

#To nonimmediate filter
ggplot(dt_survey_EST_1, aes(x=counts_to_nonimmediate_filter.y)) + geom_histogram(binwidth=1)
ggplot(subset(dt_survey_EST_1, counts_to_nonimmediate_filter.y>0), aes(x=counts_to_nonimmediate_filter.y)) + geom_histogram(binwidth=1)


###Desriptives for "after"
##All
#after_same
mean(dt_survey_EST_1$counts_after_same_1, na.rm = TRUE)
median(dt_survey_EST_1$counts_after_same_1, na.rm = TRUE)
sd(dt_survey_EST_1$counts_after_same_1, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_after_same_1))
sum(na.omit(dt_survey_EST_1$counts_after_same_1) > 0)/length(na.omit(dt_survey_EST_1$counts_after_same_1))
min(dt_survey_EST_1$counts_after_same_1, na.rm = TRUE)
max(dt_survey_EST_1$counts_after_same_1, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_after_same_1, na.rm = TRUE)
length(dt_survey_EST_1$counts_after_same_1[dt_survey_EST_1$counts_after_same_1>0])

#after_later
mean(dt_survey_EST_1$counts_after_later_1, na.rm = TRUE)
median(dt_survey_EST_1$counts_after_later_1, na.rm = TRUE)
sd(dt_survey_EST_1$counts_after_later_1, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_after_later_1))
sum(na.omit(dt_survey_EST_1$counts_after_later_1) > 0)/length(na.omit(dt_survey_EST_1$counts_after_later_1))
min(dt_survey_EST_1$counts_after_later_1, na.rm = TRUE)
max(dt_survey_EST_1$counts_after_later_1, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_after_later_1, na.rm = TRUE)
length(dt_survey_EST_1$counts_after_later_1[dt_survey_EST_1$counts_after_later_1>0])

#after_earlier
mean(dt_survey_EST_1$counts_after_earlier_1, na.rm = TRUE)
median(dt_survey_EST_1$counts_after_earlier_1, na.rm = TRUE)
sd(dt_survey_EST_1$counts_after_earlier_1, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_after_earlier_1))
sum(na.omit(dt_survey_EST_1$counts_after_earlier_1) > 0)/length(na.omit(dt_survey_EST_1$counts_after_earlier_1))
min(dt_survey_EST_1$counts_after_earlier_1, na.rm = TRUE)
max(dt_survey_EST_1$counts_after_earlier_1, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_after_earlier_1, na.rm = TRUE)
length(dt_survey_EST_1$counts_after_earlier_1[dt_survey_EST_1$counts_after_earlier_1>0])

##To immediate filter
#after_same
mean(dt_survey_EST_1$counts_immediate_filter_aftsame.y, na.rm = TRUE)
median(dt_survey_EST_1$counts_immediate_filter_aftsame.y, na.rm = TRUE)
sd(dt_survey_EST_1$counts_immediate_filter_aftsame.y, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_immediate_filter_aftsame.y))
sum(na.omit(dt_survey_EST_1$counts_immediate_filter_aftsame.y) > 0)/length(na.omit(dt_survey_EST_1$counts_immediate_filter_aftsame.y))
min(dt_survey_EST_1$counts_immediate_filter_aftsame.y, na.rm = TRUE)
max(dt_survey_EST_1$counts_immediate_filter_aftsame.y, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_immediate_filter_aftsame.y, na.rm = TRUE)
length(dt_survey_EST_1$counts_immediate_filter_aftsame.y[dt_survey_EST_1$counts_immediate_filter_aftsame.y>0])

#after_later
mean(dt_survey_EST_1$counts_immediate_filter_aftlater.y, na.rm = TRUE)
median(dt_survey_EST_1$counts_immediate_filter_aftlater.y, na.rm = TRUE)
sd(dt_survey_EST_1$counts_immediate_filter_aftlater.y, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_immediate_filter_aftlater.y))
sum(na.omit(dt_survey_EST_1$counts_immediate_filter_aftlater.y) > 0)/length(na.omit(dt_survey_EST_1$counts_immediate_filter_aftlater.y))
min(dt_survey_EST_1$counts_immediate_filter_aftlater.y, na.rm = TRUE)
max(dt_survey_EST_1$counts_immediate_filter_aftlater.y, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_immediate_filter_aftlater.y, na.rm = TRUE)
length(dt_survey_EST_1$counts_immediate_filter_aftlater.y[dt_survey_EST_1$counts_immediate_filter_aftlater.y>0])

#after_earlier
mean(dt_survey_EST_1$counts_immediate_filter_aftearlier, na.rm = TRUE)
median(dt_survey_EST_1$counts_immediate_filter_aftearlier, na.rm = TRUE)
sd(dt_survey_EST_1$counts_immediate_filter_aftearlier, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_immediate_filter_aftearlier))
sum(na.omit(dt_survey_EST_1$counts_immediate_filter_aftearlier) > 0)/length(na.omit(dt_survey_EST_1$counts_immediate_filter_aftearlier))
min(dt_survey_EST_1$counts_immediate_filter_aftearlier, na.rm = TRUE)
max(dt_survey_EST_1$counts_immediate_filter_aftearlier, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_immediate_filter_aftearlier, na.rm = TRUE)
length(dt_survey_EST_1$counts_immediate_filter_aftearlier[dt_survey_EST_1$counts_immediate_filter_aftearlier>0])

##To regular
#after_same
mean(dt_survey_EST_1$counts_to_regular_aftsame, na.rm = TRUE)
median(dt_survey_EST_1$counts_to_regular_aftsame, na.rm = TRUE)
sd(dt_survey_EST_1$counts_to_regular_aftsame, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_to_regular_aftsame))
sum(na.omit(dt_survey_EST_1$counts_to_regular_aftsame) > 0)/length(na.omit(dt_survey_EST_1$counts_to_regular_aftsame))
min(dt_survey_EST_1$counts_to_regular_aftsame, na.rm = TRUE)
max(dt_survey_EST_1$counts_to_regular_aftsame, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_to_regular_aftsame, na.rm = TRUE)
length(dt_survey_EST_1$counts_to_regular_aftsame[dt_survey_EST_1$counts_to_regular_aftsame>0])

#after_later
mean(dt_survey_EST_1$counts_to_regular_aftlater, na.rm = TRUE)
median(dt_survey_EST_1$counts_to_regular_aftlater, na.rm = TRUE)
sd(dt_survey_EST_1$counts_to_regular_aftlater, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_to_regular_aftlater))
sum(na.omit(dt_survey_EST_1$counts_to_regular_aftlater) > 0)/length(na.omit(dt_survey_EST_1$counts_to_regular_aftlater))
min(dt_survey_EST_1$counts_to_regular_aftlater, na.rm = TRUE)
max(dt_survey_EST_1$counts_to_regular_aftlater, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_to_regular_aftlater, na.rm = TRUE)
length(dt_survey_EST_1$counts_to_regular_aftlater[dt_survey_EST_1$counts_to_regular_aftlater>0])

#after_earlier
mean(dt_survey_EST_1$counts_to_regular_aftearlier, na.rm = TRUE)
median(dt_survey_EST_1$counts_to_regular_aftearlier, na.rm = TRUE)
sd(dt_survey_EST_1$counts_to_regular_aftearlier, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_to_regular_aftearlier))
sum(na.omit(dt_survey_EST_1$counts_to_regular_aftearlier) > 0)/length(na.omit(dt_survey_EST_1$counts_to_regular_aftearlier))
min(dt_survey_EST_1$counts_to_regular_aftearlier, na.rm = TRUE)
max(dt_survey_EST_1$counts_to_regular_aftearlier, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_to_regular_aftearlier, na.rm = TRUE)
length(dt_survey_EST_1$counts_to_regular_aftearlier[dt_survey_EST_1$counts_to_regular_aftearlier>0])


##To loop
#after_same
mean(dt_survey_EST_1$counts_to_loop_aftsame.y, na.rm = TRUE)
median(dt_survey_EST_1$counts_to_loop_aftsame.y, na.rm = TRUE)
sd(dt_survey_EST_1$counts_to_loop_aftsame.y, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_to_loop_aftsame.y))
sum(na.omit(dt_survey_EST_1$counts_to_loop_aftsame.y) > 0)/length(na.omit(dt_survey_EST_1$counts_to_loop_aftsame.y))
min(dt_survey_EST_1$counts_to_loop_aftsame.y, na.rm = TRUE)
max(dt_survey_EST_1$counts_to_loop_aftsame.y, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_to_loop_aftsame.y, na.rm = TRUE)
length(dt_survey_EST_1$counts_to_loop_aftsame.y[dt_survey_EST_1$counts_to_loop_aftsame.y>0])

#after_later
mean(dt_survey_EST_1$counts_to_loop_aftlater.y, na.rm = TRUE)
median(dt_survey_EST_1$counts_to_loop_aftlater.y, na.rm = TRUE)
sd(dt_survey_EST_1$counts_to_loop_aftlater.y, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_to_loop_aftlater.y))
sum(na.omit(dt_survey_EST_1$counts_to_loop_aftlater.y) > 0)/length(na.omit(dt_survey_EST_1$counts_to_loop_aftlater.y))
min(dt_survey_EST_1$counts_to_loop_aftlater.y, na.rm = TRUE)
max(dt_survey_EST_1$counts_to_loop_aftlater.y, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_to_loop_aftlater.y, na.rm = TRUE)
length(dt_survey_EST_1$counts_to_loop_aftlater.y[dt_survey_EST_1$counts_to_loop_aftlater.y>0])

#after_earlier
mean(dt_survey_EST_1$counts_to_loop_aftearlier.y, na.rm = TRUE)
median(dt_survey_EST_1$counts_to_loop_aftearlier.y, na.rm = TRUE)
sd(dt_survey_EST_1$counts_to_loop_aftearlier.y, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_to_loop_aftearlier.y))
sum(na.omit(dt_survey_EST_1$counts_to_loop_aftearlier.y) > 0)/length(na.omit(dt_survey_EST_1$counts_to_loop_aftearlier.y))
min(dt_survey_EST_1$counts_to_loop_aftearlier.y, na.rm = TRUE)
max(dt_survey_EST_1$counts_to_loop_aftearlier.y, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_to_loop_aftearlier.y, na.rm = TRUE)
length(dt_survey_EST_1$counts_to_loop_aftearlier.y[dt_survey_EST_1$counts_to_loop_aftearlier.y>0])

##Nonimmediate filter
#after_same
mean(dt_survey_EST_1$counts_nonimmediate_filter_aftsame.y, na.rm = TRUE)
median(dt_survey_EST_1$counts_nonimmediate_filter_aftsame.y, na.rm = TRUE)
sd(dt_survey_EST_1$counts_nonimmediate_filter_aftsame.y, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_nonimmediate_filter_aftsame.y))
sum(na.omit(dt_survey_EST_1$counts_nonimmediate_filter_aftsame.y) > 0)/length(na.omit(dt_survey_EST_1$counts_nonimmediate_filter_aftsame.y))
min(dt_survey_EST_1$counts_nonimmediate_filter_aftsame.y, na.rm = TRUE)
max(dt_survey_EST_1$counts_nonimmediate_filter_aftsame.y, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_nonimmediate_filter_aftsame.y, na.rm = TRUE)
length(dt_survey_EST_1$counts_nonimmediate_filter_aftsame.y[dt_survey_EST_1$counts_nonimmediate_filter_aftsame.y>0])

#after_later
mean(dt_survey_EST_1$counts_nonimmediate_filter_aftlater.y, na.rm = TRUE)
median(dt_survey_EST_1$counts_nonimmediate_filter_aftlater.y, na.rm = TRUE)
sd(dt_survey_EST_1$counts_nonimmediate_filter_aftlater.y, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_nonimmediate_filter_aftlater.y))
sum(na.omit(dt_survey_EST_1$counts_nonimmediate_filter_aftlater.y) > 0)/length(na.omit(dt_survey_EST_1$counts_nonimmediate_filter_aftlater.y))
min(dt_survey_EST_1$counts_nonimmediate_filter_aftlater.y, na.rm = TRUE)
max(dt_survey_EST_1$counts_nonimmediate_filter_aftlater.y, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_nonimmediate_filter_aftlater.y, na.rm = TRUE)
length(dt_survey_EST_1$counts_nonimmediate_filter_aftlater.y[dt_survey_EST_1$counts_nonimmediate_filter_aftlater.y>0])

#after_earlier
mean(dt_survey_EST_1$counts_nonimmediate_filter_aftearlier.y, na.rm = TRUE)
median(dt_survey_EST_1$counts_nonimmediate_filter_aftearlier.y, na.rm = TRUE)
sd(dt_survey_EST_1$counts_nonimmediate_filter_aftearlier.y, na.rm = TRUE)
length(na.omit(dt_survey_EST_1$counts_nonimmediate_filter_aftearlier.y))
sum(na.omit(dt_survey_EST_1$counts_nonimmediate_filter_aftearlier.y) > 0)/length(na.omit(dt_survey_EST_1$counts_nonimmediate_filter_aftearlier.y))
min(dt_survey_EST_1$counts_nonimmediate_filter_aftearlier.y, na.rm = TRUE)
max(dt_survey_EST_1$counts_nonimmediate_filter_aftearlier.y, na.rm = TRUE)
quantile(dt_survey_EST_1$counts_nonimmediate_filter_aftearlier.y, na.rm = TRUE)
length(dt_survey_EST_1$counts_nonimmediate_filter_aftearlier.y[dt_survey_EST_1$counts_nonimmediate_filter_aftearlier.y>0])


#Distributions

#after_same
ggplot(dt_survey_EST_1, aes(x=counts_after_same_1)) + geom_histogram(binwidth=2.5)
ggplot(subset(dt_survey_EST_1, counts_after_same_1>0), aes(x=counts_after_same_1)) + geom_histogram(binwidth=5)


#Recode independent variables:
dt_survey_EST_1$filled_in_one_sitting_updtd
dt_survey_EST_1$count_updatepage.y
dt_survey_EST_1$totalchildren
dt_survey_EST_1$female
table(dt_survey_EST_1$dem01)
frq(dt_survey_EST_1$haspartner)


#dem01 (2 - 1, 1 - 0)
dt_survey_EST_1$female <- ifelse(dt_survey_EST_1$dem01 == 1, 0, 1)
#dem07 (by group)
dt_survey_EST_1$edu_lowest <- rec(dt_survey_EST_1$dem07, rec = "0:4=1; else=0")
dt_survey_EST_1$edu_upperscndr <- rec(dt_survey_EST_1$dem07, rec = "5=1; else=0")
dt_survey_EST_1$edu_postscndr <- rec(dt_survey_EST_1$dem07, rec = "6=1; else=0")
dt_survey_EST_1$edu_tertiary <- rec(dt_survey_EST_1$dem07, rec = "7=1; else=0")
dt_survey_EST_1$edu_bachelor <- rec(dt_survey_EST_1$dem07, rec = "8=1; else=0")
dt_survey_EST_1$edu_masters <- rec(dt_survey_EST_1$dem07, rec = "9=1; else=0")
dt_survey_EST_1$edu_doctor <- rec(dt_survey_EST_1$dem07, rec = "10=1; else=0")
#dem07 (new)
dt_survey_EST_1$edu_lowest_1 <- rec(dt_survey_EST_1$dem07, rec = "0:4=1; else=0")
dt_survey_EST_1$edu_scndr_1 <- rec(dt_survey_EST_1$dem07, rec = "5:7=1; else=0")
dt_survey_EST_1$edu_bachelor_1 <- rec(dt_survey_EST_1$dem07, rec = "8=1; else=0")
dt_survey_EST_1$edu_masters_and_higher <- rec(dt_survey_EST_1$dem07, rec = "9:10=1; else=0")


#age groups (18-25, 26-35, 36-45, 46-65, 65+)
dt_survey_EST_1$age_18_25 <- rec(dt_survey_EST_1$age, rec = "18:25=1; else=0")
dt_survey_EST_1$age_26_35 <- rec(dt_survey_EST_1$age, rec = "26:35=1; else=0")
dt_survey_EST_1$age_36_45 <- rec(dt_survey_EST_1$age, rec = "36:45=1; else=0")
dt_survey_EST_1$age_46_59 <- rec(dt_survey_EST_1$age, rec = "46:59=1; else=0")
dt_survey_EST_1$age_60plus <- rec(dt_survey_EST_1$age, rec = "60:111=1; else=0")
#wrk02 (2-3 - 0)
dt_survey_EST_1$wrk02_rec <- ifelse(dt_survey_EST_1$wrk02 == 1, 1, 0)
#DEM21 (2 - 0) - haspartner
dt_survey_EST_1$dem21_rec <- ifelse(dt_survey_EST_1$dem21 == 1, 1, 0)


#Zero-inflated for table 2

#All previous
frq(dt_survey_EST_1$counts_previous)
summary(zim1_all <- zeroinfl(counts_previous.y ~ female + totalchildren + edu_upperscndr + edu_postscndr +
                                 edu_tertiary + edu_bachelor + edu_masters + edu_doctor + 
                                 age_26_35 + age_36_45 + age_45_59 + age_60plus + wrk02_rec +
                                 dem21_rec + filled_in_one_sitting_updtd + count_updatepage.y,
                             data = dt_survey_EST_1))

#To immediate filter
frq(dt_survey_EST_1$counts_to_immediate_filter.y)
summary(zim1_imm <- zeroinfl(counts_to_immediate_filter.y ~ female + totalchildren + edu_upperscndr + edu_postscndr +
                                 edu_tertiary + edu_bachelor + edu_masters + edu_doctor + 
                                 age_26_35 + age_36_45 + age_45_59 + age_60plus + wrk02_rec +
                                 dem21_rec + filled_in_one_sitting_updtd + count_updatepage.y
                             , data = dt_survey_EST_1))



#To regular
frq(dt_survey_EST_1$counts_to_regular_2)
summary(zim1_reg <- zeroinfl(counts_to_regular_2 ~ female + totalchildren + edu_upperscndr + edu_postscndr +
                                 edu_tertiary + edu_bachelor + edu_masters + edu_doctor + 
                                 age_26_35 + age_36_45 + age_45_59 + age_60plus + wrk02_rec +
                                 dem21_rec + filled_in_one_sitting_updtd + count_updatepage.y
                             , data = dt_survey_EST_1))



#To loop
frq(dt_survey_EST_1$counts_to_loop)
summary(zim1_loop <- zeroinfl(counts_to_loop ~ female + totalchildren + edu_upperscndr + edu_postscndr +
                                  edu_tertiary + edu_bachelor + edu_masters + edu_doctor + 
                                  age_26_35 + age_36_45 + age_45_59 + age_60plus + wrk02_rec +
                                  dem21_rec + filled_in_one_sitting_updtd + count_updatepage.y
                              , data = dt_survey_EST_1))


#To nonimmediate filter
frq(dt_survey_EST_1$counts_to_nonimmediate_filter.y)
summary(zim1_nonimm <- zeroinfl(counts_to_nonimmediate_filter.y ~ female + totalchildren + edu_upperscndr + edu_postscndr +
                                    edu_tertiary + edu_bachelor + edu_masters + edu_doctor + 
                                    age_26_35 + age_36_45 + age_45_59 + age_60plus + wrk02_rec +
                                    dem21_rec + filled_in_one_sitting_updtd + count_updatepage.y
                                , data = dt_survey_EST_1))

##Who is going back and changes the size of the questionnaire?

dt_survey_EST_1$once_prev <- ifelse(dt_survey_EST_1$counts_previous.y >= 1, 1, ifelse(dt_survey_EST_1$counts_previous.y == 0, 0, NA))
dt_survey_EST_1$once_sat <- ifelse(dt_survey_EST_1$counts_after_later_1 >= 1, 1, ifelse(dt_survey_EST_1$counts_after_later_1 == 0, 0, NA))

dt_survey_EST_onceprev <- subset(dt_survey_EST_1, dt_survey_EST_1$once_prev == 1)

#Female ~ Satisficing (all previous) dt_survey_EST_1$counts_after_later_1/female

w_test_female_all <- wilcox.test(counts_after_later_1 ~ female, data = dt_survey_EST_1,
                                 exact = FALSE)
w_test_female_all <- t.test(counts_after_later_1 ~ female, data = dt_survey_EST_1, var.equal = TRUE)
wilcox_effsize(x, y, rscale = "n", method = "r")

wilcox.test(counts_after_later_1 ~ female, data = dt_survey_EST_onceprev,
            exact = FALSE)
t.test(counts_after_later_1 ~ female, data = dt_survey_EST_onceprev, var.equal = TRUE)


cohens_d(sat_fact_allq ~ female, data = dt_survey_EST_1)
wilcox.test(sat_fact_allq ~ female, data = dt_survey_EST_1,
            exact = FALSE)
t.test(sat_fact_allq ~ female, data = dt_survey_EST_1, var.equal = TRUE)
wilcox_effsize(dt_survey_EST_1$sat_fact_allq, dt_survey_EST_1$female, rscale = "n", method = "r")

wilcox.test(sat_fact_allq ~ female, data = dt_survey_EST_onceprev,
            exact = FALSE)
t.test(sat_fact_allq ~ female, data = dt_survey_EST_onceprev, var.equal = TRUE)


sat_fact_allq
wilcox_effsize(x, y, rscale = "n", method = "r")

#Number of children
cor.test(dt_survey_EST_1$counts_after_later_1, dt_survey_EST_1$totalchildren, 
         method = "pearson")
cor.test(dt_survey_EST_1$counts_after_later_1, dt_survey_EST_1$totalchildren, 
         method = "spearman")

dt_survey_EST_1$children_groups <- ifelse(dt_survey_EST_1$totalchildren == 0, 1, ifelse(dt_survey_EST_1$totalchildren >= 1 & dt_survey_EST_1$totalchildren <= 2, 2, 
                                                                                        ifelse(dt_survey_EST_1$totalchildren >= 3, 3, NA)))
dt_survey_EST_1$children_0 <- ifelse(dt_survey_EST_1$children_groups == 1, 1, ifelse(dt_survey_EST_1$children_groups >=2 & dt_survey_EST_1$children_groups <=3, 0, NA))
dt_survey_EST_1$children_12 <- ifelse(dt_survey_EST_1$children_groups == 2, 1, 
                                      ifelse(dt_survey_EST_1$children_groups == 1, 0, 
                                             ifelse(dt_survey_EST_1$children_groups == 3, 0, NA)))
dt_survey_EST_1$children_3 <- ifelse(dt_survey_EST_1$children_groups == 3, 1, ifelse(dt_survey_EST_1$children_groups >=1 & dt_survey_EST_1$children_groups <=2, 0, NA))

chisq_result_child <- chisq.test(table(dt_survey_EST$sat_fact_allq, dt_survey_EST_1$children_groups))
child_change <- round(prop.table(table(dt_survey_EST_1$children_groups, dt_survey_EST_1$sat_fact_allq), margin = 1), 4)
rownames(child_change) <- c("0 children", "1-2 children", "3 or more children")
colnames(child_change) <- c("No back and change", "Back and change")
child_change
chisq_result <- chisq.test(table(dt_survey_EST_1$sat_fact_allq, dt_survey_EST_1$children_groups))

install.packages("lsr")
anova_children_all <- aov(counts_after_later_1 ~ children_groups, data = dt_survey_EST_1)
TukeyHSD(aov(counts_after_later_1 ~ as.factor(children_groups), data = dt_survey_EST_1))
f <- effectsize::cohens_f(anova_children_all)

summary(aov(sat_fact_allq ~ children_groups, data = dt_survey_EST_1))
TukeyHSD(aov(sat_fact_allq ~ as.factor(children_groups), data = dt_survey_EST_1))

chisq_result_child <- chisq.test(table(dt_survey_EST_onceprev$sat_fact_allq, dt_survey_EST_onceprev$children_groups))
child_change <- round(prop.table(table(dt_survey_EST_onceprev$children_groups, dt_survey_EST_onceprev$sat_fact_allq), margin = 1), 4)
rownames(child_change) <- c("0 children", "1-2 children", "3 or more children")
colnames(child_change) <- c("No back and change", "Back and change")
child_change

summary(aov(sat_fact_allq ~ children_groups, data = dt_survey_EST_onceprev))
TukeyHSD(aov(sat_fact_allq ~ as.factor(children_groups), data = dt_survey_EST_onceprev))
f <- effectsize::cohens_f(aov(sat_fact_allq ~ children_groups, data = dt_survey_EST_onceprev))


#Age
cor.test(dt_survey_EST_1$counts_after_later_1, dt_survey_EST_1$age, 
         method = "pearson")
cor.test(dt_survey_EST_1$counts_after_later_1, dt_survey_EST_1$age, 
         method = "spearman")
#age groups (18-25, 26-35, 36-45, 46-65, 65+)
dt_survey_EST_1$age_groups[dt_survey_EST_1$age<= 25 & dt_survey_EST_1$age >= 18] <- "1"
dt_survey_EST_1$age_groups[dt_survey_EST_1$age<= 35 & dt_survey_EST_1$age >= 26] <- "2"
dt_survey_EST_1$age_groups[dt_survey_EST_1$age<= 45 & dt_survey_EST_1$age >= 36] <- "3"
dt_survey_EST_1$age_groups[dt_survey_EST_1$age<= 59 & dt_survey_EST_1$age >= 46] <- "4"
dt_survey_EST_1$age_groups[dt_survey_EST_1$age>=60] <- "5"

chisq_result <- chisq.test(table(dt_survey_EST_1$sat_fact_allq, dt_survey_EST_1$age_groups))
age_change <- round(prop.table(table(dt_survey_EST_1$age_groups, dt_survey_EST_1$sat_fact_allq), margin = 1), 4)
rownames(age_change) <- c("18-25", "26-35", "36-45", "46-65", "65+")
colnames(age_change) <- c("No back and change", "Back and change")
age_change
chisq_result <- chisq.test(table(dt_survey_EST_1$sat_fact_allq, dt_survey_EST_1$age_groups))

summary(aov(sat_fact_allq ~ age_groups, data = dt_survey_EST_1))
TukeyHSD(aov(sat_fact_allq ~ age_groups, data = dt_survey_EST_1))
effectsize::cohens_f(aov(counts_after_later_1 ~ age_groups, data = dt_survey_EST_1))

chisq_result <- chisq.test(table(dt_survey_EST_onceprev$sat_fact_allq, dt_survey_EST_onceprev$age_groups))
age_change <- round(prop.table(table(dt_survey_EST_onceprev$age_groups, dt_survey_EST_onceprev$sat_fact_allq), margin = 1), 4)
rownames(age_change) <- c("18-25", "26-35", "36-45", "46-65", "65+")
colnames(age_change) <- c("No back and change", "Back and change")
age_change

summary(aov(counts_after_later_1 ~ age_groups, data = dt_survey_EST_onceprev))
TukeyHSD(aov(counts_after_later_1 ~ age_groups, data = dt_survey_EST_onceprev))
effectsize::cohens_f(aov(sat_fact_allq ~ age_groups, data = dt_survey_EST_onceprev))

#Has work
wilcox.test(counts_after_later_1 ~ wrk02_rec, data = dt_survey_EST_1,
            exact = FALSE)
t.test(counts_after_later_1 ~ wrk02_rec, data = dt_survey_EST_1, var.equal = TRUE)
round(prop.table(table(dt_survey_EST_1$wrk02_rec, dt_survey_EST_1$sat_fact_allq), margin = 2), 4)

wilcox.test(sat_fact_allq ~ wrk02_rec, data = dt_survey_EST_1,
            exact = FALSE)
t.test(sat_fact_allq ~ wrk02_rec, data = dt_survey_EST_1, var.equal = TRUE)



round(prop.table(table(dt_survey_EST_onceprev$wrk02_rec, dt_survey_EST_onceprev$sat_fact_allq), margin = 2), 4)

wilcox.test(sat_fact_allq ~ wrk02_rec, data = dt_survey_EST_onceprev,
            exact = FALSE)
t.test(sat_fact_allq ~ wrk02_rec, data = dt_survey_EST_onceprev, var.equal = TRUE)
cohens_d(sat_fact_allq ~ wrk02_rec, data = dt_survey_EST_onceprev)



##Has partner
wilcox.test(counts_after_later_1 ~ dem21_rec, data = dt_survey_EST_1,
            exact = FALSE)
t.test(counts_after_later_1 ~ dem21_rec, data = dt_survey_EST_1, var.equal = TRUE)

wilcox.test(sat_fact_allq ~ dem21_rec, data = dt_survey_EST_1,
            exact = FALSE)
t.test(sat_fact_allq ~ dem21_rec, data = dt_survey_EST_1, var.equal = TRUE)

wilcox.test(sat_fact_allq ~ dem21_rec, data = dt_survey_EST_onceprev,
            exact = FALSE)
t.test(sat_fact_allq ~ dem21_rec, data = dt_survey_EST_onceprev, var.equal = TRUE)
cohens_d(sat_fact_allq ~ dem21_rec, data = dt_survey_EST_onceprev)



##Education
dt_survey_EST_1$education[dt_survey_EST_1$dem07<= 4] <- "1"
dt_survey_EST_1$education[dt_survey_EST_1$dem07>=5 & dt_survey_EST_1$dem07<=7] <- "2"
dt_survey_EST_1$education[dt_survey_EST_1$dem07==8] <- "3"
dt_survey_EST_1$education[dt_survey_EST_1$dem07>=9] <- "4"

summary(aov(counts_after_later_1 ~ education, data = dt_survey_EST_1))
summary(aov(sat_fact_allq ~ education, data = dt_survey_EST_1))
effectsize::cohens_f(aov(sat_fact_allq ~ education, data = dt_survey_EST_1))
round(prop.table(table(dt_survey_EST_1$sat_fact_allq, dt_survey_EST_1$education), margin = 2), 3)


prop.table(table(dt_survey_EST_1$counts_after_later_1, dt_survey_EST_1$education))
cor.test(dt_survey_EST_1$counts_after_later_1, as.numeric(dt_survey_EST_1$education), 
         method = "pearson")
cor.test(dt_survey_EST_1$counts_after_later_1, as.numeric(dt_survey_EST_1$education), 
         method = "spearman")

summary(aov(sat_fact_allq ~ education, data = dt_survey_EST_onceprev))
round(prop.table(table(dt_survey_EST_onceprev$sat_fact_allq, dt_survey_EST_onceprev$education), margin = 2), 3)
effectsize::cohens_f(aov(sat_fact_allq ~ education, data = dt_survey_EST_onceprev))



##Filled in one sitting
wilcox.test(counts_after_later_1 ~ filled_in_one_sitting_updtd, data = dt_survey_EST_1,
            exact = FALSE)
t.test(counts_after_later_1 ~ filled_in_one_sitting_updtd, data = dt_survey_EST_1, var.equal = TRUE)

wilcox.test(sat_fact_allq ~ filled_in_one_sitting_updtd, data = dt_survey_EST_1,
            exact = FALSE)
t.test(sat_fact_allq ~ filled_in_one_sitting_updtd, data = dt_survey_EST_1, var.equal = TRUE)

wilcox.test(sat_fact_allq ~ filled_in_one_sitting_updtd, data = dt_survey_EST_onceprev,
            exact = FALSE)
t.test(sat_fact_allq ~ filled_in_one_sitting_updtd, data = dt_survey_EST_onceprev, var.equal = TRUE)
cohens_d(sat_fact_allq ~ filled_in_one_sitting_updtd, data = dt_survey_EST_onceprev)

##Number of pages
cor.test(dt_survey_EST_1$counts_after_later_1, dt_survey_EST_1$count_updatepage.y, 
         method = "pearson")
cor.test(dt_survey_EST_1$counts_after_later_1, dt_survey_EST_1$count_updatepage.y, 
         method = "spearman")

cor.test(dt_survey_EST_1$sat_fact_allq, dt_survey_EST_1$count_updatepage.y, 
         method = "pearson")
cor.test(dt_survey_EST_1$sat_fact_allq, dt_survey_EST_1$count_updatepage.y, 
         method = "spearman")

wilcox.test(count_updatepage.y  ~ sat_fact_allq, data = dt_survey_EST_1,
            exact = FALSE)
t.test(count_updatepage.y ~ sat_fact_allq, data = dt_survey_EST_1, var.equal = TRUE)

wilcox.test(count_updatepage.y  ~ sat_fact_allq, data = dt_survey_EST_onceprev,
            exact = FALSE)
t.test(count_updatepage.y ~ sat_fact_allq, data = dt_survey_EST_onceprev, var.equal = TRUE)
cohens_d(count_updatepage.y ~ sat_fact_allq, data = dt_survey_EST_onceprev)

aggregate(count_updatepage.y ~ sat_fact_allq, data = dt_survey_EST_1, 
          FUN = function(x) c(mean = mean(x), sd = sd(x)))

aggregate(count_updatepage.y ~ sat_fact_allq, data = dt_survey_EST_onceprev, 
          FUN = function(x) c(mean = mean(x), sd = sd(x)))

##Proportion of satisficing
dt_survey_EST_1$satisficing_prop <- dt_survey_EST_1$counts_after_later_1/dt_survey_EST_1$count_updatepage.y

#Female
wilcox.test(satisficing_prop ~ female, data = dt_survey_EST_1,
            exact = FALSE)
t.test(satisficing_prop ~ female, data = dt_survey_EST_1, var.equal = TRUE)

#Number of children
cor.test(dt_survey_EST_1$satisficing_prop, dt_survey_EST_1$totalchildren, 
         method = "pearson")
cor.test(dt_survey_EST_1$satisficing_prop, dt_survey_EST_1$totalchildren, 
         method = "spearman")

#Age
cor.test(dt_survey_EST_1$satisficing_prop, dt_survey_EST_1$age, 
         method = "pearson")
cor.test(dt_survey_EST_1$satisficing_prop, dt_survey_EST_1$age, 
         method = "spearman")
summary(aov(satisficing_prop ~ age_groups, data = dt_survey_EST_1))
TukeyHSD(aov(satisficing_prop ~ age_groups, data = dt_survey_EST_1))

#Has work
wilcox.test(satisficing_prop ~ wrk02_rec, data = dt_survey_EST_1,
            exact = FALSE)
t.test(satisficing_prop ~ wrk02_rec, data = dt_survey_EST_1, var.equal = TRUE)

##Has partner
wilcox.test(satisficing_prop ~ dem21_rec, data = dt_survey_EST_1,
            exact = FALSE)
t.test(satisficing_prop ~ dem21_rec, data = dt_survey_EST_1, var.equal = TRUE)

##Education
summary(aov(satisficing_prop ~ education, data = dt_survey_EST_1))
cor.test(dt_survey_EST_1$satisficing_prop, as.numeric(dt_survey_EST_1$education), 
         method = "pearson")
cor.test(dt_survey_EST_1$satisficing_prop, as.numeric(dt_survey_EST_1$education), 
         method = "spearman")

##Filled in one sitting
wilcox.test(satisficing_prop ~ filled_in_one_sitting_updtd, data = dt_survey_EST_1,
            exact = FALSE)
t.test(satisficing_prop ~ filled_in_one_sitting_updtd, data = dt_survey_EST_1, var.equal = TRUE)

#Smartpone/Computer (device)
wilcox.test(satisficing_prop ~ smartphone, data = dt_survey_EST_onceprev,
            exact = FALSE)
t.test(satisficing_prop ~ smartphone, data = dt_survey_EST_onceprev, var.equal = TRUE)
cohens_d(satisficing_prop ~ smartphone, data = dt_survey_EST_onceprev)

wilcox.test(sat_fact_allq ~ smartphone, data = dt_survey_EST_onceprev,
            exact = FALSE)
t.test(sat_fact_allq ~ smartphone, data = dt_survey_EST_onceprev, var.equal = TRUE)
cohens_d(sat_fact_allq ~ smartphone, data = dt_survey_EST_onceprev)


##Number of pages
cor.test(dt_survey_EST_1$satisficing_prop, dt_survey_EST_1$count_updatepage.y, 
         method = "pearson")
cor.test(dt_survey_EST_1$satisficing_prop, dt_survey_EST_1$count_updatepage.y, 
         method = "spearman")




##Effect sizes

cohens_d(satisficing_prop ~ female, data = dt_survey_EST_1)
cohens_d(counts_after_later_1 ~ female, data = dt_survey_EST_1)

eta_squared(aov(counts_after_later_1 ~ age_groups, data = dt_survey_EST_1), partial = TRUE)

cohens_d(counts_after_later_1 ~ wrk02_rec, data = dt_survey_EST_1)
cohens_d(satisficing_prop ~ wrk02_rec, data = dt_survey_EST_1)

cohens_d(counts_after_later_1 ~ dem21_rec, data = dt_survey_EST_1)
cohens_d(satisficing_prop ~ dem21_rec, data = dt_survey_EST_1)

cohens_d(counts_after_later_1 ~ filled_in_one_sitting_updtd, data = dt_survey_EST_1)
cohens_d(satisficing_prop ~ filled_in_one_sitting_updtd, data = dt_survey_EST_1)

table(dt_survey_EST_1$counts_after_later_1)
dt_survey_EST_1$sat_fact_allq <- ifelse(dt_survey_EST_1$counts_after_later_1 >0, 1, ifelse(dt_survey_EST_1$counts_after_later_1 == 0, 0, NA))
table(dt_survey_EST_1$sat_fact_allq)


##Add Dem14, Dem15
wilcox.test(sat_fact_allq ~ female, data = dt_survey_EST_onceprev,
            exact = FALSE)
t.test(sat_fact_allq ~ female, data = dt_survey_EST_onceprev, var.equal = TRUE)

table(dt_survey_EST_onceprev$dem15)

#dem15 groups
dt_survey_EST_1$dem15_12 <- rec(dt_survey_EST_1$dem15, rec = "0:1=1; else=0")
dt_survey_EST_1$dem15_3 <- rec(dt_survey_EST_1$dem15, rec = "2=1; else=0")
dt_survey_EST_1$dem15_4 <- rec(dt_survey_EST_1$dem15, rec = "3=1; else=0")
dt_survey_EST_1$dem15_5 <- rec(dt_survey_EST_1$dem15, rec = "4=1; else=0")

#dem groups 
dt_survey_EST_onceprev$dem15_groups[dt_survey_EST_onceprev$dem15 <= 1 & dt_survey_EST_onceprev$dem15 >= 0] <- "1"
dt_survey_EST_onceprev$dem15_groups[dt_survey_EST_onceprev$dem15 == 2] <- "2"
dt_survey_EST_onceprev$dem15_groups[dt_survey_EST_onceprev$dem15 == 3] <- "3"
dt_survey_EST_onceprev$dem15_groups[dt_survey_EST_onceprev$dem15 == 4] <- "4"


summary(m2 <- zeroinfl(counts_previous.y ~ female + 
                           children_12 + children_3 + edu_scndr_1 + edu_bachelor_1 + 
                           edu_masters_and_higher + age_26_35 + age_36_45 + age_46_59 + 
                           age_60plus + wrk02_rec + dem21_rec + filled_in_one_sitting_updtd + 
                           count_updatepage.y + dem15_3 + dem15_4 +dem15_5, data = dt_survey_EST_1))


summary(aov(sat_fact_allq ~ dem15_groups, data = dt_survey_EST_onceprev))

chisq_result <- chisq.test(table(dt_survey_EST_onceprev$sat_fact_allq, dt_survey_EST_onceprev$dem15_groups))


TukeyHSD(aov(counts_after_later_1 ~ dem15_groups, data = dt_survey_EST_onceprev))
effectsize::cohens_f(aov(sat_fact_allq ~ dem15_groups, data = dt_survey_EST_onceprev))

##Add device

para_estfltr_ordered_onlystart <-para_estfltr_ordered[(para_estfltr_ordered$startsession=="TRUE"),]

#Extract the "OS=" text
para_estfltr_ordered_onlystart$os_text <- str_extract(para_estfltr_ordered_onlystart$v4, "(?<=OS=\\\")([^\\\"]+)")

#Extract the device based on the OS text
get_device <- function(os_text) {
    if (str_detect(os_text, "Android")) {
        return("Smartphone")
    } else if (str_detect(os_text, "iPhone|iPad|iPod")) {
        return("Smartphone")
    } else if (str_detect(os_text, "Windows")) {
        return("Computer")
    } else if (str_detect(os_text, "Macintosh|Mac OS X")) {
        return("Computer")
    } else {
        return("Tablet")
    }
}

para_estfltr_ordered_onlystart$device <- sapply(str_extract_all(para_estfltr_ordered_onlystart$v4, "(?<=OS=\\\")([^\\\"]+)"), get_device)

device <- gsub('.*\\((.*?)\\).*', '\\1', para_estfltr_ordered_onlystart$v4)

device <- ifelse(grepl('iPhone|iOS', device), 'Smartphone',
                 ifelse(grepl('Android', device), 'Smartphone',
                        ifelse(grepl('Macintosh|Mac OS', device), 'Computer',
                               ifelse(grepl('Windows', device), 'Computer',
                                      ifelse(grepl('iPad|Tablet', device), 'Tablet', 'Unknown')))))

para_estfltr_ordered_onlystart$device <- device


width <- str_extract(para_estfltr_ordered_onlystart$v4, "(?<=Width=\\\")[0-9]+")
height <- str_extract(para_estfltr_ordered_onlystart$v4, "(?<=Height=\\\")[0-9]+")

para_estfltr_ordered_onlystart$width <- as.numeric(width)
para_estfltr_ordered_onlystart$height <- as.numeric(height)

mean_width <- aggregate(para_estfltr_ordered_onlystart$width, by = list(para_estfltr_ordered_onlystart$device), FUN = mean)
mean_height <- aggregate(para_estfltr_ordered_onlystart$height, by = list(para_estfltr_ordered_onlystart$device), FUN = mean)

colnames(mean_width) <- c("Device", "Mean Width")
colnames(mean_height) <- c("Device", "Mean Height")

mean_width
mean_height

max_width <- aggregate(para_estfltr_ordered_onlystart$width, by = list(para_estfltr_ordered_onlystart$device), FUN = max)
min_width <- aggregate(para_estfltr_ordered_onlystart$width, by = list(para_estfltr_ordered_onlystart$device), FUN = min)
max_height <- aggregate(para_estfltr_ordered_onlystart$height, by = list(para_estfltr_ordered_onlystart$device), FUN = max)
min_height <- aggregate(para_estfltr_ordered_onlystart$height, by = list(para_estfltr_ordered_onlystart$device), FUN = min)

colnames(max_width) <- c("Device", "Max Width")
colnames(min_width) <- c("Device", "Min Width")
colnames(max_height) <- c("Device", "Max Height")
colnames(min_height) <- c("Device", "Min Height")

max_width
min_width
max_height
min_height

length(unique(para_estfltr_ordered_onlystart$respid))

para_estfltr_ordered_onlystart$v3 <- as.POSIXct(para_estfltr_ordered_onlystart$v3)
para_estfltr_ordered_onlystart_aggregated <- para_estfltr_ordered_onlystart %>%
    group_by(respid) %>%
    filter(v3 == max(v3)) %>%
    select(respid, device) %>%
    distinct()
View(para_estfltr_ordered_onlystart_aggregated)
freq_table <- table(para_estfltr_ordered_onlystart$respid)
sorted_freq <- sort(freq_table, decreasing = TRUE)
top_five <- head(names(sorted_freq), 5)
print(top_five)

respid_counts <- para_estfltr_ordered_onlystart %>%
    group_by(respid) %>%
    summarise(unique_devices = n_distinct(device))

respid_values_with_multiple_devices <- respid_counts %>%
    filter(unique_devices > 1) %>%
    pull(respid)

# Add "device_completion" variable to dt_survey_EST_onceprev
dt_survey_EST_onceprev <- merge(dt_survey_EST_onceprev, para_estfltr_ordered_onlystart_aggregated[, c("respid", "device")], by = "respid", all.x = TRUE)

# Rename the "device" column to "device_completion"
colnames(dt_survey_EST_onceprev)[colnames(dt_survey_EST_onceprev) == "device"] <- "device_completion"

# Add "device_completion" variable to dt_survey_EST_1
dt_survey_EST_1 <- merge(dt_survey_EST_1, para_estfltr_ordered_onlystart_aggregated[, c("respid", "device")], by = "respid", all.x = TRUE)

# Rename the "device" column to "device_completion"
colnames(dt_survey_EST_1)[colnames(dt_survey_EST_1) == "device"] <- "device_completion"

dt_survey_EST_onceprev$device_completion <- ifelse(dt_survey_EST_onceprev$device_completion == "Unknown", "Computer", dt_survey_EST_onceprev$device_completion)
dt_survey_EST_1$device_completion <- ifelse(dt_survey_EST_1$device_completion == "Unknown", "Computer", dt_survey_EST_1$device_completion)

dt_survey_EST_onceprev$smartphone <- ifelse(dt_survey_EST_onceprev$device_completion == "Smartphone", 1, ifelse(dt_survey_EST_onceprev$device_completion == "Computer", 0, NA))
dt_survey_EST_1$smartphone <- ifelse(dt_survey_EST_1$device_completion == "Smartphone", 1, ifelse(dt_survey_EST_1$device_completion == "Computer", 0, NA))

summary(m2 <- zeroinfl(counts_previous.y ~ female + 
                           children_12 + children_3 + edu_scndr_1 + edu_bachelor_1 + 
                           edu_masters_and_higher + age_26_35 + age_36_45 + age_46_59 + 
                           age_60plus + wrk02_rec + dem21_rec + dem15_3 + dem15_4 +dem15_5 + smartphone, data = dt_survey_EST_1))

summary(m2 <- zeroinfl(counts_previous.y ~ female + 
                           children_12 + children_3 + edu_scndr_1 + edu_bachelor_1 + 
                           edu_masters_and_higher + age_26_35 + age_36_45 + age_46_59 + 
                           age_60plus + wrk02_rec + dem21_rec + dem15_3 + dem15_4 +dem15_5 + smartphone + filled_in_one_sitting_updtd + 
                           count_updatepage.y, data = dt_survey_EST_1))

# Remove missing values from the data
dt_survey_EST_1_clean <- na.omit(dt_survey_EST_1$counts_previous.y)

# Fit Poisson distribution
fit_poisson <- fitdist(as.numeric(dt_survey_EST_1_clean), "pois")

# Fit negative binomial distribution
fit_nb <- fitdist(as.numeric(dt_survey_EST_1_clean), "nbinom")

# Create density plots
plot(density(dt_survey_EST_1_clean), main = "Density Plot of Counts",
     xlab = "Counts", ylab = "Density", col = "skyblue", xlim = c(0, 100))
lines(density(rpois(10000, fit_poisson$estimate["lambda"])), col = "red", lwd = 2)
lines(density(rnbinom(10000, size = fit_nb$estimate["size"], mu = fit_nb$estimate["mu"])), col = "blue", lwd = 2)
legend("topright", legend = c("Observed Data", "Poisson Distribution", "Negative Binomial Distribution"),
       col = c("skyblue", "red", "blue"), lwd = 2)


# Perform the Kolmogorov-Smirnov test for the Poisson distribution
ks_test_poisson <- ks.test(dt_survey_EST_1_clean_unique, "ppois", fit_poisson$estimate["lambda"])

# Perform the Kolmogorov-Smirnov test for the Negative Binomial distribution
ks_test_nb <- ks.test(dt_survey_EST_1_clean_unique, "pnbinom", size = fit_nb$estimate["size"], mu = fit_nb$estimate["mu"])


standard_model <- glm(counts_previous.y ~ female + children_12 + children_3 + edu_scndr_1 + 
                          edu_bachelor_1 + edu_masters_and_higher + age_26_35 + age_36_45 + 
                          age_46_59 + age_60plus + wrk02_rec + dem21_rec + dem15_3 + dem15_4 + 
                          dem15_5 + smartphone + filled_in_one_sitting_updtd + count_updatepage.y, 
                      data = dt_survey_EST_1, family = poisson)

zero_inflated_model <- zeroinfl(counts_previous.y ~ female + children_12 + children_3 + 
                                    edu_scndr_1 + edu_bachelor_1 + edu_masters_and_higher + 
                                    age_26_35 + age_36_45 + age_46_59 + age_60plus + wrk02_rec + 
                                    dem21_rec + dem15_3 + dem15_4 + dem15_5 + smartphone + 
                                    filled_in_one_sitting_updtd + count_updatepage.y, 
                                data = dt_survey_EST_1)
vuong_test <- vuong(zero_inflated_model, standard_model)
print(vuong_test)

standard_model <- glm(counts_previous.y ~ female + children_12 + children_3 + edu_scndr_1 + 
                          edu_bachelor_1 + edu_masters_and_higher + age_26_35 + age_36_45 + 
                          age_46_59 + age_60plus + wrk02_rec + dem21_rec + dem15_3 + dem15_4 + 
                          dem15_5 + smartphone + filled_in_one_sitting_updtd + count_updatepage.y, 
                      data = dt_survey_EST_1, link = logit)
zero_inflated_model <- zeroinfl(counts_previous.y ~ female + children_12 + children_3 + 
                                    edu_scndr_1 + edu_bachelor_1 + edu_masters_and_higher + 
                                    age_26_35 + age_36_45 + age_46_59 + age_60plus + wrk02_rec + 
                                    dem21_rec + dem15_3 + dem15_4 + dem15_5 + smartphone + 
                                    filled_in_one_sitting_updtd + count_updatepage.y, 
                                data = dt_survey_EST_1)
vuong_test <- vuong(zero_inflated_model, standard_model)
print(vuong_test)



jpeg("Distribution.jpg", width = 800, height = 600, quality = 10)

# Create histogram
hist(dt_survey_EST_1_clean, main = "Distribution of number of revistis to all types of questions",
     xlab = "Number of revisits", ylab = "Frequency", col = "white", xlim = c(0, 40), ylim = c(0, 5000), breaks = 400)

# Add x-axis labels
axis(side = 1, at = seq(0, 40, by = 5))
dev.off()
