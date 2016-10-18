# explore the predictors of income using t-tests and ANOVA
# TODO how to incorporate weights and repeated observations?
library("ff")
library("ffbase")
library("ggplot2")
library("stats")
library("nortest")

filename <- "/home/eli/Data/ACS/people.ff"
acsData <- read.csv.ffdf(file=filename)

income <- as.ram(acsData$totalIncome)
indices <- which(!is.na(income)) # only people who have an income
income <- income[indices]

gender <- as.factor(as.ram(acsData$gender[indices]))
people <- data.frame(income, gender)

# does gender predict a significant difference in total income?
# SPOILERS: yes DOUBLE SPOILERS: women have less income than men
# since the ACS has two genders, this gives two groups and we can potential use a two sample t test
# test for t-test assumptions:
# are the variances the same? use Fisher's F test
var.test(income~gender) # no, they are not, since p < 0.05
# are distributions close to normal? use the Cramer-von Mises test for normality
# total income is signed, so it can be negative
cvm.test(income) # not even close
cvm.test(income[which(gender==1)]) # not even close
cvm.test(income[which(gender==2)]) # not even close
# we really shouldn't use a two sample t test, but what if we do anyway?
t.test(income~gender, var.equal=FALSE) # means are significantly different
t.test(income~gender, var.equal=FALSE, alternative="greater") # men have a significantly greater income than women
# t test was not appropriate, what if we use the Wilcocon-Mann-Whitney test instead?
wilcox.test(income~gender) # data are significantly different
wilcox.test(income~gender, alternative="greater") # men have a significantly greater income than women

# what about for eduation? more than two education groups, so we need ANOVA
# what about using R's stats package to do this?
# first, use the Fligner-Killeen test to determine if ANOVA is appropriate
# could also use Bartlett's K-squared, but that seems buggy in this implementation
fligner.test(people, gender) # we actually have to reject the null hypothesis; the variances are not homogeneous
# try doing it anyway
fit <- lm(formula = people ~ gender)
