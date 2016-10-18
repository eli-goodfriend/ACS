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
education <- as.factor(as.ram(acsData$highestDegree[indices]))

remove(acsData) # free memory

# does gender predict a significant difference in total income?
# SPOILERS: yes DOUBLE SPOILERS: women have less income than men
boxplot(income~gender, notch=TRUE, varwidth=TRUE, outline=FALSE,
        names=c("Men","Women"), ylab="Total income ($/year)")
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

# what about for eduation? more than two education groups, so we need one-way ANOVA
# SPOILERS: yes income is significantly different for different education levels
# preprocess education to make more meaningful bins and order the factors
source("~/Dropbox/Code/ACS/preprocess/group_education.R")
education <- group_education(education)
boxplot(income~education, notch=TRUE, varwidth=TRUE, outline=FALSE,
        ylab="Total income ($/year)",
        names=c("No diploma or GED","High school","Some college",
                "Assoc.","Bach.","Mast.","Professional","PhD"))
# use Bartlett's K-squared to determine if variances are homogeneous
bartlett.test(income~education) # no, they are not
# TODO are the data normally distributed? that seems very unlikely, but can check
# try to use ANOVA anyway
#fit <- lm(income~education)
#anova(fit) # means are significantly different, education significantly affects income
# does this do the same thing?
fit_alt <- aov(income~education)
summary(fit_alt) # seems like same results as using lm and anova
#TukeyHSD(fit_alt) # can't do, data is too big
# ANOVA was not appropriate, what if we use a non-parametric test?
# let's try Kruskal Wallis
kruskal.test(income~education) # yup, education generates significantly different income




