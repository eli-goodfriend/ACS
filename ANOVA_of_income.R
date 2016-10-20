# explore the predictors of income
# DANGER this doesn't know about weights, so it is not as accurate as it could be
# working on that in ANOVA_of_income_weighted.R
# TODO rm c/p so fewer errors
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
industry <- as.factor(as.ram(acsData$industry[indices]))
degree <- as.factor(as.ram(acsData$fieldOfDegree1[indices]))
degree2 <- as.factor(as.ram(acsData$fieldOfDegree2[indices]))

remove(acsData) # free memory

# does gender predict a significant difference in total income?
# SPOILERS: yes DOUBLE SPOILERS: women have less income than men
png(file="incomeGender.png")
par(mar=c(2.1,4.1,1.1,1.1))
boxplot(income~gender, notch=TRUE, varwidth=TRUE, outline=FALSE,
        names=c("Men","Women"), ylab="Total income ($/year)")
dev.off()
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
# t test was not appropriate, what if we use the Wilcoxon-Mann-Whitney test instead?
wilcox.test(income~gender) # data are significantly different
wilcox.test(income~gender, alternative="greater") # men have a significantly greater income than women

# Is there a difference in income between a high school diploma and a GED?
# let's just skip to the non-parametric test
HSidx <- which(education==16 | education==17)
HSincome <- income[HSidx]
HSeducation <- factor(education[HSidx])
png(file="incomeDiploma.png")
par(mar=c(2.1,4.1,1.1,1.1))
boxplot(HSincome~HSeducation, notch=TRUE, varwidth=TRUE, outline=FALSE,
        ylab="Total income ($/year)",
        names=c("Diploma","GED"))
dev.off()
wilcox.test(HSincome~HSeducation, alternative="greater") # diploma is better
aggregate(HSincome, by=list(HSeducation), FUN=quantile, probs=seq(0,1,0.25))

# Is there a difference in income between finishing high school and not finishing college?
SCidx <- which(education==18 | education==19)
SCincome <- income[SCidx]
png(file="incomeSomeCollege.png")
par(mar=c(2.1,4.1,1.1,1.1))
boxplot(HSincome, SCincome, notch=TRUE, varwidth=TRUE, outline=FALSE,
        ylab="Total income ($/year)",
        names=c("High school","Some college"))
dev.off()
wilcox.test(HSincome, SCincome, alternative="less") # some college is better
quantile(HSincome)
quantile(SCincome)

# what about for eduation overall? more than two education groups, so we need one-way ANOVA
# SPOILERS: yes income is significantly different for different education levels
# preprocess education to make more meaningful bins and order the factors
source("~/Dropbox/Code/ACS/preprocess/group_education.R")
education <- group_education(education)
png(file="incomeEducation.png")
par(mar=c(10.1,4.1,1.1,1.1))
boxplot(income~education, notch=TRUE, varwidth=TRUE, outline=FALSE,
        ylab="Total income ($/year)", las=3,
        names=c("No diploma or GED","High school","Some college",
                "Associates","Bachelors","Masters","Professional","Doctoral"))
dev.off()
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
aggregate(income, by=list(education), FUN=quantile, probs=seq(0,1,0.25))


# given that you have a degree, how does the type of degree affect your income?
degIdx <- which(!is.na(degree))
degree <- degree[degIdx]
source("~/Dropbox/Code/ACS/preprocess/group_degree.R")
degree <- group_degree(degree)
incomeWithDegree <- income[degIdx]
png(file="incomeDegree.png")
par(mar=c(10.1,4.1,1.1,1.1))
boxplot(incomeWithDegree~degree, notch=TRUE, varwidth=TRUE, outline=FALSE,
        ylab="Total income ($/year)", las=3)
dev.off()
kruskal.test(incomeWithDegree~degree) # yup, your degree matters
aggregate(incomeWithDegree, by=list(degree), FUN=quantile, probs=seq(0,1,0.25))


# given that you work in an industry, how does it affect your income?
indIdx <- which(!is.na(industry))
industry <- industry[indIdx]
source("~/Dropbox/Code/ACS/preprocess/group_industry.R")
industry <- group_industry(industry)
ag <- aggregate(incomeWithIndustry, by=list(industry), FUN=quantile, probs=seq(0,1,0.25))
ag <- ag[order(ag$x[,3]),] # reorder by median
industry <- ordered(industry, levels=ag$Group.1)
incomeWithIndustry <- income[indIdx]
png(file="incomeIndustry.png")
par(mar=c(14.1,4.1,1.1,1.1))
boxplot(incomeWithIndustry~industry, notch=TRUE, varwidth=TRUE, outline=FALSE,
        ylab="Total income ($/year)", las=3)
dev.off()
kruskal.test(incomeWithIndustry~industry) # yup, your industry matters

