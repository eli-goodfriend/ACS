# explore the predictors of income using ANOVA
# TODO how to incorporate weights and repeated observations?
library("ff")
library("ffbase")
library("ggplot2")
library("stats")

filename <- "/home/eli/Data/ACS/people.ff"
acsData <- read.csv.ffdf(file=filename)

income <- as.ram(acsData$totalIncome)
indices <- which(!is.na(income))
income <- income[indices]

gender <- as.factor(as.ram(acsData$gender[indices]))
people <- data.frame(income, gender)

# does gender generate a significant different in total income?
# since the ACS has two genders, this gives two groups and we can use a two sample t test


# what about for eduation? more than two education groups, so we need ANOVA
# what about using R's stats package to do this?
# first, use the Fligner-Killeen test to determine if ANOVA is appropriate
# could also use Bartlett's K-squared, but that seems buggy in this implementation
fligner.test(people, gender) # we actually have to reject the null hypothesis; the variances are not homogeneous
# try doing it anyway
fit <- lm(formula = people ~ gender)
