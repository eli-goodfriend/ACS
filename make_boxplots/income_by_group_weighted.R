# explore the predictors of income using t-tests and ANOVA
# TODO how to incorporate repeated observations?
# TODO try sqlsurvey instead, making the ACS into a database, because this is memory hogging
library("ff")
library("ffbase")
library("ggplot2")
library("stats")
library("nortest")
library("survey")

# read in and preprocess data
filename <- "/home/eli/Data/ACS/people.ff"
acsData <- read.csv.ffdf(file=filename)

income <- as.ram(acsData$totalIncome)
indices <- which(!is.na(income)) # only people who have an income
income <- income[indices]

gender <- as.factor(as.ram(acsData$gender[indices]))

education <- as.factor(as.ram(acsData$highestDegree[indices]))
source("~/Dropbox/Code/ACS/preprocess/group_education.R")
education <- group_education(education)

weight <- as.ram(acsData$weight[indices])

people <- data.frame(income, gender, education, weight)

remove(acsData) # free memory

# make a survey design so I can use the survey package for weighted analysis
design <- svydesign(id=~1, data=people, weights=weight)

# overall distribution of income
svyquantile(~income, design, quantile = c(0.25, 0.5, 0.75), ci = TRUE) # sophisticated
quantile(income) # naive

# does gender predict a significant difference in total income?
# SPOILERS: yes 
svyboxplot(income~gender, design, varwidth=TRUE, outline=FALSE,
        names=c("Men","Women"), ylab="Total income ($/year)")
svyby(~income, ~gender, design, svyquantile, quantile=c(0.25,0.5,0.75), ci=TRUE, vartype="ci")
svyranktest(income~gender, design, test="wilcoxon") # datasets are significantly different

# what about for eduation overall?
# SPOILERS: yes income is significantly different for different education levels
svyboxplot(income~education, design, varwidth=TRUE, outline=FALSE,
        ylab="Total income ($/year)", las=3,
        names=c("No diploma or GED","High school","Some college",
                "Associates","Bachelors","Masters","Professional","Doctoral"))
svyby(~income, ~education, design, svyquantile, quantile=c(0.25,0.5,0.75), ci=TRUE, vartype="ci")
svyranktest(income~education, design, test="KruskaWallis") # datasets are significantly different
