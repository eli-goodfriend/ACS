# income, hours works, and possibly other correlations
# TODO adjust using ACS magic repeated values
library("ff")
library("ffbase")
library("ggplot2")


filename <- "/home/eli/Data/ACS/people.ff"
people <- read.csv.ffdf(file=filename)

# visualize everything
#library("tabplot")
#tableplot(people)

income <- as.ram(people$totalIncome)
hoursPerWeek <- as.ram(people$workHoursPerWeek)

#plot(hoursPerWeek,income) # this plot takes a long time to render and is a cloud of data
cor(hoursPerWeek,income,use="complete.obs") # basically uncorrelated

# nationally, what is the cutoff for top 1% of income? top 10%?
# na indicates person was less than 15 years old
hist(income)
quantile(income, na.rm=TRUE, probs=c(0.5,0.75,0.9,0.99))

# let's just look at adults between the ages of 18 and 65
age <- as.ram(people$age)
ageRangeIndices <- which(age>=18 & age <= 65)
income_ageLimited <- income[ageRangeIndices]
hist(income_ageLimited)
quantile(income_ageLimited, na.rm=TRUE, probs=c(0.01,0.1,0.5,0.75,0.9,0.99))

# how about age limited adults, broken down between men and women?
gender <- as.factor(as.ram(people$gender))
ageLimitedMenIndices <- which(gender[ageRangeIndices]==1)
ageLimitedWomenIndices <- which(gender[ageRangeIndices]==2)
income_ageLimited_men <- income_ageLimited[ageLimitedMenIndices]
income_ageLimited_women <- income_ageLimited[ageLimitedWomenIndices]
hist(income_ageLimited_men)
quantile(income_ageLimited_men, na.rm=TRUE, probs=c(0.5,0.75,0.9,0.99))
hist(income_ageLimited_women)
quantile(income_ageLimited_women, na.rm=TRUE, probs=c(0.5,0.75,0.9,0.99))

# let's look at all people who are working, according to their industry mark
industry <- as.factor(as.ram(people$industry))
workingIndices <- which(industry!=9920)
income_working <- income[workingIndices]
hist(income_working)
quantile(income_working,na.rm=TRUE, probs=c(0.5,0.75,0.9,0.99))
# for people who work, is the hours per week worked and income related?
hoursPerWeek_working <- hoursPerWeek[workingIndices]
cor(hoursPerWeek_working, income_working, use="complete.obs") # nope

# now break down working people between men and women
workingMenIndices <- which(gender[workingIndices]==1)
workingWomenIndices <- which(gender[workingIndices]==2)
income_working_men <- income_working[workingMenIndices]
income_working_women <- income_working[workingWomenIndices]
hist(income_working_men)
quantile(income_working_men, na.rm=TRUE, probs=c(0.5,0.75,0.9,0.99))
hist(income_working_women)
quantile(income_working_women, na.rm=TRUE, probs=c(0.5,0.75,0.9,0.99))

gender_working <- gender[workingIndices]
boxplot(income_working~gender_working,
        main='Income under different genders, workers',
        ylab='Income ($/year)',
        xlab='Gender',
        names=c('Men','Women'))

# break down working people among races
# this "race" designation has problems
race <- as.factor(as.ram(people$race))
race_working <- race[workingIndices]
boxplot(income_working~race_working,
        main='Income under different races, workers',
        ylab='Income ($/year)',
        xlab='Race',
        names=c('White','Black','American Indian','Alaska Native',
                'Specific tribal affiliation','Asian','Pacific Islander','Other','Multi'))

# is there a demographic for which longer work means higher income?
# working men
cor(hoursPerWeek_working[workingMenIndices], income_working[workingMenIndices], 
    use="complete.obs") # nope
# working women
cor(hoursPerWeek_working[workingWomenIndices], income_working[workingWomenIndices], 
    use="complete.obs") # nope
# working white men
whiteIndices <- which(race==1)
menIndices <- which(gender==1)
workingWhiteMenIndices <- intersect(intersect(whiteIndices,menIndices),workingIndices)
cor(hoursPerWeek[workingWhiteMenIndices],income[workingWhiteMenIndices], 
    use="complete.obs") # nope
# working black women
blackIndices <- which(race==2)
womenIndices <- which(gender==2)
workingBlackWomenIndices <- intersect(intersect(blackIndices,womenIndices),workingIndices)
cor(hoursPerWeek[workingBlackWomenIndices],income[workingBlackWomenIndices], 
    use="complete.obs") # nope
# try 'em all! hint: the answer is nope
for (genderType in c(1,2)){
  genderIndices <- which(gender==genderType)
  genderIndices <- intersect(genderIndices,workingIndices) # workers only
  for (raceType in c(1,2,3,4,5,6,7,8,9)){
    raceIndices <- which(race==raceType)
    indices <- intersect(genderIndices,raceIndices)
    correl <- cor(hoursPerWeek[indices],income[indices],use="complete.obs")
    cat("For gender",as.character(genderType),"and race",as.character(raceType),
        "hours worked and income have correlation",as.character(correl))
  }
}





