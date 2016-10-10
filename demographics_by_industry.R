# demographics: overall and by industry
# TODO: adjust using weights
library("ff")
library("ffbase")
library("ggplot2")

filename <- "/home/eli/Data/ACS/people.ff"
people <- read.csv.ffdf(file=filename)

gender <- as.factor(as.ram(people$gender))
race <- as.factor(as.ram(people$race))
income <- as.ram(people$totalIncome)
education <- as.factor(as.ram(people$highestDegree))
industry <- as.factor(as.ram(people$industry))
industry.num <- as.ram(people$industry)
povertyRatio <- as.ram(people$incomeToPovertyRatio)
age <- as.ram(people$age)
demographics.df <- data.frame(gender,race,income,industry,povertyRatio,age,education)

plotDemographics <- function(data, index){
  pdf(file='plot.pdf')
  op <- par(mfrow=c(3,2), 
            oma = c(5,4,0,0) + 0.1,
            mar = c(1,1,1,1) + 0.1)
  plot(data$gender[index], names=c('Men','Women'))
  plot(data$race[index], names=c('White','Black','Am. Ind.','Alaska Nat.',
                     'Spec. tribal','Asian','Pac. Isl.','Other','Multi'))
  hist(data$income[index], xlab="Income ($/yr)", main=NULL, breaks=500, xlim=c(-10000,200000))
  hist(data$age[index], xlab="Age (yrs)", main=NULL)
  plot(data$education[index])
  par(op)
  dev.off()
}

# demographics: the entire country
allIndices <- which(gender==1 | gender==2)
plotDemographics(demographics.df, allIndices)
#demographics <- table(gender,race,industry) #this maybe is a different thought
#summary(demographics)

# demographics: people working
workingIndices <- which(industry!=9920)
plotDemographics(demographics.df, workingIndices)

# demographics: people below the poverty line
povertyIndices <- which(povertyRatio<=100)
plotDemographics(demographics.df, povertyIndices)

# demographics: people working at bowling alleys
bowlingAllyIndices <- which(industry==8580)
plotDemographics(demographics.df, bowlingAllyIndices)

# demographics: working retail
retailIndices <- which(industry.num>=4670 & industry.num<=5790 )
plotDemographics(demographics.df, retailIndices)

# demographics: farming (crops)
farmingIndices <- which(industry==170)
plotDemographics(demographics.df, farmingIndices)

