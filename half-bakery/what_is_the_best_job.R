# what is the best industry in America, by various metrics?
# TODO: adjust using weights
library("ff")
library("ffbase")
library("ggplot2")
library("plyr")

filename <- "/home/eli/Data/ACS/people.ff"
people <- read.csv.ffdf(file=filename)

# pick out entries for people who earned wages #TODO is this quite what I want? maybe who worked hours
industry <- as.factor(as.ram(people$industry))
wages <- as.ram(people$wages)
idxIsIndustry <- which(!is.na(industry)[])
idxEarnedWages <- which(wages>0)
idx <- intersect(idxIsIndustry,idxEarnedWages)
industry <- industry[idx]
wages <- wages[idx]
industry.num <- as.ram(people$industry[idx])
hoursPerWeek <- as.ram(people$workHoursPerWeek[idx])
weight <- as.ram(people$weight[idx])
jobData <- data.frame(industry,wages,hoursPerWeek)

# what industry has the highest median wage?
medianWage <- ddply(jobData, .(industry), summarize,  medianWage=median(wages))
medianWage <- medianWage[order(-medianWage$medianWage),]

# what industry has the highest wages / worked hours ratio?
jobData$wagesToHours <- wages/hoursPerWeek
medianWagesToHours <- ddply(jobData, .(industry), summarize, medianWagesToHours=median(wagesToHours))
medianWagesToHours <- medianWagesToHours[order(-medianWagesToHours$medianWagesToHours),]

# what industry has the smallest variance in wage?
varianceWage <- ddply(jobData, .(industry), summarize, varianceWage=var(wages))
varianceWage <- varianceWage[order(varianceWage$varianceWage),]

# what industry has the smallest interquartile range?
quartileWage <- ddply(jobData, .(industry), summarize, quartiles=quantile(wages))


