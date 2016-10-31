# plot percentages of attributes by PUMA region
library("ff")
library("ffbase")
filename <- "/home/eli/Data/ACS/people.ff"
acsData <- read.csv.ffdf(file=filename)

source("~/Dropbox/Code/ACS/plot_by_PUMA.R")

attributeToPlot = "transportToWork"
desiredAttributes <- c(9,10)
plotTitle <- "Percent of people who walk or bike to work"
plotName <- "walkorbike.png"
plotByPUMA(acsData, attributeToPlot, desiredAttributes, plotTitle, plotName)

attributeToPlot = "industry"
desiredAttributes <- c(8580)
plotTitle <- "Percent of people who work in bowling alleys"
plotName <- "bowling.png"
plotByPUMA(acsData, attributeToPlot, desiredAttributes, plotTitle, plotName)

attributeToPlot = "industry"
desiredAttributes <- c(170,180,190,270,280,290)
plotTitle <- "Percent of people who work in agriculture"
plotName <- "agriculture.png"
plotByPUMA(acsData, attributeToPlot, desiredAttributes, plotTitle, plotName)

attributeToPlot = "industry"
desiredAttributes <- c(7860,7870,7880,7890)
plotTitle <- "Percent of people who work in education"
plotName <- "education.png"
plotByPUMA(acsData, attributeToPlot, desiredAttributes, plotTitle, plotName)

attributeToPlot <- "totalIncome"
newAttribute <- integer(length(as.ram(acsData$totalIncome)))
newAttribute[which(as.ram(acsData$totalIncome) > 100000)] <- 1
acsData$newAttribute <- as.ff(newAttribute)
remove(newAttribute)
plotTitle <- "Percent of people making more than $100,000/yr"
plotName <- "richPeople.png"
plotByPUMA(acsData, "newAttribute", c(1), plotTitle, plotName)

attributeToPlot <- "workHoursPerWeek"
# TODO c/p because making this a function broke memory, fix that
newAttribute <- integer(length(as.ram(acsData$workHoursPerWeek)))
newAttribute[which(as.ram(acsData$workHoursPerWeek) > 40)] <- 1
acsData$newAttribute <- as.ff(newAttribute)
remove(newAttribute)
plotTitle <- "Percent of people who work more than 40 hours per week"
plotName <- "hardWorkers.png"
plotByPUMA(acsData, "newAttribute", c(1), plotTitle, plotName)

attributeToPlot <- "totalIncome"
plotTitle <- "Median income"
plotName <- "medianIncome.png"
#this crashes plotByPUMAMedian(acsData, attributeToPlot, plotTitle, plotName)





