# plot percentages of attributes by PUMA region
source("~/Dropbox/Code/ACS/plot_by_PUMA.R")

attributeToPlot = "transportToWork"
desiredAttributes <- c(9,10)
plotTitle <- "Percent of people who walk or bike to work"
plotName <- "bicycle.png"

plotByPUMA(attributeToPlot, desiredAttributes, plotTitle, plotName)