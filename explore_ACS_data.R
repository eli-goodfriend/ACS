# load ff format ACS data and play with it
library("ff")
library("ffbase")
library("maptools")

filename <- "/home/eli/Data/ACS/people.ff"
people <- read.csv.ffdf(file=filename)

# what is the most common industry overall?
industryAlone <- people$industry
idx <- which(!is.na(industryAlone)[])
industryAlone <- industryAlone[idx] # only entries that have an industry
industryTable <- sort(table(industryAlone), decreasing=TRUE)
cat("The most common industry is", names(industryTable[1]), ", with", 
      as.character(industryTable[1]), "workers")

# what is the most common industry in each PUMA region?
geographyForIndustry <- people$geography[idx]
geographyIndustryTable <- table(geographyForIndustry, industryAlone)
maxIndustryIdx <- apply(geographyIndustryTable,1,which.max)
maxIndustry <- industryAlone[maxIndustryIdx]
maxIndustryDF <- as.data.frame(maxIndustry)

# clean up PUMA so it matches the shapefile for plotting
maxIndustryDF$PUMA <- rownames(as.data.frame(maxIndustryIdx))
maxIndustryDF$PUMA <- sprintf("%05d",as.numeric(maxIndustryDF$PUMA))
maxIndustryDF$PUMA <- as.factor(maxIndustryDF$PUMA)

# pull out the industry names
setOfMaxIndustries <- sort(unique(maxIndustryDF$maxIndustry))
setOfMaxIndustries <- formatC(setOfMaxIndustries,width=4,flag="0")
industryNames <- read.table("/home/eli/Data/ACS/industries.txt", sep=".")
maxIndustryNamesIdx <- match(setOfMaxIndustries,industryNames$V1)
maxIndustryNames <- industryNames$V2[maxIndustryNamesIdx]

# tidy this data to make it display better
setOfMaxIndustries <- sort(unique(maxIndustryDF$maxIndustry))
maxIndustryDF$maxIndustryCleaned <- maxIndustryDF$maxIndustry
for (idx in 1:length(setOfMaxIndustries)){
  maxIndustryDF$maxIndustryCleaned <- replace(maxIndustryDF$maxIndustryCleaned, maxIndustryDF$maxIndustry==setOfMaxIndustries[idx], idx)
}
maxIndustryDF$maxIndustryCleaned <- as.factor(maxIndustryDF$maxIndustryCleaned)

# plot a map of the results
library("ggplot2")
library("mapproj")
library("ggmap")
library("plyr")
load("/home/eli/Data/ACS/shapefiles/cb_2015_all.df")
allPUMAregions.df <- merge(allPUMAregions.df, maxIndustryDF, by.x="id", by.y="PUMA",all.x="TRUE")
allPUMAregions.df <- allPUMAregions.df[order(allPUMAregions.df$order), ]
plotOfMap <- ggplot() +
  geom_polygon(data = allPUMAregions.df,
              aes(x = long, y = lat, group = group, fill = maxIndustryCleaned)) +
  coord_map() +
  theme_nothing(legend=TRUE) +
  xlim(-130,-65) +
  ylim(20,50) +
  scale_fill_hue("Most common industry in region",l = 40,labels = maxIndustryNames)
ggsave(plotOfMap, file="prettyPlot.png", type="cairo-png", width=15, height=8)
