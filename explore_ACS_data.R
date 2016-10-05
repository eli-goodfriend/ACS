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
maxIndustryDF$PUMA <- rownames(as.data.frame(maxIndustryIdx))
maxIndustryDF$PUMA <- formatC(as.numeric(maxIndustryDF$PUMA),width=5,flag="0")

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
shapeFilename <- "/home/eli/Data/ACS/shapefiles/cb_2015_all"
allPUMAregions <- readShapeSpatial(shapeFilename)
library("ggplot2")
library("mapproj")
library("ggmap")
library("plyr")
allPUMAregions@data$id <- rownames(allPUMAregions@data)
allPUMAregions.df <- fortify(allPUMAregions)
allPUMAregions.df <- join(allPUMAregions.df,allPUMAregions@data,by="id")
allPUMAregions.df <- merge(allPUMAregions.df, maxIndustryDF, by.x="PUMACE10", by.y="PUMA")
ggplot() +
  geom_polygon(data = allPUMAregions.df,
              aes(x = long, y = lat, group = group, fill = maxIndustryCleaned)) +
  coord_map() +
  theme_nothing(legend=TRUE) +
  xlim(-120,-80) +
  scale_color_discrete(name="Most common industry",breaks=1:16,labels=maxIndustryNames)
