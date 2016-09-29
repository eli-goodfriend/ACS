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
# TODO map the indices to the actual industry tag
maxIndustryDF <- as.data.frame(maxIndustryIdx)
maxIndustryDF$PUMA <- rownames(maxIndustryDF)

# plot a map of the results
shapeFilename <- "/home/eli/Data/ACS/shapefiles/cb_2015_all"
allPUMAregions <- readShapeSpatial(shapeFilename)
allPUMAregions@data <- merge(allPUMAregions@data, maxIndustryDF,by.x="PUMACE10",by.y="PUMA")

plot(allPUMAregions,col=allPUMAregions@data$maxIndustryIdx,
     xlim=c(-120,-80),ylim=c(30,40),border=FALSE)
# TODO make this map actually legible

