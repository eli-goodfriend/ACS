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

# plot a map of the results
shapeFilename <- "/home/eli/Data/ACS/shapefiles/cb_2015_all"
allPUMAregions <- readShapeSpatial(shapeFilename)
allPUMAregions@data <- merge(allPUMAregions@data, maxIndustryDF,by.x="PUMACE10",by.y="PUMA")
# TODO this merge is broken

#png("USindustry.png")
plot(allPUMAregions,col=allPUMAregions@data$maxIndustryCleaned,
     xlim=c(-125,-70),ylim=c(35,40),border=FALSE)
legend("topleft", fill=allPUMAregions@data$maxIndustryCleaned, legend=maxIndustryNames, col=allPUMAregions@data$maxIndustryCleaned)
#dev.off()
# TODO make this map actually legible

## Why does this not plot the continental US?
# library("ggplot2")
# library("mapproj")
# library("ggmap")
# allPUMAregions@data$id <- rownames(allPUMAregions@data)
# allPUMAregions.df <- fortify(allPUMAregions)
# allPUMAregions.df <- merge(allPUMAregions.df,allPUMAregions@data,"id")
# ggplot() +
#   geom_polygon(data = allPUMAregions.df,
#               aes(x = long, y = lat, group = group, fill = maxIndustryCleaned),
#               color = "black", size = 0.25) +
#   coord_map() +
#   theme_nothing(legend=TRUE)
