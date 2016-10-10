# what is the most common industry in each PUMA region?
# TODO adjust using ACS magic repeated values
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
cat("The most common three industries are:\n")
cat(names(industryTable[1]), ", with", as.character(industryTable[1]), "workers,")
cat(names(industryTable[2]), ", with", as.character(industryTable[2]), "workers, and")
cat(names(industryTable[3]), ", with", as.character(industryTable[3]), "workers.")


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

# plot a map of the results
maxIndustryDF$maxIndustry <- as.factor(maxIndustryDF$maxIndustry) # to make discrete color plot
library("ggplot2")
library("mapproj")
library("ggmap")
library("plyr")
load("/home/eli/Data/ACS/shapefiles/cb_2015_all.df")
allPUMAregions.df <- merge(allPUMAregions.df, maxIndustryDF, by.x="id", by.y="PUMA",all.x="TRUE")
allPUMAregions.df <- allPUMAregions.df[order(allPUMAregions.df$order), ]
customPalette = c("tomato4","darkgrey","dimgrey","tan3","steelblue1","steelblue2","steelblue3",
                  "green","lightgoldenrod1","lightgoldenrod2","lightgoldenrod3","darkorchid1",
                  "darkorchid4","red","hotpink1","hotpink2","hotpink3","yellow","olivedrab1",
                  "olivedrab4")
plotOfMap <- ggplot() +
  geom_polygon(data = allPUMAregions.df,
              aes(x = long, y = lat, group = group, fill = maxIndustry)) +
  coord_map() +
  theme_nothing(legend=TRUE) +
  xlim(-130, -65) +
  ylim(25,50) +
  scale_fill_hue("Most common industry in region",l = 40,labels = maxIndustryNames)
ggsave(plotOfMap, file="prettyPlot.png", type="cairo-png", width=20, height=10)
