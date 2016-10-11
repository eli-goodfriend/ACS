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

# construct unique geography codes from state and PUMA
PUMA_ram <- as.ram(people$PUMA[idx])
PUMA_ram <- sprintf("%05d",as.numeric(PUMA_ram))
states_ram <- as.ram(people$state[idx])
states_ram <- sprintf("%02d",as.numeric(states_ram))
geography <- paste(states_ram,PUMA_ram,sep="")

# what is the most common industry in each PUMA region?
geographyIndustryTable <- table(geography, industryAlone)
maxIndustryIdx <- apply(geographyIndustryTable,1,which.max)
maxIndustry <- industryAlone[maxIndustryIdx]
maxIndustryDF <- as.data.frame(maxIndustry)
maxIndustryDF$geography <- rownames(as.data.frame(maxIndustryIdx))

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
allPUMAregions.df <- merge(allPUMAregions.df, maxIndustryDF, by.x="id", by.y="geography",all.x="TRUE")
allPUMAregions.df <- allPUMAregions.df[order(allPUMAregions.df$order), ]
plotOfMap <- ggplot() +
  geom_polygon(data = allPUMAregions.df,
              aes(x = long, y = lat, group = group, fill = maxIndustry)) +
  coord_map() +
  theme_nothing(legend=TRUE) +
  xlim(-130, -65) +
  ylim(25,50) +
  scale_fill_hue("Most common industry in region",l = 40,labels = maxIndustryNames)
ggsave(plotOfMap, file="prettyPlot.png", type="cairo-png", width=20, height=10)
