# plotting function
plotMap <- function(dataframe, column, titleText, fileName){    
  library("ggplot2")
  library("mapproj")
  library("ggmap")
  library("gridExtra")
  
  limits = c(min(column),max(column))
  continentalPlot <- ggplot() +
    geom_polygon(data = dataframe,
                 aes(x = long, y = lat, group = group, fill = column)) +
    coord_map() +
    theme_nothing(legend=TRUE) +
    theme(legend.text=element_text(size=20),
          plot.title=element_text(size=24)) +
    labs(title=titleText) +
    xlim(-130, -65) +
    ylim(25,50) +
    scale_fill_gradient("",low="white", high="blue", limits=limits)
  alaskaPlot <- ggplot() +
    geom_polygon(data = dataframe,
                 aes(x = long, y = lat, group = group, fill = column)) +
    coord_map() +
    theme_nothing(legend=FALSE) +
    xlim(-180, -130) +
    ylim(53,73) +
    scale_fill_gradient(low="white", high="blue", limits=limits)
  hawaiiPlot <- ggplot() +
    geom_polygon(data = dataframe,
                 aes(x = long, y = lat, group = group, fill = column)) +
    coord_map() +
    theme_nothing(legend=FALSE) +
    xlim(-160, -153) +
    ylim(17.5,22.5) +
    scale_fill_gradient(low="white", high="blue", limits=limits)
  usaPlot <- grid.arrange(arrangeGrob(alaskaPlot + theme(legend.position="none"),
                                      hawaiiPlot + theme(legend.position="none"),
                                      nrow=2),
                          continentalPlot, nrow=1, widths=c(1,5))
  ggsave(usaPlot, file=fileName, type="cairo-png", width=20, height=10)
}

# primary function
plotByPUMA <- function(acsData, attributeToPlot, desiredAttributes, plotTitle, plotName){
  library("ff")
  library("ffbase")
  
  attribute <- as.ram(acsData[,c(attributeToPlot)])
  indices <- which(!is.na(attribute))
  attribute <- attribute[indices]
  attribute <- as.factor(attribute)
  
  geography <- as.ram(acsData$geography[indices])
  geography <- sprintf("%07d",as.numeric(geography))
  
  weight <- as.ram(acsData$weight[indices])
  
  people <- data.frame(attribute, geography, weight)
  
  # what subset of the attribute are we interested in?
  people$hasDesiredAttribute <- people$attribute %in% desiredAttributes
  
  # what is the percentage overall?
  totalPeople <- sum(people$weight)
  totalWithDesiredAttribute <- sum(people$weight[which(people$hasDesiredAttribute)])
  nationalPercentage <- totalWithDesiredAttribute / totalPeople *100
  
  # what is the percentage in each PUMA region?
  totalPeoplePerPUMA <- aggregate(weight~geography, people, sum)
  names(totalPeoplePerPUMA)[names(totalPeoplePerPUMA) == 'weight'] <- 'totalWeight'
  
  totalWithDesiredAttributePerPUMA <- aggregate(weight~geography+hasDesiredAttribute, people, sum)
  totalWithDesiredAttributePerPUMA <- totalWithDesiredAttributePerPUMA[which(totalWithDesiredAttributePerPUMA$hasDesiredAttribute),]
  totalPeoplePerPUMA <- merge(totalPeoplePerPUMA, totalWithDesiredAttributePerPUMA, by='geography', all.x=TRUE)
  totalPeoplePerPUMA$hasDesiredAttribute <- NULL
  totalPeoplePerPUMA[is.na(totalPeoplePerPUMA)] <- 0
  
  totalPeoplePerPUMA$percentWithAttribute <- totalPeoplePerPUMA$weight / totalPeoplePerPUMA$totalWeight * 100
  
  # plot it
  load("/home/eli/Data/ACS/shapefiles/cb_2015_all.df") # loads allPUMAregions.df
  allPUMAregions.df <- merge(allPUMAregions.df, totalPeoplePerPUMA, by.x="id", by.y="geography",all.x="TRUE")
  allPUMAregions.df <- allPUMAregions.df[order(allPUMAregions.df$order), ]
  plotMap(allPUMAregions.df, allPUMAregions.df$percentWithAttribute, plotTitle, plotName)
}

# primary function
plotByPUMAMedian <- function(acsData, attributeToPlot, plotTitle, plotName){
  # TODO this crashes memory, fix it
  library("ff")
  library("ffbase")
  library("matrixStats")
  
  attribute <- as.ram(acsData[,c(attributeToPlot)])
  indices <- which(!is.na(attribute))
  attribute <- attribute[indices]

  geography <- as.ram(acsData$geography[indices])
  geography <- sprintf("%07d",as.numeric(geography))
  
  weight <- as.ram(acsData$weight[indices])
  
  people <- data.frame(attribute, geography, weight)
  
  # what is the national median?
  nationalMedian <- weightedMedian(attribute, weight)
  
  # what is the median of attribute in each puma region, weighted?
  medianPerPUMA <- by(people, list(people$geography), function(x) weightedMedian(x$attribute, x$weight))
  medianPerPUMA <- cbind(medianPerPUMA)
  medianPerPUMA$geography <- rownames(medianPerPUMA)
  
  # plot it
  load("/home/eli/Data/ACS/shapefiles/cb_2015_all.df") # loads allPUMAregions.df
  allPUMAregions.df <- merge(allPUMAregions.df, medianPerPUMA, by.x="id", by.y="geography",all.x="TRUE")
  allPUMAregions.df <- allPUMAregions.df[order(allPUMAregions.df$order), ]
  plotMap(allPUMAregions.df, allPUMAregions.df$medianPerPUMA, plotTitle, plotName)
}