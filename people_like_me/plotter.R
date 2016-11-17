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
plotByPUMA <- function(dataset, plotTitle, plotName){
  dataset$geography <- sprintf("%07d",as.numeric(dataset$geography))

  # what is the percentage overall?
  totalPeople <- sum(dataset$pwgtp)
  totalWithDesiredAttribute <- sum(dataset$pwgtp[which(dataset$likeme == 1)])
  nationalPercentage <- totalWithDesiredAttribute / totalPeople *100
  nationalPercentage
  
  # what is the percentage in each PUMA region?
  totalPeoplePerPUMA <- aggregate(pwgtp~geography, dataset, sum)
  names(totalPeoplePerPUMA)[names(totalPeoplePerPUMA) == 'pwgtp'] <- 'totalWeight'
  
  totalWithDesiredAttributePerPUMA <- aggregate(
    pwgtp~geography+likeme, dataset, sum)
  totalWithDesiredAttributePerPUMA <- 
    totalWithDesiredAttributePerPUMA[which(
      totalWithDesiredAttributePerPUMA$likeme == 1),]
  totalPeoplePerPUMA <- merge(totalPeoplePerPUMA, 
                              totalWithDesiredAttributePerPUMA, by='geography', 
                              all.x=TRUE)
  totalPeoplePerPUMA$hasDesiredAttribute <- NULL
  totalPeoplePerPUMA[is.na(totalPeoplePerPUMA)] <- 0
  
  totalPeoplePerPUMA$percentWithAttribute <- totalPeoplePerPUMA$pwgtp / 
    totalPeoplePerPUMA$totalWeight * 100
  
  # plot it
  load("/home/eli/Data/ACS/shapefiles/cb_2015_all.df") # loads allPUMAregions.df
  allPUMAregions.df <- merge(allPUMAregions.df, totalPeoplePerPUMA, 
                             by.x="id", by.y="geography",all.x="TRUE")
  allPUMAregions.df <- allPUMAregions.df[order(allPUMAregions.df$order), ]
  plotMap(allPUMAregions.df, allPUMAregions.df$percentWithAttribute, 
          plotTitle, plotName)
}




