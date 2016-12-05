library(MonetDBLite)
library(DBI)

# open the database containing the datasets
dbfolder <- "~/Data/ACS/MonetDB"
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
tablename <- "acs14lite"

# plotting function
plotMap <- function(dataframe, column, titletext){    
  library("ggplot2")
  library("mapproj")
  library("ggmap")
  library("gridExtra")

  # overall outline
  # TODO this should get moved
  library("maptools")
  outlineFile = "~/Data/ACS/shapefiles/cb_2015_us_nation_20m.shp"
  outline = readShapePoly(outlineFile)
  outlineToPlot = fortify(outline)
  outlineToPlot = outlineToPlot[order(outlineToPlot$order),]
  
  limits = c(min(column),max(column))
  continentalPlot <- ggplot() +
    geom_polygon(data = dataframe,
                 aes(x = long, y = lat, group = group, fill = column)) +
    coord_map() +
    theme_nothing(legend=TRUE) +
    theme(legend.text=element_text(size=20),
          plot.title=element_text(size=24)) +
    labs(title=titletext) +
    xlim(-130, -65) +
    ylim(20,50) +
    scale_fill_gradient("",low="white", high="blue", limits=limits) +
    geom_polygon(data = outlineToPlot, aes(x=long, y=lat, group=group), 
                 color="black", fill=NA)
  alaskaPlot <- ggplot() +
    geom_polygon(data = dataframe,
                 aes(x = long, y = lat, group = group, fill = column)) +
    coord_map() +
    theme_nothing(legend=FALSE) +
    xlim(-180, -120) +
    ylim(50,73) +
    scale_fill_gradient(low="white", high="blue", limits=limits) +
    geom_polygon(data = outlineToPlot, aes(x=long, y=lat, group=group), 
                 color="black", fill=NA)
  hawaiiPlot <- ggplot() +
    geom_polygon(data = dataframe,
                 aes(x = long, y = lat, group = group, fill = column)) +
    coord_map() +
    theme_nothing(legend=FALSE) +
    xlim(-160, -153) +
    ylim(17.5,22.5) +
    scale_fill_gradient(low="white", high="blue", limits=limits) +
    geom_polygon(data = outlineToPlot, aes(x=long, y=lat, group=group), 
                 color="black", fill=NA)
  usaPlot <- grid.arrange(arrangeGrob(alaskaPlot + theme(legend.position="none"),
                                      hawaiiPlot + theme(legend.position="none"),
                                      nrow=2),
                          continentalPlot, nrow=1, widths=c(1,5))
  print(usaPlot)
}

# data crunching
plotByPUMA <- function(dataset){
  dataset$geography <- sprintf("%07d",as.numeric(dataset$geography))

  # what is the percentage overall?
  totalPeople <- sum(dataset$pwgtp)
  totalWithDesiredAttribute <- sum(dataset$pwgtp[which(dataset$likeme == 1)])
  nationalPercentage <- totalWithDesiredAttribute / totalPeople *100
  titletext <- paste0("National percent: ", round(nationalPercentage), "%")
  
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
  plotMap(allPUMAregions.df, allPUMAregions.df$percentWithAttribute, titletext)
}

# data corralling
likeMe <- function(criteriaIn){

  
  # determine the data we want, pull it, and clean it up
  criteria <- strsplit(criteriaIn, ".", fixed = TRUE)
  criteria <- matrix(unlist(criteria), ncol = 3, byrow = TRUE)
  newVariables <- criteria[,1]
  variables <- c("pwgtp", "geography", newVariables)
  dataset <- dbGetQuery(db, paste("SELECT", toString(variables),
                                  "FROM", tablename))
  
  # figure out who is like me
  criterion <- gsub(".", "", criteriaIn, fixed = TRUE)
  criterion <- paste("dataset$", criterion, sep="")
  criterion <- paste(criterion, collapse = "&")
  cmd <- paste0("dataset$likeme <- ifelse(",criterion,", 1, 0)")
  eval(parse(text=cmd))
  dataset$likeme[is.na(dataset$likeme)] <- 0
  dataset$likeme <- factor(dataset$likeme)
  
  # make the plot
  plotByPUMA(dataset)
}



