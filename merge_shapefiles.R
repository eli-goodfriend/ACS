# take the state by state shapefiles for PUMA regions and merge them into one file
library("maptools")

shapeDir <- "/home/eli/Data/ACS/shapefiles/"
states <- read.table("/home/eli/Data/ACS/states.txt", sep=".")
stateNums <- states$V1
firstState = TRUE
for (state in stateNums){
  oneShapeFile <- paste(shapeDir,"cb_2015_",formatC(state,width=2,flag='0'),"_puma10_500k", sep="")
  region <- readShapeSpatial(oneShapeFile)
  row.names(region) <- paste(formatC(state,width=2,flag='0'),row.names(region),sep="")
  # first state: start the file
  if (firstState){
    allStates <- region
    firstState = FALSE
  }
  # all other states: add to the file
  else{
    allStates <- spRbind(allStates, region)
  }
}

plot(allStates)
writeSpatialShape(allStates,"/home/eli/Data/ACS/shapefiles/cb_2015_all")

allPUMAregions.df <- fortify(allStates, region="PUMACE10")
save(allPUMAregions.df, file="/home/eli/Data/ACS/shapefiles/cb_2015_all.df")

