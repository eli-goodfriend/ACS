cleanData <- function(dataset){
  # after the data comes in from the database, it needs a little tidying
  # TODO: maybe generate these variables in the database, like I did with education?
  
  # divide age into chunks and make an ordered factor
  dataset$agepf <- factor(cut(dataset$agep, c(17,25,35,65,100)))
  levels(dataset$agepf) <- c("Very young adult","Young adult","Middle aged",
                             "Senior citizen")
  
  # # make education an ordered factor
  # # TODO this screws up the logreg for some reason, maybe it's trying too hard?
  # dataset$education <- ordered(dataset$education,
  #                      levels=c("No diploma or GED","High school","Some college",
  #                               "Associates","Batchelors","Masters","Doctoral",
  #                               "Professional"))

  # generate a Latinx "race" classification
  # TODO this should be more nuanced, probably
  dataset$hispn <- as.numeric(dataset$hisp)
  dataset$raclat <- factor(ifelse(dataset$hispn > 1 & dataset$hispn < 23, 1, 0))
  dataset$hispn <- NULL
  
  # combine "northern america" i.e. Canada and "oceania" since both are small
  levels(dataset$waob) <- c("America","PR and US Island Areas","Latin America",
                            "Asia","Europe","Africa","Canada, Oceania, or at sea",
                            "Canada, Oceania, or at sea")
  
  # change name of type_ to type
  dataset$htype <- factor(dataset$type_)
  levels(dataset$htype) <- c("Housing unit","Group quarters","Group quarters")
  dataset$type_ <- NULL
  
  return(dataset)
}