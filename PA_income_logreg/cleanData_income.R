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
  
  # combine similar mv levels
  levels(dataset$mv) <- c("Occupied less than 1 year","Occupied 1 - 10 years",
                          "Occupied 1 - 10 years","Occupied 1 - 10 years",
                          "Occupied more than 10 years",
                          "Occupied more than 10 years",
                          "Occupied more than 10 years")
  
  # combine similar hht levels
  levels(dataset$hht) <- c("Married couple","Not a married couple",
                           "Not a married couple","Not a married couple",
                           "Not a married couple","Not a married couple",
                           "Not a married couple")
  
  # combine similar cow levels
  levels(dataset$cow) <- c("For profit","Non-profit","Government","Government",
                           "Government","Self-employed: unincorporated",
                           "Self-employed: incorporated",
                           "Not working for pay","Not working for pay")
  
  # combine similar education levels
  levels(dataset$education) <- c("Associates","Batchelors","Graduate",
                                 "High school","Graduate","No diploma or GED",
                                 "Graduate","Some College")
  dataset$education <- factor(dataset$education, levels=
                               c("No diploma or GED","High school",
                                 "Some College","Associates","Batchelors",
                                 "Graduate"))
  
  # change name of type_ to htype
  dataset$htype <- factor(dataset$type_)
  levels(dataset$htype) <- c("Housing unit","Group quarters","Group quarters")
  dataset$type_ <- NULL
  
  # the ten percent, easier to calculate than the 1%
  topTen <- quantile(dataset$pincp, probs=0.90, na.rm = TRUE)
  dataset$topTen <- ifelse(dataset$pincp > topTen, 1, 0)
  bottomTen <- quantile(dataset$pincp, probs=0.10, na.rm = TRUE)
  dataset$bottomTen <- ifelse(dataset$pincp < bottomTen, 1, 0)
  
  return(dataset)
}