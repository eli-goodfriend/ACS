runSVM <- function(db, tablename, interestingData, factorData, svmData, 
                                  runtype, variable){
  filename = paste("output_", runtype, "_", variable, ".txt")

  dataset <- dbGetQuery(db, paste("SELECT", toString(interestingData),
                               "FROM", tablename))
  dataset <- dataset[dataset$agep > 17,]
  dataset <- dataset[complete.cases(dataset), ] # TODO this hammer will be too big in future
  
  poor <- quantile(dataset$pincp, probs=0.10)
  dataset$povpipb <- ifelse(dataset$pincp < poor, 1, 0) # in group if in bottom 10% of total income
  dataset$povpipb <- factor(dataset$povpipb)
  dataset$agepf <- cut(dataset$agep, c(17,25,35,65,100))
  rich <- quantile(dataset$pincp, probs=0.90)
  dataset$rich <- ifelse(dataset$pincp > rich, 1, 0) # in group if in top 10% of total income
  dataset$rich <- factor(dataset$rich)
  
  successCmd <- paste("dataset$success <- dataset$",variable)
  eval(parse(text=successCmd))
  dataset[factorData] <- lapply(dataset[factorData], factor)
  
  library(Amelia)
  png(file=paste("missmap_", runtype, "_", variable, ".png"))
  missmap(dataset)
  dev.off()
  
  # divide into test and training datasets
  # Is serialno a good variable to divide on? might be related to income through geography
  sql <- paste("SELECT MIN(serialno) FROM", tablename)
  minSerialNo <- dbGetQuery(db, sql)
  sql <- paste("SELECT MAX(serialno) FROM", tablename)
  maxSerialNo <- dbGetQuery(db, sql)
  cutoffID <- minSerialNo + (maxSerialNo - minSerialNo)*0.8
  train_data <- dataset[dataset$serialno <  cutoffID[1,1],]
  test_data  <- dataset[dataset$serialno >= cutoffID[1,1],]
  
  # undersample the majority class
  # TODO am I doing this right? 
  temp_data <- train_data[train_data$success == 0,]
  cutoffNum <- round(length(train_data$success)/10)
  temp_data <- temp_data[1:cutoffNum,] # this could be cleverer
  train_data <- rbind(temp_data, train_data[train_data$success == 1,])

  # TODO want to explore space of cost and gamma parameters
  svm_cmd <- paste("svm_mod <- svm(success~", paste(svmData, collapse="+"),
                   ", data=train_data, cost=100, gamma=1,
                   class.weights=c('0'=1, '1'=10))")
  eval(parse(text=svm_cmd)) # this is loooooong
  svm_pred <- predict(svm_mod, test_data)
  
  sink(file=filename)
  print(summary(svm_mod))
  print(table(pred = svm_pred, true = test_data$rich))
  # confint?
  sink()
  
  return(svm_mod)
}