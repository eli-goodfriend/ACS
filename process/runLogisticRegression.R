runLogisticRegression <- function(db, tablename, interestingData, factorData, logregData, 
                                  runtype, variable){
  filename = paste("output_", runtype, "_", variable, ".txt")

  dataset <- dbGetQuery(db, paste("SELECT", toString(interestingData),
                               "FROM", tablename))
  dataset <- dataset[dataset$agep > 17,]
  dataset <- dataset[complete.cases(dataset), ] # TODO this hammer will be too big in future
  dataset$povpipb <- ifelse(dataset$povpip < 100, 1, 0) # in group if below poverty line
  dataset$povpipb <- factor(dataset$povpipb)
  dataset$agepf <- cut(dataset$agep, c(18,25,35,65,100))
  rich <- quantile(dataset$pincp, probs=0.99) # the one percent! statistically!
  dataset$rich <- ifelse(dataset$pincp > rich, 1, 0) # in group if in top 1% of total income
  dataset$rich <- factor(dataset$rich)
  dataset[factorData] <- lapply(dataset[factorData], factor)
  
  library(Amelia)
  png(file=paste("missmap_", runtype, "_", variable, ".png"))
  missmap(dataset)
  dev.off()
  
  sql <- paste("SELECT MIN(serialno) FROM", tablename)
  minSerialNo <- dbGetQuery(db, sql)
  sql <- paste("SELECT MAX(serialno) FROM", tablename)
  maxSerialNo <- dbGetQuery(db, sql)
  cutoffID <- minSerialNo + (maxSerialNo - minSerialNo)*0.8
  train_data <- dataset[dataset$serialno <  cutoffID[1,1],]
  test_data  <- dataset[dataset$serialno >= cutoffID[1,1],]
  
  #library(biglm) # this is fragile because if a chunk doesn't have all the factors, it will cry
  glm_cmd <- paste("glm_mod <- glm(",variable,"~", paste(logregData, collapse="+"),
                   ", family=binomial(link='logit'), data=train_data)")
  eval(parse(text=glm_cmd))
  
  sink(file=filename)
  print(summary(glm_mod))
  sink()
  
  sink(file=filename, append=TRUE)
  print(anova(glm_mod, test="Chisq"))
  sink()
  library("pscl")
  sink(file=filename, append=TRUE)
  print(pR2(glm_mod))
  sink()
  
  fitted_results <- predict(glm_mod, newdata=test_data, type="response")
  fitted_results <- ifelse(fitted_results > 0.5, 1, 0)
  missclassCmd <- paste("missclassError <- mean(fitted_results != test_data$",
                        variable,", na.rm = TRUE)")
  eval(parse(text=missclassCmd))
  sink(file=filename, append=TRUE)
  print(paste("Accuracy", 1-missclassError)) # good! (want close to 1)
  sink()
  
  library(ROCR)
  p <- predict(glm_mod, newdata=test_data, type="response")
  predictionCmd <- paste("pr <- prediction(p, test_data$",variable,")")
  eval(parse(text=predictionCmd))
  prf <- performance(pr, measure="tpr", x.measure="fpr")
  png(file=paste("performance_", runtype, "_", variable, ".png"))
  plot(prf)
  dev.off()
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  sink(file=filename, append=TRUE)
  print(paste("Area under curve = ", auc)) # ok! (want closer to 1 than 0.5)
  sink()
  
  retList <- list("model"=glm_mod, "prf"=prf)
  return(retList)
}