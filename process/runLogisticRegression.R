runLogisticRegression <- function(db, interestingData, factorData, logregData, runtype){
  filename = paste("output_", runtype,".txt")
  
  data <- dbGetQuery(db, paste("SELECT", toString(interestingData),
                               "FROM", tablename))
  data <- data[data$agep > 17,]
  data <- data[complete.cases(data), ] # TODO this hammer will be too big in future
  data$povpipb <- ifelse(data$povpip > 100, 1, 0) # if above poverty line, success!
  data$povpipb <- factor(data$povpipb)
  data$agepf <- cut(data$agep, 10)
  data[factorData] <- lapply(data[factorData], factor)
  library(Amelia)
  png(file=paste("missmap_",runtype,".png"))
  missmap(data)
  dev.off()
  
  train_data <- data[data$serialno <  cutoffID[1,1],]
  test_data  <- data[data$serialno >= cutoffID[1,1],]
  
  library(biglm) # this is fragile because if a chunk doesn't have all the factors, it will cry
  glm_cmd <- paste("glm_mod <- glm(povpipb ~", paste(logregData, collapse="+"),
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
  missclassError <- mean(fitted_results != test_data$povpipb, na.rm = TRUE)
  sink(file=filename, append=TRUE)
  print(paste("Accuracy", 1-missclassError)) # good! (want close to 1)
  sink()
  
  library(ROCR)
  p <- predict(glm_mod, newdata=test_data, type="response")
  pr <- prediction(p, test_data$povpipb)
  prf <- performance(pr, measure="tpr", x.measure="fpr")
  png(file=paste("performance_",runtype,".png"))
  plot(prf)
  dev.off()
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  sink(file=filename, append=TRUE)
  print(paste("Area under curve = ", auc)) # ok! (want closer to 1 than 0.5)
  sink()
  
  return(glm_mod)
}