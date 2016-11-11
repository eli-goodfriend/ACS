runLogisticRegression <- function(db, tablename, interestingData, factorData, logregData, 
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
  
  index     <- 1:nrow(dataset)
  testindex <- sample(index, trunc(length(index)/5))
  train_data <- dataset[-testindex,]
  test_data  <- dataset[testindex,]
  
  # correction for class imbalance: use weighted maximum likelihood
  # (King 2001) Logistic Regression in Rare Events Data
  # do this in the training set only
  # subsample all of the data that is a "success", since that's small
  # and 10% of the data that's a "failure"
  temp_data <- train_data[train_data$success == 0,]
  successindex <- 1:nrow(temp_data)
  undersampleIndex <- sample(successindex, trunc(length(successindex)/10))
  temp_data <- temp_data[undersampleIndex,]
  train_data <- rbind(temp_data, train_data[train_data$success == 1,])
  train_data$weights <- as.integer(train_data$success)*9 + 1 # 10 for success, 1 otherwise
  
  #library(biglm) # this is fragile because if a chunk doesn't have all the factors, it will cry
  glm_cmd <- paste("glm_mod <- glm(success~", paste(logregData, collapse="+"),
                   ", family=binomial(link='logit'), data=train_data,
                   weights = weights)")
  eval(parse(text=glm_cmd))
  
  sink(file=filename)
  print(summary(glm_mod))
  #print(confint(glm_mod)) # less useful in this form, better as odds ratio
  oddsRatio <- exp(cbind(OR = coef(glm_mod), confint(glm_mod)))
  print(oddsRatio)
  sink()
  
  library(ggplot2)
  oddsRatio <- as.data.frame(oddsRatio)
  oddsRatio <- oddsRatio[-c(1),] # don't care about intercept, which is always first
  oddsRatio$varNames <- rownames(oddsRatio)
  oddsRatio$varNames <- gsub('sex2','Female',oddsRatio$varNames) #TODO this is annoying
  oddsRatio$varNames <- gsub('agepf\\(25,35]','Age 26-35',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('agepf\\(35,65]','Age 36-65',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('agepf\\(65,100]','Age 66 or older',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('racaian1','Native American',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('racasn1','Asian American',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('racblk1','African American',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('racsor1','Some other race',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('racwht1','White',oddsRatio$varNames)
  oddsRatio$varNames <- factor(oddsRatio$varNames, levels = oddsRatio$varNames)
  ggfile=paste("OR_", runtype, "_", variable, ".png")
  ggplot(oddsRatio, aes(x = varNames, y = OR, ymin = `2.5 %`, ymax = `97.5 %`)) + 
    geom_pointrange() + coord_flip() + scale_y_continuous(trans='log10') +
    ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) + 
    xlab("")
  ggsave(ggfile)
  
  sink(file=filename, append=TRUE)
  print(anova(glm_mod, test="Chisq"))
  sink()
  library("pscl")
  sink(file=filename, append=TRUE)
  #print(pR2(glm_mod)) # why is this broken now? don't use it but still
  sink()
  
  #TODO should newdata only have vars used in fitting? or smart enough?
  fitted_results <- predict(glm_mod, newdata=test_data, type="response")
  fitted_results <- ifelse(fitted_results > 0.5, 1, 0)
  missclassError <- mean(fitted_results != test_data$success, na.rm = TRUE)
  sink(file=filename, append=TRUE)
  print(paste("Accuracy", 1-missclassError)) # good! (want close to 1)
  print(summary(factor(fitted_results)))
  sink()
  
  library(ROCR)
  p <- predict(glm_mod, newdata=test_data, type="response")
  pr <- prediction(p, test_data$success)
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