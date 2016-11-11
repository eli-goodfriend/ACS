getConfusionMatrix <- function(fit, testdata){
  fitClasses <- predict(fit, newdata = testdata)
  confusionMatrix(data = fitClasses, testdata$success)
}

getAUC <- function(fit, testdata){
  library(ROCR)
  p <- predict(fit, newdata = testdata, type="prob")[,2] # why [,2]? dunno
  pr <- prediction(p, testdata$success)
  prf <- performance(pr, measure="tpr", x.measure="fpr")
  plot(prf)
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  auc
}

plotOddsRatios <- function(fit){
  oddsRatio <- exp(cbind(OR = coef(fit$finalModel), 
                         confint(fit$finalModel)))
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
  oddsRatio$varNames <- gsub('racwht1','Caucasian',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('raclat','Latinx',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('education','',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('waob','',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('dis2','No disability',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('lanx2','Speak English at home',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('mar2','Widowed',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('mar3','Divorced',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('mar4','Separated',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('mar5','Never married',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('cow2','Non-profit employee',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('cow3','Local government',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('cow4','State government',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('cow5','Federal government',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('cow6','Self-employed: not incorporated',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('cow7','Self-employed: incorporated',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('cow8','Without pay in family business',oddsRatio$varNames)
  oddsRatio$varNames <- gsub('cow9','Unemployed',oddsRatio$varNames)
  oddsRatio$varNames <- factor(oddsRatio$varNames, levels = oddsRatio$varNames)
  
  ggplot(oddsRatio, aes(x = varNames, y = OR, ymin = `2.5 %`, ymax = `97.5 %`)) + 
    geom_pointrange() + coord_flip() + scale_y_continuous(trans='log10') +
    ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) + 
    xlab("")
}