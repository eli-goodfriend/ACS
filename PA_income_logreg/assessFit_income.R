cleanNames <- function(columnName){
  columnName <- gsub('sex2','Female',columnName) #TODO this is annoying
  columnName <- gsub('agepf\\(25,35]','Age 26-35',columnName)
  columnName <- gsub('agepf\\(35,65]','Age 36-65',columnName)
  columnName <- gsub('agepf\\(65,100]','Age 66 or older',columnName)
  columnName <- gsub('racaian1','Native American',columnName)
  columnName <- gsub('racasn1','Asian American',columnName)
  columnName <- gsub('racblk1','African American',columnName)
  columnName <- gsub('racsor1','Some other race',columnName)
  columnName <- gsub('racwht1','Caucasian',columnName)
  columnName <- gsub('raclat','Latinx',columnName)
  columnName <- gsub('education','',columnName)
  columnName <- gsub('waob','',columnName)
  columnName <- gsub('mv','',columnName)
  columnName <- gsub('hht','',columnName)
  columnName <- gsub('cow','',columnName)
  columnName <- gsub('agepf','',columnName)
  columnName <- gsub('`','',columnName)
  columnName <- gsub('Latinx1','Latin@',columnName)
  columnName <- gsub('dis2','No disability',columnName)
  columnName <- gsub('lanx2','Speak English at home',columnName)
  columnName <- gsub('mar2','Widowed',columnName)
  columnName <- gsub('mar3','Divorced',columnName)
  columnName <- gsub('mar4','Separated',columnName)
  columnName <- gsub('mar5','Never married',columnName)
  columnName <- gsub('cow2','Non-profit employee',columnName)
  columnName <- gsub('cow3','Local government',columnName)
  columnName <- gsub('cow4','State government',columnName)
  columnName <- gsub('cow5','Federal government',columnName)
  columnName <- gsub('cow6','Self-employed: not incorporated',columnName)
  columnName <- gsub('cow7','Self-employed: incorporated',columnName)
  columnName <- gsub('cow8','Without pay in family business',columnName)
  columnName <- gsub('cow9','Unemployed',columnName)
  
  return(columnName)
}


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
  oddsRatio$varNames <- cleanNames(oddsRatio$varNames)
  oddsRatio$varNames <- factor(oddsRatio$varNames, levels = oddsRatio$varNames)
  
  
  al <- 0.3
  gg <- ggplot(oddsRatio, aes(x = varNames, y = OR, ymin = `2.5 %`, ymax = `97.5 %`)) + 
    geom_pointrange() + coord_flip() + scale_y_continuous(trans='log10') +
    ylab("Odds ratio & 95% CI") + geom_hline(aes(yintercept = 1)) + 
    xlab("") + 
    annotation_custom(grob=grid::rectGrob(gp=gpar(fill="orchid", alpha=al)),
                      xmin = 0.5, xmax = 2.5, ymin = -20, ymax = 20) +
    annotation_custom(grob=grid::rectGrob(gp=gpar(fill="purple", alpha=al)), 
             xmin = 2.5, xmax = 3.5, ymin = -20, ymax = 20) +
    annotation_custom(grob=grid::rectGrob(gp=gpar(fill="blue", alpha=al)),
             xmin = 3.5, xmax = 8.5, ymin = -20, ymax = 20) +
    annotation_custom(grob=grid::rectGrob(gp=gpar(fill="green", alpha=al)), 
             xmin = 8.5, xmax = 13.5, ymin = -20, ymax = 20) +
    annotation_custom(grob=grid::rectGrob(gp=gpar(fill="yellow", alpha=al)), 
             xmin = 13.5, xmax = 19.5, ymin = -20, ymax = 20) +
    annotation_custom(grob=grid::rectGrob(gp=gpar(fill="orange", alpha=al)), 
             xmin = 19.5, xmax = 20.5, ymin = -20, ymax = 20) +
    annotation_custom(grob=grid::rectGrob(gp=gpar(fill="orangered", alpha=al)), 
                      xmin = 20.5, xmax = 23.5, ymin = -20, ymax = 20) +
    annotation_custom(grob=grid::rectGrob(gp=gpar(fill="red", alpha=al)), 
                      xmin = 23.5, xmax = 28.5, ymin = -20, ymax = 20) +
    annotation_custom(grob=grid::rectGrob(gp=gpar(fill="red4", alpha=al)), 
                      xmin = 28.5, xmax = 29.5, ymin = -20, ymax = 20)
  gb <- ggplot_build(gg)
  gt <- ggplot_gtable(gb)
  gt$layout$clip[gt$layout$name=="panel"] <- "off" # lets rectangles go off grid
  grid.draw(gt)
  
  ggsave("logreg.png")
}

plotVarImp <- function(fit){
  varImp <- varImp(fit)
  varImp <- varImp$importance
  varImp$category <- rownames(varImp)
  varImp$category <- cleanNames(varImp$category)
  varImp <- varImp[order(-varImp$Overall),]
  colorVec <- c("purple","green","green","orchid","orangered","orange",
                "orangered","green","green","green","blue",
                "blue","red4","blue","orangered","orchid","yellow",
                "red","yellow","yellow","red","blue","red","yellow",
                "red","yellow","blue","yellow","red")
  par(mar=c(13.1,4.1,1.1,1.1))
  barplot(varImp$Overall[1:10], names=varImp$category[1:10], 
          ylab = "Relative importance", las = 3, col=colorVec[1:10])
}



