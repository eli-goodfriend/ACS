boxplotFromQuantiles <- function(quartiles, range, order, variablename, filename){
  range$min <- range$pincp[,1]
  range$max <- range$pincp[,2]
  quartiles$IQR <- quartiles$V3 - quartiles$V1
  size <- eval(parse( text=paste(
         "svyby(~I(sex<3), ~ ", variablename,", pa.acs , svytotal )") ))
  data <- merge(quartiles, range, by=variablename)
  data <- merge(data, size, by=variablename)
  rownames(data) <- eval(parse( text=paste(
                    "data$",variablename) ))
  data <- data[order,]
  data$bottom <- data$min # minimum income is always within 1.5 IQR # TODO actually test
  data$top <- data$IQR * 1.5 + data$V3 # maximum income never is # TODO actually test
  size <- data$`I(sex < 3)TRUE`
  dataToPlot <- t(data[, c("bottom","V1","V2","V3","top")])
  png(file=filename)
  par(mar=c(10.1,4.1,1.3,1.1))
  bxp(list(stats=dataToPlot, n=size, names=rownames(data)),
      outline=FALSE, varwidth=TRUE, las=3,
      ylab="Total income $/year")
  dev.off()
}
