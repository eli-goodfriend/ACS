# use logistic regression to predict income in PA
# using the survey package to account for survey weights
library(survey)
library(MonetDBLite)
library(DBI)

# open the database containing the datasets and specify the PA only table
dbfolder <- "~/Data/ACS/MonetDB"
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
tablename <- "acs14pa"

# get a cutoff id for splitting data into training and testing
sql <- paste("SELECT MIN(serialno) FROM", tablename)
minSerialNo <- dbGetQuery(db, sql)
sql <- paste("SELECT MAX(serialno) FROM", tablename)
maxSerialNo <- dbGetQuery(db, sql)
cutoffID <- minSerialNo + (maxSerialNo - minSerialNo)*0.8

interestingData <- c("serialno","povpip","agep","sex","rac1p","hisp","waob","cit",
                     "dis","qtrbir","mar","mil","schl","ddrs","dear","deye",
                     "dout","dphy","drem","lanx","relp","sch","anc","anc1p","anc2p",
                     "msp","nativity","pobp","rac2p","rac3p","pincp",
                     "racaian","racas","racblk","racnh","racnum","racpi","racsor","racwht")
factorData <- interestingData[interestingData != "agep" & interestingData != "povpip" &
                                interestingData != "serialno" & interestingData != "pincp"]
logregData <- c("agepf","sex","rac1p","waob","cit","dis","lanx")

source("~/Dropbox/Code/ACS/process/runLogisticRegressionWeighted.R")
model <- runLogisticRegressionWeighted(db, interestingData, factorData, logregData, "withoutMar")


