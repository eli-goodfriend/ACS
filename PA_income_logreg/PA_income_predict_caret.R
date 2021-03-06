# let's try using the caret package to automate some training stuff
library(survey)
library(MonetDBLite)
library(DBI)
library(caret)
source("~/Dropbox/Code/ACS/PA_income_logreg/cleanData_income.R")
source("~/Dropbox/Code/ACS/PA_income_logreg/assessFit_income.R")

# open the database containing the datasets and specify the PA only table
dbfolder <- "~/Data/ACS/MonetDB"
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
tablename <- "acs14pa"

# load and clean data
interestingData <- c("serialno","povpip","agep","sex","rac1p","hisp","waob",
                     "cit","dis","qtrbir","mar","mil","schl","ddrs","dear",
                     "deye","dout","dphy","drem","lanx","relp","sch","anc",
                     "anc1p","anc2p","msp","nativity","pobp","rac2p","rac3p",
                     "pincp","racaian","racasn","racblk","racnh","racnum",
                     "racpi","racsor","racwht","education","cow",
                     "type_","hhl","hht","mv","partner","ssmc")
dataset <- dbGetQuery(db, paste("SELECT", toString(interestingData),
                                "FROM", tablename))
factorData <- interestingData[interestingData != "agep" & 
                              interestingData != "povpip" &
                              interestingData != "serialno" & 
                              interestingData != "pincp"]
dataset[factorData] <- lapply(dataset[factorData], factor)
dataset <- cleanData(dataset)
dataset <- dataset[complete.cases(dataset), ] # TODO check how much this removes

# define success (and other canonically difficult tasks)
dataset$success <- factor(ifelse(dataset$povpip < 100, "yes", "no"))

# divide into a test and training set
# the authors of this class are real people, since they use camelCase instead of
# the usual.r.terrible.formatting
inTrain <- createDataPartition(y = dataset$success, p = .75, list = FALSE)
training <- dataset[inTrain,]
testing <- dataset[-inTrain,]

# --- logistic regression!
tc <- trainControl("cv", 10, savePredictions = T,
                   summaryFunction = twoClassSummary, 
                   classProbs = TRUE,
                   sampling = "down")
logregFit <- train(success ~ 
                     mv + hht + cow + education + waob +
                     dis + agepf +
                     racaian + raclat + racwht + racasn + racblk +
                     sex, # in inverse order of plotting
                data = training, 
                method = "glm", family = "binomial", 
                metric = "ROC",
                trControl = tc)

plotVarImp(logregFit)
getConfusionMatrix(logregFit, testing)
getAUC(logregFit, testing)
plotOddsRatios(logregFit)


# # --- support vector machine!
# # takes much longer than logreg and is no more accurate
# tc <- trainControl("cv", 10, savePredictions = T,
#                    #summaryFunction = twoClassSummary, # these cause name problems
#                    #classProbs = TRUE,
#                    sampling = "down")
# svmFit <- train(success ~ 
#                      sex + waob + dis + mar + agepf + education + cow +
#                      racaian + racasn + racblk + racwht + raclat +
#                      hhl + hht + mv + partner + ssmc, 
#                    data = training, 
#                    method = "svmRadial", 
#                    #metric = "ROC", # need the broken summaryFunction and classProbs
#                    trControl = tc)
# 
# getConfusionMatrix(svmFit, testing)
# getAUC(svmFit, testing)
# plotOddsRatios(svmFit)






