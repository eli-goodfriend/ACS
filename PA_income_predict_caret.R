# let's try using the caret package to automate some training stuff
library(survey)
library(MonetDBLite)
library(DBI)
library(caret)
source("~/Dropbox/Code/ACS/preprocess/cleanData.R")
source("~/Dropbox/Code/ACS/postprocess/plotOddsRatios.R")

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

# define success (and other canonically difficult tasks)
# let's say success is having a poverty ratio of at least 2
dataset$success <- factor(ifelse(dataset$povpip >200, 1, 0))
dataset <- dataset[complete.cases(dataset), ] # TODO check how much this removes

# divide into a test and training set
# the authors of this class are real people, since they use camelCase instead of
# the usual.r.terrible.formatting
inTrain <- createDataPartition(y = dataset$success, p = .75, list = FALSE)
training <- dataset[inTrain,]
testing <- dataset[-inTrain,]

# train it, using caret
tc <- trainControl("cv", 10, savePredictions = T,
                   #summaryFunction = twoClassSummary, # these cause name problems
                   #classProbs = TRUE,
                   sampling = "down")
logregFit <- train(success ~ 
                  sex + waob + dis + mar + agepf + education + cow +
                  racaian + racasn + racblk + racwht + raclat +
                  hhl + hht + mv + partner + ssmc, 
                data = training, 
                method = "glm", family = "binomial", 
                #metric = "ROC", # need the broken summaryFunction and classProbs
                trControl = tc)

# test it, also using caret
logregClasses <- predict(logregFit, newdata = testing)
confusionMatrix(data = logregClasses, testing$success)
library(ROCR)
p <- predict(logregFit, newdata = testing, type="prob")[,2] # why [,2]? dunno
pr <- prediction(p, testing$success)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# compute and plot the odds ratios
plotOddsRatios(logregFit)

