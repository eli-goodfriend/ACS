# Can I predict a person's total income based on other data points from the ACS?
# aka playing with machine learning and pattern recognition in R
# start with PA alone, to make an easier dataset to work with
# start with ignoring weights, to learn more basic techniques first
library(survey)
library(MonetDBLite)
library(DBI)

# open the database containing the datasets and specify the PA only table
dbfolder <- "~/Data/ACS/MonetDB"
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
tablename <- "acs14pa"

# most of our data is in terms of factors, so let's turn total income
# into a factor using binning. then we can use the data in the usual
# all-discrete data algorithms
# TODO should this be in the ACStoDB.R routines? yes, bc should only run once
# TODO come on now, loop it
addtablename <- "newcolumns"
cnts <- dbGetQuery(db, paste("SELECT uniqueid, pincp, agep, jwmnp, wkhp FROM", tablename))
cnts$pincp <- cut(cnts$pincp, 100) # this should be done more carefully
cnts$agep <- cut(cnts$agep, 20)
cnts$jwmnp <- cut(cnts$jwmnp, 10)
cnts$wkhp <- cut(cnts$wkhp, 20)
dbWriteTable(db, addtablename, cnts)

dbSendQuery(db, paste("ALTER TABLE", tablename, "ADD COLUMN pincpf VARCHAR(255);"))
dbSendQuery(db, paste("ALTER TABLE", tablename, "ADD COLUMN agepf VARCHAR(255);"))
dbSendQuery(db, paste("ALTER TABLE", tablename, "ADD COLUMN jwmnpf VARCHAR(255);"))
dbSendQuery(db, paste("ALTER TABLE", tablename, "ADD COLUMN wkhpf VARCHAR(255);"))
dbSendQuery(db, paste("UPDATE", tablename, "SET pincpf=(SELECT", addtablename,".pincp FROM", 
                      addtablename, "WHERE ", addtablename,".uniqueid =", tablename,".uniqueid)"))
dbSendQuery(db, paste("UPDATE", tablename, "SET agepf=(SELECT", addtablename,".agep FROM", 
                      addtablename, "WHERE ", addtablename,".uniqueid =", tablename,".uniqueid)"))
dbSendQuery(db, paste("UPDATE", tablename, "SET jwmnpf=(SELECT", addtablename,".jwmnp FROM", 
                      addtablename, "WHERE ", addtablename,".uniqueid =", tablename,".uniqueid)"))
dbSendQuery(db, paste("UPDATE", tablename, "SET wkhpf=(SELECT", addtablename,".wkhp FROM", 
                      addtablename, "WHERE ", addtablename,".uniqueid =", tablename,".uniqueid)"))
dbRemoveTable(db, addtablename)

# split the data into a training and a test set
trainTablename <- "acs14patrain"
testTablename <- "acs14patest"

sql <- paste("SELECT MIN(serialno) FROM", tablename)
minSerialNo <- dbGetQuery(db, sql)
sql <- paste("SELECT MAX(serialno) FROM", tablename)
maxSerialNo <- dbGetQuery(db, sql)

cutoffID <- minSerialNo + (maxSerialNo - minSerialNo)*0.8

sql <- paste("CREATE TABLE", trainTablename,
             "AS SELECT * FROM", tablename,
             "WHERE serialno<", cutoffID[1,1], ";")
dbSendQuery( db , sql )
sql <- paste("CREATE TABLE", testTablename,
             "AS SELECT * FROM", tablename,
             "WHERE serialno>=", cutoffID[1,1], ";")
dbSendQuery( db , sql )


# try doing a linear fit to total income with person (not housing) level
# numeric quantities (not factors) and not income information:
# age (agep), travel time to work (jwmnp), hours worked per week (wkhp),
# that's it for numeric variables, although other variables could be
# converted
# note that this does not use the survey design, so it's wrong
lm_data <- dbGetQuery(db,paste(
  "SELECT pincp, agep, jwmnp, wkhp FROM",trainTablename))
test_data <- dbGetQuery(db,paste(
  "SELECT pincp, agep, jwmnp, wkhp FROM",testTablename))
# most basic possible model: one dependent, one independent
# does your total yearly income depend on your weekly hours worked?
lm_model <- lm(pincp ~ wkhp, data=lm_data)
plot(pincp ~ wkhp, data=lm_data, xlab="Hours worked per week",
     ylab="Total yearly income")
abline(a=coef(lm_model)[1], b=coef(lm_model)[2])
plot(resid(lm_model) ~ fitted(lm_model)) # this is a bad fit! even of training data!
# slighly more complicated model: include age and travel time too
lm_model <- lm(pincp ~ agep + jwmnp + wkhp, data=lm_data)
plot(resid(lm_model) ~ fitted(lm_model)) # still really bad
predicted <- predict(lm_model, test_data)
plot(predicted, test_data$pincp) # yup, not even close


# let's try logistic regression with the binned income data
# the full case would be ordinal logistic regression, and is maybe not natural for this data set
# since these methods are actually trying to work around data being binary or otherwise discrete
# well, it's a learning experience anyway
# first, a simpler case: can I predict whether someone is living below the poverty line?
# povpip gives a numerical value of the income-to-poverty ratio as a percent, so
# povpip = 100 means that the person's income is at the poverty line
# we also want to look at age, gender, race, ...
# TODO would the biglm package be useful here?
data <- dbGetQuery(db, paste("SELECT 
                             serialno, povpip, agep, sex, rac1p FROM", tablename))
library(Amelia)
missmap(data)
data <- data[complete.cases(data), ] # TODO this hammer will be too big in future
data$povpipb <- ifelse(data$povpip > 100, 1, 0) # if above poverty line, success!
data$povpipb <- factor(data$povpipb)
data$sex <- factor(data$sex)
data$rac1p <- factor(data$rac1p)

train_data <- data[data$serialno <  cutoffID[1,1],]
test_data  <- data[data$serialno >= cutoffID[1,1],]

glm_mod <- glm(povpipb ~ agep + sex + rac1p, 
               family=binomial(link="logit"), data=train_data)

fitted_results <- predict(glm_mod, newdata=test_data, type="response")
fitted_results <- ifelse(fitted_results > 0.5, 1, 0)
missclassError <- mean(fitted_results != test_data$povpipb, na.rm = TRUE)
print(paste("Accuracy", 1-missclassError)) # good!

library(ROCR)
p <- predict(glm_mod, newdata=test_data, type="response")
pr <- prediction(p, test_data$povpipb)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc # not so good

# TODO could apriori be useful? needs binary data, but we have that




