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
# TODO actually, is this necessary at all? can recalc in dataframe quickly for PA only
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
# TODO would the biglm package be useful here? EH, it cries when you don't bin the data right
interestingData <- c("serialno","povpip","agep","sex","rac1p","hisp","waob","cit",
                     "dis","qtrbir","mar","mil","schl","ddrs","dear","deye",
                     "dout","dphy","drem","lanx","relp","sch","anc","anc1p","anc2p",
                     "msp","nativity","pobp","rac2p","rac3p")
factorData <- interestingData[interestingData != "agep" & interestingData != "povpip" &
                                interestingData != "serialno"]
# logregData <- interestingData[interestingData != "serialno" & 
#                               interestingData != "povpip" &
#                               interestingData != "agep" & # use the cut version instead
#                               interestingData != "hisp"] # too fine grained
# logregData <- c(logregData, "agepf") # this takes too long to run
logregData <- c("agepf","sex","rac1p","waob","cit","dis","lanx")

source("~/Dropbox/Code/ACS/process/runLogisticRegression.R")
model <- runLogisticRegression(db, interestingData, factorData, logregData, "withoutMar")


# Before we did a logistic regression with just factors that a person has no control
# over.
# Now let's try with all factors that apply to all adults (e.g. not college degree)
# that don't directly deal with the kind of work the person may do (e.g. industry)
# or the kinds of assistance services the person may receive (e.g. medicare)
# These are all NA for children, so we'll restrict data to people 18 or older
# TODO so actually that ambitious plan is hard because it takes too long to run
# need to make it faster
interestingData <- c("serialno","povpip","agep","sex","rac1p","hisp","waob","cit",
                     "dis","qtrbir","mar","mil","schl","ddrs","dear","deye",
                     "dout","dphy","drem","lanx","relp","sch","anc","anc1p","anc2p",
                     "msp","nativity","pobp","rac2p","rac3p")
factorData <- interestingData[interestingData != "agep" & interestingData != "povpip" &
                              interestingData != "serialno"]
# logregData <- interestingData[interestingData != "serialno" & 
#                               interestingData != "povpip" &
#                               interestingData != "agep" & # use the cut version instead
#                               interestingData != "hisp"] # too fine grained
# logregData <- c(logregData, "agepf") # this takes too long to run
logregData <- c("agepf","sex","rac1p","waob","cit","dis","lanx",
                "mar") # these actually have significant effect

source("~/Dropbox/Code/ACS/process/runLogisticRegression.R")
model <- runLogisticRegression(db, interestingData, factorData, logregData, "withMar")

