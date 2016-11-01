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









