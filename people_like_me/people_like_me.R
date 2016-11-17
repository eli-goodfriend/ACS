# make a map of the density of people like you in America
library(survey)
library(MonetDBLite)
library(DBI)
library(caret)
source("~/Dropbox/Code/ACS/people_like_me/preprocess.R")
source("~/Dropbox/Code/ACS/people_like_me/postprocess.R")
source("~/Dropbox/Code/ACS/people_like_me/plotter.R")

# open the database containing the datasets
dbfolder <- "~/Data/ACS/MonetDB"
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
tablename <- "acs14"

# determine the data we want, pull it, and clean it up
# TODO interactive
# TODO need to add the unique geography to the db
variables <- c("pwgtp", "geography"," sex")
dataset <- dbGetQuery(db, paste("SELECT", toString(variables),
                                "FROM", tablename))
dataset$sex <- factor(dataset$sex)

# figure out who is like me
dataset$likeme <- ifelse(dataset$sex == 1, 1, 0)
dataset$likeme <- factor(dataset$likeme)

# make the plot
plotTitle <- "Percent of people who are like me"
plotName <- "likeme.png"
plotByPUMA(dataset, plotTitle, plotName)




