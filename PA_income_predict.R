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
                     "msp","nativity","pobp","rac2p","rac3p","pincp",
                     "racaian","racasn","racblk","racnh","racnum","racpi",
                     "racsor","racwht","education")
factorData <- interestingData[interestingData != "agep" & interestingData != "povpip" &
                                interestingData != "serialno" & interestingData != "pincp"]
# logregData <- interestingData[interestingData != "serialno" & 
#                               interestingData != "povpip" &
#                               interestingData != "agep" & # use the cut version instead
#                               interestingData != "hisp"] # too fine grained
# logregData <- c(logregData, "agepf") # this takes too long to run
source("~/Dropbox/Code/ACS/process/runLogisticRegression.R")

logregData <- c("agepf","sex","racaian","racasn","racblk","racsor","racwht",
                "waob","dis")
povpipb_withoutMar <- runLogisticRegression(db, tablename, interestingData, factorData, logregData,
                                            "withoutMar","povpipb")
rich_withoutMar <- runLogisticRegression(db, tablename, interestingData, factorData, logregData,
                               "withoutMar","rich")

# Before we did a logistic regression with just factors that a person has no control
# over.
# Now let's try with all factors that apply to all adults (e.g. not college degree)
# that don't directly deal with the kind of work the person may do (e.g. industry)
# or the kinds of assistance services the person may receive (e.g. medicare)
# These are all NA for children, so we'll restrict data to people 18 or older
# TODO so actually that ambitious plan is hard because it takes too long to run
# need to make it faster
logregData <- c("agepf","sex","racaian","racasn","racblk","racsor","racwht",
                "mar","lanx","waob","dis","education")

pov_all <- runLogisticRegression(db, tablename, interestingData, factorData, logregData,
                               "all","povpipb")
rich_all <- runLogisticRegression(db, tablename, interestingData, factorData, logregData,
                               "all","rich")

# plot all the prf lines together for comparison
# TODO more compact way of doing this?
png(file="performance_together.png")
plot(povpipb_withoutMar$prf, lwd=3, lty=6)
par(new=TRUE)
plot(rich_withoutMar$prf, lwd=3)
legend("bottomright", c("In bottom 10%","In top 10%"),
       lwd=3, lty=c(6,1), cex=1.5)
dev.off()

# can we do better with support vector machines?
# TODO I have no idea how these work
# TODO in what contexts is one method likely to work better than another?
#      or perhaps SVM are just slower and better than logreg?
library(e1071)
source("~/Dropbox/Code/ACS/process/runSVM.R")

svmData <- c("agepf","sex","racaian","racasn","racblk","racsor","racwht",
                "mar","mil","lanx")
povpipb_withoutMar <- runSVM(db, tablename, interestingData, factorData, svmData,
                             "withoutMar","povpipb") # takes so long.

