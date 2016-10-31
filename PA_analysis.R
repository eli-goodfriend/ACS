# analyse ACS data using survey package and MonetDB database
# this is only PA right now because trying to make a survey design from
# the entire US caused my computer to crash
# cribbed heavily from asdfree by Anthony Damico
# TODO try the acs package?
library(survey)
library(MonetDBLite)
library(DBI)

load( '~/Data/ACS/pa_design.rda' )	# analyze the 2014 single year ACS
pa.acs <- open( pa.acs.design , driver = MonetDBLite() )

dbfolder <- "~/Data/ACS/MonetDB"
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
padata <- dbGetQuery(db, "SELECT pincp, education, degree, industry FROM acs14")
padata$education <- as.factor(padata$education)
padata$degree <- as.factor(padata$degree)
padata$industry <- as.factor(padata$industry)


# relp is relationship (to owner of house?)
# 1-17 is all possible answers
# 16 and 17 are group quarters
# 16 is institutional, 17 is noninstitutional
svytotal( ~I( relp %in% 0:17 ) , pa.acs)						# total population
svytotal( ~I( relp %in% 16:17 ) , pa.acs )						# gq population
svytotal( ~I( relp == 16 ) , pa.acs )							# gq institutional population
svytotal( ~I( relp == 17 ) , pa.acs )							# gq noninstitutional population
svyby( ~I( relp %in% 0:17 ) , ~ sex , pa.acs , svytotal )		# total males & females

svytotal( 
  ~I( agep %in% 0:4 ) +
    I( agep %in% 5:9 )   +
    I( agep %in% 10:14 ) +
    I( agep %in% 15:19 ) +
    I( agep %in% 20:24 ) +
    I( agep %in% 25:34 ) +
    I( agep %in% 35:44 ) +
    I( agep %in% 45:54 ) +
    I( agep %in% 55:59 ) +
    I( agep %in% 60:64 ) +
    I( agep %in% 65:74 ) +
    I( agep %in% 75:84 ) +
    I( agep %in% 85:100 ) , 
  pa.acs
)

# start by repeating the incorrect analysis I published for the whole US
# TODO comparing using repeated weights vs not?
pa.acs.hasincome <- subset(pa.acs, pincp > -20000) # income is bottom coded, so this is all
svymean(~pincp, pa.acs.hasincome)
svyquantile( ~pincp , pa.acs.hasincome , c( .25 , .5 , .75 ) )
#svyhist(~pincp, pa.acs.hasincome) # TODO why doesn't this work? package broken?

# is it better to have just a diploma or just a GED?
pa.acs.hasincome.hsonly <- subset(pa.acs.hasincome, schl == 16 | schl ==17)
svyquantile( ~pincp , pa.acs.hasincome.hsonly , c( .25 , .5 , .75 ) )
svyby(~pincp, ~schl, pa.acs.hasincome.hsonly, svyquantile, c(0.25,0.5,0.75))
svyranktest(pincp~schl, pa.acs.hasincome.hsonly) # diploma gives significantly more income

# income by level of education
educationQuantiles <- svyby(~pincp, ~education, pa.acs.hasincome, 
                            svyquantile, c(0.25,0.5,0.75))
educationRange <- aggregate(pincp ~ education, data=padata, range)
educationOrder <- c("No diploma or GED","High school","Some college","Associates",
                    "Batchelors","Masters","Doctoral","Professional")
source("~/Dropbox/Code/ACS/postprocess/boxplotFromQuantiles.R")
boxplotFromQuantiles(educationQuantiles, educationRange, educationOrder, 
                     "education", "PAincomeEducation.png")




close(pa.acs)


dbDisconnect( db , shutdown = TRUE)



