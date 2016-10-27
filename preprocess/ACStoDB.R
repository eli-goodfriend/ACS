# cribbed heavily from asdfree by Anthony Damico
library(survey)
library(MonetDBLite)
library(DBI)

# name the database files in the "MonetDB" folder of the current working directory
dbfolder <- "~/Data/ACS/MonetDB"

# open the connection to the monetdblite database
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )

#-----------------------------------------------------------------------------------
# load the person data
#-----------------------------------------------------------------------------------
# create the table name
tablename <- "acs14p"

# initiate the table in the database using any of the csv files #
csvpath <- "~/Data/ACS/ss14pusa.csv"

# read in the first five hundred records of the csv file
headers <- read.csv( csvpath , nrows = 500 )

# figure out the column type (class) of each column
cl <- sapply( headers , class )

# convert all column names to lowercase
names( headers ) <- tolower( names( headers ) )

# if one of the column names is the word 'type'
# change it to 'type_' -- monetdb doesn't like columns called 'type'
if ( 'type' %in% tolower( names( headers ) ) ){
  print( "warning: column name 'type' unacceptable in monetdb.  changing to 'type_'" )
  names( headers )[ names( headers ) == 'type' ] <- 'type_'
  
  headers.h[ headers.h == 'type' ] <- 'type_'
}

# the american community survey data only contains integers and character strings..
# so store integer columns as numbers and all others as characters
# note: this won't work on other data sets, since they might have columns with non-integers (decimals)
colTypes <- ifelse( cl == 'integer' , 'INT' , 'VARCHAR(255)' )

# create a character vector grouping each column name with each column type..
colDecl <- paste( names( headers ) , colTypes )

# ..and then construct an entire 'create table' sql command
sql <-
  sprintf(
    paste(
      "CREATE TABLE" ,
      tablename ,
      "(%s)"
    ) ,
    paste(
      colDecl ,
      collapse = ", "
    )
  )

# actually execute the 'create table' sql command
dbSendQuery( db , sql )

# try to add the full csv to the table: part 1
# get rid of any comma-space-comma values.
incon <- file( csvpath , "r") 
tf_out <- tempfile()
outcon <- file( tf_out , "w") 
while( length( line <- readLines( incon , 1 ) ) > 0 ){
  # remove all whitespace
  line <-  gsub( ", ," , ",," , gsub( ",( +)," , ",," , line ) )
  writeLines( line , outcon )
}

close( outcon )
close( incon , add = TRUE )

# and add to the table
dbSendQuery( 
      db , 
      paste0( 
        "copy offset 2 into " , 
        tablename , 
        " from '" , 
        normalizePath( tf_out ) , 
        "' using delimiters ',','\\n','\"'  NULL AS ''" 
      ) 
) 

# do it again for part b
# TODO c/p bad
csvpath <- "~/Data/ACS/ss14pusb.csv"
incon <- file( csvpath , "r") 
tf_out <- tempfile()
outcon <- file( tf_out , "w") 
while( length( line <- readLines( incon , 1 ) ) > 0 ){
  # remove all whitespace
  line <-  gsub( ", ," , ",," , gsub( ",( +)," , ",," , line ) )
  writeLines( line , outcon )
}

close( outcon )
close( incon , add = TRUE )

dbSendQuery( 
  db , 
  paste0( 
    "copy offset 2 into " , 
    tablename , 
    " from '" , 
    normalizePath( tf_out ) , 
    "' using delimiters ',','\\n','\"'  NULL AS ''" 
  ) 
) 

#-----------------------------------------------------------------------------------
# load the housing data
#-----------------------------------------------------------------------------------
# create the table name
tablename <- "acs14h"

# initiate the table in the database using any of the csv files #
csvpath <- "~/Data/ACS/ss14husa.csv"

# read in the first five hundred records of the csv file
headers <- read.csv( csvpath , nrows = 500 )

# figure out the column type (class) of each column
cl <- sapply( headers , class )

# convert all column names to lowercase
names( headers ) <- tolower( names( headers ) )

# if one of the column names is the word 'type'
# change it to 'type_' -- monetdb doesn't like columns called 'type'
if ( 'type' %in% tolower( names( headers ) ) ){
  print( "warning: column name 'type' unacceptable in monetdb.  changing to 'type_'" )
  names( headers )[ names( headers ) == 'type' ] <- 'type_'
  
  #headers.h[ headers.h == 'type' ] <- 'type_'
}

# the american community survey data only contains integers and character strings..
# so store integer columns as numbers and all others as characters
# note: this won't work on other data sets, since they might have columns with non-integers (decimals)
colTypes <- ifelse( cl == 'integer' , 'INT' , 'VARCHAR(255)' )

# create a character vector grouping each column name with each column type..
colDecl <- paste( names( headers ) , colTypes )

# ..and then construct an entire 'create table' sql command
sql <-
  sprintf(
    paste(
      "CREATE TABLE" ,
      tablename ,
      "(%s)"
    ) ,
    paste(
      colDecl ,
      collapse = ", "
    )
  )

# actually execute the 'create table' sql command
dbSendQuery( db , sql )

# try to add the full csv to the table: part 1
# get rid of any comma-space-comma values.
incon <- file( csvpath , "r") 
tf_out <- tempfile()
outcon <- file( tf_out , "w") 
while( length( line <- readLines( incon , 1 ) ) > 0 ){
  # remove all whitespace
  line <-  gsub( ", ," , ",," , gsub( ",( +)," , ",," , line ) )
  writeLines( line , outcon )
}

close( outcon )
close( incon , add = TRUE )

# and add to the table
dbSendQuery( 
  db , 
  paste0( 
    "copy offset 2 into " , 
    tablename , 
    " from '" , 
    normalizePath( tf_out ) , 
    "' using delimiters ',','\\n','\"'  NULL AS ''" 
  ) 
) 

# do it again for part b
# TODO c/p bad! very bad!
csvpath <- "~/Data/ACS/ss14husb.csv"
incon <- file( csvpath , "r") 
tf_out <- tempfile()
outcon <- file( tf_out , "w") 
while( length( line <- readLines( incon , 1 ) ) > 0 ){
  # remove all whitespace
  line <-  gsub( ", ," , ",," , gsub( ",( +)," , ",," , line ) )
  writeLines( line , outcon )
}

close( outcon )
close( incon , add = TRUE )

dbSendQuery( 
  db , 
  paste0( 
    "copy offset 2 into " , 
    tablename , 
    " from '" , 
    normalizePath( tf_out ) , 
    "' using delimiters ',','\\n','\"'  NULL AS ''" 
  ) 
) 

#----------------------------------------------------------------------------------
# merge personal and household data tables
#----------------------------------------------------------------------------------
# pull all fields from the person..
pfields <- names( dbGetQuery( db , paste0( "select * from acs14p limit 1") ) )
# ..and household tables
hfields <- names( dbGetQuery( db , paste0( "select * from acs14h limit 1") ) )

# then throw fields out of the person file that match fields in the household table
pfields <- pfields[ !( pfields %in% hfields ) ]
# and also throw out the 'rt' field from the household table
hfields <- hfields[ hfields != 'rt' ]

# construct a massive join statement		
sql <-
  paste0(
    "create table " ,					# create table statement
    "acs14 as select " ,				# select from statement
    "'M' as rt, " ,
    paste( paste0( 'a.' , hfields ) , collapse = ", " ) ,
    ", " ,
    paste( pfields , collapse = ", " ) ,
    " from acs14h as a inner join acs14p as b " ,
    "on a.serialno = b.serialno with data" 
  )
dbSendQuery( db , sql )

#----------------------------------------------------------------------------------------
# make some edits to the merged table to make it more useful
#----------------------------------------------------------------------------------------
# add a unique ID for each entry by combining serialno and sporder
dbSendQuery(db, "ALTER TABLE acs14 ADD COLUMN uniqueid INT;")
dbSendQuery(db, "UPDATE acs14 SET uniqueid = serialno + 100000000*sporder;")

# do some data binning, for ease of future analyses
# TODO documentation suggests I can add these to the svrepdesign object later
# but I couldn't figure out how; maybe the functions are too complicated?
# anyway, not hard to add to the main db
addtablename <- "newcolumns"

edu <- dbGetQuery(db, "SELECT schl FROM acs14") # education
education <- edu$schl
source("~/Dropbox/Code/ACS/preprocess/group_education.R")
education <- group_education(education)

deg <- dbGetQuery(db, "SELECT fod1p FROM acs14") # college+ degree
degree <- deg$fod1p
source("~/Dropbox/Code/ACS/preprocess/group_degree.R")
degree <- group_degree(degree)

ind <- dbGetQuery(db, "SELECT indp FROM acs14") # industry
industry <- ind$indp
source("~/Dropbox/Code/ACS/preprocess/group_industry.R")
industry <- group_industry(industry)

personid <- dbGetQuery(db, "SELECT uniqueid FROM acs14")
uniqueid <- personid$uniqueid
newColumns <- data.frame(uniqueid, education, degree, industry)
dbWriteTable(db, addtablename, newColumns)

# TODO loop this up
dbSendQuery(db, "ALTER TABLE acs14 ADD COLUMN education VARCHAR(255);")
dbSendQuery(db, "ALTER TABLE acs14 ADD COLUMN degree VARCHAR(255);")
dbSendQuery(db, "ALTER TABLE acs14 ADD COLUMN industry VARCHAR(255);")
dbSendQuery(db, "UPDATE acs14 SET education=(SELECT newcolumns.education FROM newcolumns WHERE newcolumns.uniqueid = acs14.uniqueid)")
dbSendQuery(db, "UPDATE acs14 SET degree=(SELECT newcolumns.degree FROM newcolumns WHERE newcolumns.uniqueid = acs14.uniqueid)")
dbSendQuery(db, "UPDATE acs14 SET industry=(SELECT newcolumns.industry FROM newcolumns WHERE newcolumns.uniqueid = acs14.uniqueid)")
dbRemoveTable(db, addtablename)

#----------------------------------------------------------------------------------------
# # make a survey object  
#----------------------------------------------------------------------------------------
# # TODO this crashes the whole dang computer. Because computer or because code?
# # others on the internet seem to get this code to run fine, so thinking computer
# options( "survey.replicates.mse" = TRUE) 
# acs.design <- 									# name the survey object
#   svrepdesign(									
#     weight = ~pwgtp , 							# person-level weights are stored in column "pwgtp"
#     repweights = 'pwgtp[0-9]+' ,				# the acs contains 80 replicate weights, pwgtp1 - pwgtp80.  this [0-9] format captures all numeric values
#     scale = 4 / 80 ,
#     rscales = rep( 1 , 80 ) ,
#     mse = TRUE ,
#     type = 'JK1' ,
#     data = tablename , 				
#     dbtype = "MonetDBLite" ,
#     dbname = dbfolder
#   )
# # workaround for a bug in survey::svrepdesign.character
# acs.design$mse <- TRUE

# let's just look at Pennsylvania, that's smaller anyway
# start with just person data
tablename <- "acs14"
PAtablename <- "acs14pa"
PAnum <- 42 # Pennsylvania's state number

sql <- paste("CREATE TABLE", PAtablename,
             "AS SELECT * FROM", tablename,
             "WHERE st=", PAnum, ";")
dbSendQuery( db , sql )

# make a replicant weight survey design for PA
# this runs to completion!! YAY!
pa.acs.design <- 									# name the survey object
  svrepdesign(
    weight = ~pwgtp , 							# person-level weights are stored in column "pwgtp"
    repweights = 'pwgtp[0-9]+' ,				# the acs contains 80 replicate weights, pwgtp1 - pwgtp80.  this [0-9] format captures all numeric values
    scale = 4 / 80 ,
    rscales = rep( 1 , 80 ) ,
    mse = TRUE ,
    type = 'JK1' ,
    data = PAtablename ,
    dbtype = "MonetDBLite" ,
    dbname = dbfolder
  )
# workaround for a bug in survey::svrepdesign.character
pa.acs.design$mse <- TRUE

save( pa.acs.design , file = "~/Data/ACS/pa_design.rda" )

# close the connection to the design object
close( pa.acs.design )

# remove from memory
rm( pa.acs.design)

# clear up RAM
gc()

# disconnect from the current monet database
dbDisconnect( db , shutdown = TRUE )
