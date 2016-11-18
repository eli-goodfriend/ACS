# make a smaller table that just has the data we need
library(MonetDBLite)
library(DBI)

dbfolder <- "~/Data/ACS/MonetDB"
db <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
oldtablename <- "acs14"
tablename <- "acs14lite"

# copy the old table to the new table
sql <- paste("CREATE TABLE", tablename,
             "AS SELECT * FROM", oldtablename)
dbSendQuery(db, sql)

# remove the repeated weights, since we aren't using them
for (idx in seq(80)){
  colname <- paste0("pwgtp",idx)
  sql <- paste("ALTER TABLE ", tablename, "DROP COLUMN ", colname)
  dbSendQuery(db, sql)
  colname <- paste0("wgtp",idx)
  sql <- paste("ALTER TABLE ", tablename, "DROP COLUMN ", colname)
  dbSendQuery(db, sql)
}

toRemove <- c("rt","hotwat","faccessp","facrp","fagsp","fbathp","fbdsp",
              "fbldp","fbroadbndp","fbusp","fcompothxp","fconp",
              "fdialupp","fdslp","felep","ffiberopp","ffsp","ffulp",
              "fgasp","fhandheldp","fhflp","fhotwatp","finsp","fkitp",
              "flaptopp","fmhp","fmodemp","fmrgip","fmrgp","fmrgtp",
              "fmrgxp","fmvp","fothsvcexp","fplmp","frefrp","frmsp",
              "frntmp","frntp","frwatp","frwatprp","fsatellitep",
              "fsinkp","fsmp","fsmxhp","fsmxsp","fstovp","ftaxp",
              "ftelp","ftenp","ftoilp","fvacsp","fvalp","fvehp","fwatp",
              "fyblp","ffincp","fgrntp","fhincp","fplmprp","fsmocp",
              "fagep","fancp","fcitp","fcitwp","fcowp","fddrsp","fdearp",
              "fdeyep","fdisp","fdoutp","fdphyp","fdratp","fdratxp",
              "fdremp","fengp","fesrp","fferp","ffodp","fgclp","fgcmp",
              "fgcrp","fhins1p","fhins2p","fhins3p","fhins4p","fhins5p",
              "fhins6p","fhins7p","fhisp","findp","fintp","fjwdp",
              "fjwmnp","fjwrip","fjwtrp","flanp","flanxp","fmarhdp",
              "fmarhmp","fmarhtp","fmarhwp","fmarhyp","fmarp","fmigp",
              "fmigsp","fmilpp","fmilsp","foccp","foip","fpap","fpernp",
              "fpincp","fpobp","fpowsp","fprivcovp","fpubcovp","fracp",
              "frelp","fretp","fschgp","fschlp","fschp","fsemp","fsexp",
              "fssip","fssp","fwagp","fwkhp","fwklp","fwkwp","fwrkp",
              "fyoep")
for (colname in toRemove){
  sql <- paste("ALTER TABLE ", tablename, "DROP COLUMN ", colname)
  dbSendQuery(db, sql)
}



