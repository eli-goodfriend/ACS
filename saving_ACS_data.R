# read in the data from the csv files
library("ff")
library("ffbase")

personFileA <- "/home/eli/Data/ACS/ss14pusa.csv" 
personFileB <- "/home/eli/Data/ACS/ss14pusb.csv"
ffOutputName <- "/home/eli/Data/ACS/people.ff"
people <- read.csv.ffdf(file=personFileA, header=TRUE, VERBOSE=TRUE, 
                        first.rows=10000, next.rows=50000)
#peopleB <- read.csv.ffdf(file=personFileB, header=TRUE, VERBOSE=TRUE, 
#                         first.rows=10000, next.rows=50000)

# pull interesting data, combine, and write
serialno <- people$SERIALNO
geography <- people$PUMA
industry <- people$INDP
timesMarried <- people$MARHT
timeLeaveForWork <- people$JWDP

dataToWrite <- ffdf(serialno, geography, industry, 
                    timesMarried, timeLeaveForWork)
write.csv.ffdf(dataToWrite, file=ffOutputName)
