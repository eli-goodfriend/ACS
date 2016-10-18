# read in the data from the csv files
library("ff")
library("ffbase")

personFileA <- "/home/eli/Data/ACS/ss14pusa.csv" 
personFileB <- "/home/eli/Data/ACS/ss14pusb.csv"
ffOutputName <- "/home/eli/Data/ACS/people.ff"
people <- read.csv.ffdf(file=personFileA, header=TRUE, VERBOSE=TRUE, 
                        first.rows=10000, next.rows=50000)
peopleB <- read.csv.ffdf(file=personFileB, header=TRUE, VERBOSE=TRUE, 
                         first.rows=10000, next.rows=50000)
people <- ffdfappend(people,peopleB)

serialno <- people$SERIALNO
PUMA <- people$PUMA
state <- people$ST
industry <- people$INDP
timesMarried <- people$MARHT
weight <- people$PWGTP
age <- people$AGEP
transportToWork <- people$JWTR
maritalStatus <- people$MAR
highestDegree <- people$SCHL
gender <- people$SEX # 1 = male, 2 = female. of course.
wages <- people$WAGP # past twelve months
workHoursPerWeek <- people$WKHP
fieldOfDegree1 <- people$FOD1P # at least a Bachelor's
fieldOfDegree2 <- people$FOD2P
totalIncome <- people$PINCP # signed, can be negative
incomeToPovertyRatio <- people$POVPIP
race <- people$RAC1P # the most coarse-grained of the race variables
#TODO Hispanic ethnicity, immigration background

# construct unique geography codes from state and PUMA
PUMA_ram <- as.ram(PUMA)
PUMA_ram <- sprintf("%05d",as.numeric(PUMA_ram))
states_ram <- as.ram(state)
states_ram <- sprintf("%02d",as.numeric(states_ram))
geography <- paste(states_ram,PUMA_ram,sep="")
geography <- as.ff(as.numeric(geography)) # when unpacking, need to add leading 0

dataToWrite <- ffdf(serialno, PUMA, state, industry, timesMarried, weight, age,
                    transportToWork, maritalStatus, highestDegree, gender,
                    wages, workHoursPerWeek, fieldOfDegree1, fieldOfDegree2,
                    totalIncome, incomeToPovertyRatio, race, geography)
write.csv.ffdf(dataToWrite, file=ffOutputName)
