# make a map of the density of people like you in America
# who am I?
source("~/Dropbox/Code/ACS/people_like_me/plotter.R")
criteriaIn <- c("education.==.'High school'",
                "agep.>.21")
likeMe(criteriaIn)




