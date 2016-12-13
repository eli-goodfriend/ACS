# what variables should people be able to look up?
possible.categories <- list(
  Housing = c("Type of unit" = "type_", 
              "Number of people at this housing unit" = "np",
              "Access to the internet" = "access",
              "Lot size (single family home)" = "acr",
              "Yearly sales of agricultural products" = "ags",
              "Bathtub or shower" = "bath"),
  Demographics = c("Gender" = "sex",
                   "Marital status" = "mar")
)

# what values do those take on?
possibleValues <- c("")

getPossibleValues <- function(colname){
  # this is janky AF
  # TODO this should be a lookup dataframe or something
  switch(colname,
         type_ = {possibleValues <- c("Housing unit" = "1", "Institutional group quarters" = "2", "Non-institutional group quarters" = 3)},
         np = {possibleValues <- c("Enter an integer between 1 and 20")},
         acr = {possibleValues <- c("Less than one acre" = "1", "One to ten acres" = "2", "Ten or more acres" = "3")},
         ags = {possibleValues <- c("None" = "1","$1 - $999" = "2","$1000 - $2499" = "3","$2500 - $4999" = "4","$5000 - $9999" = "5","$10000 +","6")},
         access = {possibleValues <- c("Yes, with subscription to an internet service" = "1", "Yes, without a subscription to an internet service" = "2", "No internet access at home" = "3")},
         bath = {possibleValues <- c("Yes" = "1","No" = "2")},
         sex = {possibleValues <- c("Male" = "1", "Female" = "2")},
         mar = {possibleValues <- c("Married" = "1", "Widowed" = "2", "Divorced" = "3", "Separated" = "4", "Never married" = "5")}
  )
  return(possibleValues)
}

# is the category a factor or numeric?
getIsFactor <- function(colname){
  # this is also janky AF
  isFactor <- -1
  if (colname == 'np'){
    isFactor <- 0
  } else if (colname == 'type_'){
    isFactor <- 1
  }
  return(isFactor)
}



