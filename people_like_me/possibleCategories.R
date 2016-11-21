# what variables should people be able to look up?
possible.categories <- list(
  Housing = c("Type of unit" = "type_", 
              "Access to the internet" = "access"),
  Demographics = c("Gender" = "sex",
                   "Marital status" = "mar")
)

# what values do those take on?
possible.values <- c("")


getPossibleValues <- function(colname){
  # this is janky AF
  # TODO this should be a lookup dataframe or something
  switch(colname,
         type_ = {possible.values <- c("Housing unit" = "1", "Institutional group quarters" = "2", "Non-institutional group quarters" = 3)},
         access = {possible.values <- c("Yes, with subscription to an internet service" = "1", "Yes, without a subscription to an internet service" = "2", "No internet access at home" = "3")},
         sex = {possible.values <- c("Male" = "1", "Female" = "2")},
         mar = {possible.values <- c("Married" = "1", "Widowed" = "2", "Divorced" = "3", "Separated" = "4", "Never married" = "5")}
  )
  return(possible.values)
}
