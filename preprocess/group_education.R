group_education <- function(education){
  library("car")
  
  # 1 - 15 is no high school diploma
  education <- recode(education,"seq(1,15)='No diploma or GED'")

  # group together GED and diploma
  education <- recode(education,"16:17='High school'")

  # group together all partial college
  education <- recode(education,"18:19='Some college'")
  
  education <- recode(education,"20='Associates'")
  education <- recode(education,"21='Batchelors'")
  education <- recode(education,"22='Masters'")
  education <- recode(education,"23='Professional'")
  education <- recode(education,"24='Doctoral'")

  education <- ordered(education,
                      levels=c("No diploma or GED","High school","Some college","Associates","Batchelors","Masters","Professional","Doctoral"))
  return(education)
}