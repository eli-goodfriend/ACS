group_education <- function(education){
  library("car")
  education <- factor(education, ordered=TRUE)
  
  # 1 - 15 is no high school diploma
  # TODO make this a loop
  education <- recode(education,"'1'='15'")
  education <- recode(education,"'2'='15'")
  education <- recode(education,"'3'='15'")
  education <- recode(education,"'4'='15'")
  education <- recode(education,"'5'='15'")
  education <- recode(education,"'6'='15'")
  education <- recode(education,"'7'='15'")
  education <- recode(education,"'8'='15'")
  education <- recode(education,"'9'='15'")
  education <- recode(education,"'10'='15'")
  education <- recode(education,"'11'='15'")
  education <- recode(education,"'12'='15'")
  education <- recode(education,"'13'='15'")
  education <- recode(education,"'14'='15'")
  
  # group together GED and diploma
  education <- recode(education,"'16'='17'")

  # group together all partial college
  education <- recode(education,"'18'='19'")

  education <- factor(education)
  return(education)
}