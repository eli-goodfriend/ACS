group_degree <- function(degree){
  library("car")

  # group all degrees into
  # Math and science, engineering, humanities, social sciences, 
  # industry specific, and other

  degree <- recode(degree,"1100:1199='Industry specific'")
  degree <- recode(degree,"1300:1399='Industry specific'")
  degree <- recode(degree,"1400:1499='Industry specific'")
  degree <- recode(degree,"1500:1599='Humanities'")
  degree <- recode(degree,"1900:1999='Industry specific'")
  degree <- recode(degree,"2000:2199='Math and science'")
  degree <- recode(degree,"2200:2299='Industry specific'")
  degree <- recode(degree,"2300:2399='Social sciences'")
  degree <- recode(degree,"2400:2599='Engineering'")
  degree <- recode(degree,"2600:2699='Humanities'")
  degree <- recode(degree,"2900:2999='Industry specific'")
  degree <- recode(degree,"3200:3299='Industry specific'")
  degree <- recode(degree,"3300:3499='Humanities'")
  degree <- recode(degree,"3500:3599='Social sciences'")
  degree <- recode(degree,"3600:3699='Math and science'")
  degree <- recode(degree,"3700:3799='Math and science'")
  degree <- recode(degree,"3800:3899='Industry specific'")
  degree <- recode(degree,"4000:4001='Humanities'")
  degree <- recode(degree,"4002:4005='Math and science'")
  degree <- recode(degree,"4006:4007='Social sciences'")
  degree <- recode(degree,"4100:4199='Industry specific'")
  degree <- recode(degree,"4800:4899='Humanities'")
  degree <- recode(degree,"4900:4999='Industry specific'")
  degree <- recode(degree,"5000:5199='Math and science'")
  degree <- recode(degree,"5200:5299='Social sciences'")
  degree <- recode(degree,"5300:5499='Industry specific'")
  degree <- recode(degree,"5500:5599='Social sciences'")
  degree <- recode(degree,"5600:5999='Industry specific'")
  degree <- recode(degree,"6000:6099='Humanities'")
  degree <- recode(degree,"6100:6299='Industry specific'")
  degree <- recode(degree,"6402:6403='Social sciences'")

  degree <- ordered(degree,
                    levels=c("Engineering","Math and science","Industry specific","Social sciences","Humanities"))
  return(degree)
}
