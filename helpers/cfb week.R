if(!require(lubridate)){
  install.packages("lubridate")
}

# System Date to College Football Week Converstion:
returnCfbWeek <- function(date){
  
  # Week Intervals
  week1  <- lubridate::interval(ymd("23-08-26"), ymd("23-09-10"))
  week2  <- lubridate::interval(ymd("23-09-11"), ymd("23-09-17"))
  week3  <- lubridate::interval(ymd("23-08-18"), ymd("23-09-24"))
  week4  <- lubridate::interval(ymd("23-08-25"), ymd("23-10-01"))
  week5  <- lubridate::interval(ymd("23-10-02"), ymd("23-10-08"))
  week6  <- lubridate::interval(ymd("23-10-09"), ymd("23-10-15"))
  week7  <- lubridate::interval(ymd("23-10-16"), ymd("23-10-22"))
  week8  <- lubridate::interval(ymd("23-10-23"), ymd("23-10-29"))
  week9  <- lubridate::interval(ymd("23-10-30"), ymd("23-11-05"))
  week10 <- lubridate::interval(ymd("23-11-06"), ymd("23-11-12"))
  week11 <- lubridate::interval(ymd("23-11-13"), ymd("23-11-19"))
  week12 <- lubridate::interval(ymd("23-11-20"), ymd("23-11-26"))
  week13 <- lubridate::interval(ymd("23-11-20"), ymd("23-11-26"))
  week14 <- lubridate::interval(ymd("23-11-27"), ymd("23-12-03"))
  week15 <- lubridate::interval(ymd("23-12-04"), ymd("23-12-10"))

  # Week 1:
  if(Sys.Date() %within% week1){
    week = 1
  }
  # Week 2:
  if(Sys.Date() %within% week2){
    week = 2
  }
  # Week 3:
  if(Sys.Date() %within% week3){
    week = 3
  }
  # Week 4:
  if(Sys.Date() %within% week4){
    week = 4
  }
  # Week 5:
  if(Sys.Date() %within% week5){
    week = 5
  }
  # Week 6:
  if(Sys.Date() %within% week6){
    week = 6
  }
  # Week 7:
  if(Sys.Date() %within% week7){
    week = 7
  }
  # Week 8:
  if(Sys.Date() %within% week8){
    week = 8
  }
  # Week 9:
  if(Sys.Date() %within% week9){
    week = 9
  }
  # Week 10:
  if(Sys.Date() %within% week10){
    week = 10
  }
  # Week 11:
  if(Sys.Date() %within% week11){
    week = 11
  }
  # Week 12:
  if(Sys.Date() %within% week12){
    week = 12
  }
  # Week 13:
  if(Sys.Date() %within% week13){
    week = 13
  }
  # Week 14:
  if(Sys.Date() %within% week14){
    week = 14
  }
  # Week 15:
  if(Sys.Date() %within% week15){
    week = 15
  }
  if(Sys.Date() > ymd("23-12-10")){
    week = 16
  }
  return(week)
}
