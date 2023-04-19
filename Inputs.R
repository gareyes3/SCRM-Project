#Inputs:

#Preharvest Logistics

#Selecting Season
IS_Seasons = c("Spring", "Winter", "Summer")
IR_SeasonSelected = sample(IS_Seasons, 1)

##Spring
if (IR_SeasonSelected == "Spring"){
  ##Growing Time in Spring
  IR_Days = rpert(1, 32.0, 36.8, 43.0)
  #Rainy Days in Spring
  IR_RainyDays = rpert(1, 1, 5.6, 14)
} 

##Winter
if (IR_SeasonSelected == "Winter"){
  ##Growing Time in Winter
  IR_Days = rpert(1, 51.0, 56.7, 67.0)
  #Rainy Days in Winter
  IR_RainyDays = rpert(1, 3, 10.6, 23)
} 

##Summer
if (IR_SeasonSelected == "Summer"){
  ## Growing Time in Summer
  IR_Days = rpert(1, 35, 40, 45)
  #Rainy Days in Summer
  IR_RainyDays = rpert(1, 1, 6, 11)
} 

#Probability of a Sunny Day
IC_PrSunnyDay = 1-IR_RainyDay/IR_Days

