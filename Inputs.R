
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
  #Sun hours per day
  IR_SunHoursDay =  rpert(IR_Days, 5, 10.4, 12)
} 

##Winter
if (IR_SeasonSelected == "Winter"){
  ##Growing Time in Winter
  IR_Days = rpert(1, 51.0, 56.7, 67.0)
  #Rainy Days in Winter
  IR_RainyDays = rpert(1, 3, 10.6, 23)
  #Sun hours per day
  IR_SunHoursDay =  rpert(IR_Days, 6, 8.5, 11)
} 

##Summer
if (IR_SeasonSelected == "Summer"){
  ## Growing Time in Summer
  IR_Days = rpert(1, 35, 40, 45)
  #Rainy Days in Summer
  IR_RainyDays = rpert(1, 1, 6, 11)
} 

#Probability of a Sunny Day
IC_PrSunnyDay = 1-IR_RainyDays/IR_Days
IC_IsSunny = rbinom(IR_Days, 1, IC_PrSunnyDay)


#Contamination

##Contamination Level in Irrigation Water
IR_WaterPerGramLett = rtruncnorm(1,a = 0, b = Inf, mean = 0.108, sd = 0.019) #ml of water per g of lettuce 
IR_ContIrrWater = runif(1,1,235) #CFU/100 ml
IR_RatioEC = 10^(rtruncnorm(1,a = -Inf, b = 0, mean = 1.9, sd = 0.6)) #Ratio of O157:H7 to E.coli
IC_ConcentrationIrr = (IR_ContIrrWater/100) * IR_RatioEC * IR_WaterPerGramLett #CFU/g

##Allende Contamination Event
IR_EcoliContSoil = rnorm(1,0.549,0.816) #log CFU/g
IR_EcoliSoilPlant = runif(IR_Days, 0.35,0.9) #Percentage



