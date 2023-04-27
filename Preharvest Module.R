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
  IR_SunHoursDay =  rpert(1, 5, 10.4, 12)
} 

##Winter
if (IR_SeasonSelected == "Winter"){
  ##Growing Time in Winter
  IR_Days = rpert(1, 51.0, 56.7, 67.0)
  #Rainy Days in Winter
  IR_RainyDays = rpert(1, 3, 10.6, 23)
  #Sun hours per day
  IR_SunHoursDay =  rpert(1, 6, 8.5, 11)
} 

##Summer
if (IR_SeasonSelected == "Summer"){
  ## Growing Time in Summer
  IR_Days = rpert(1, 35, 40, 45)
  #Rainy Days in Summer
  IR_RainyDays = rpert(1, 1, 6, 11)
  #Sun hours per day ******MADE UP*******
  IR_SunHoursDay =  rpert(1, 7, 11, 13)
} 




#Probability of a Sunny Day
IC_Pr_SunnyDay = 1-IR_RainyDays/IR_Days

##Ecoli levels in soil
IR_ContSoil = rnorm(1,0.549,0.816)
##Probability of irrigation water being contaminated
IR_Pr_EcoliIrrWater = 0.95
## Contamination in irrigation water
IR_ContIrrWater = (10^(rnorm(1,1.270, 0.567)))/100 #CFU/mL
## mL of water per gram of lettuce
IR_TransIrrWater = runif(1, 1.8, 21.6) #mL/g

##Porbability of irrigation splashing
IR_Pr_IrrigationSplash<- rpert(1, 0.02, 0.04, 0.06)

##Probability of rain splashing
IR_Pr_RainSplash<- rpert(1, 0.02, 0.04, 0.06)

#Soil transfered by irrigation or by rain splash
IR_TransSoil = rbetagen(1, 0.4, 0.8, 0.05, 16.4) #g soil/g lettuce

#Proibability of bacteria tranfered from soil to plant
IR_Pr_EcoliSoiltoPlant = runif(1, 0.35, 0.9)


#Dieoff_Preharvest
F_DieoffProduce <-function(ContIn){
  if (ContIn == 0){
    ContRed = 0
  } else{
    Red = -(1/(2.45/24))^0.3
    ContRed = log10(ContIn) + Red
  }
  return(10^ContRed)
}




Initial = 0 #CFU/g
for (i in 1:IR_Days){
  #Decisions
  ## Is it sunny
  IC_Is_Sunny = rbinom(1, 1, IC_Pr_SunnyDay)
  ## Is the irrigation water contamination
  IC_Is_IrrWaterCont = rbinom(1, 1,IR_Pr_EcoliIrrWater)
  ## Does irrigation splash occur?
  IC_Is_IrrigationSplash = rbinom(1, 1,IR_Pr_IrrigationSplash)
  ## Does Rain splash occur?
  IC_Is_RainSplash = rbinom(1, 1,IR_Pr_RainSplash)
  
  if(IC_Is_Sunny == 1){
    #Increase due to Irrigation water
    In_Irr_Water = (IR_ContIrrWater* IR_TransIrrWater*IC_Is_IrrWaterCont)
    #Increase due to Irrigation water splash
    In_Irr_Splash = ((10^IR_ContSoil)*IR_TransSoil*IR_Pr_EcoliSoiltoPlant*IC_Is_IrrigationSplash)
    In_Rain_Splash = 0
  } else {
    In_Irr_Water =0
    In_Irr_Splash = 0 
    #Increase due to rain splash
    In_Rain_Splash = round (10^(IR_ContSoil)*IR_TransSoil*IR_Pr_EcoliSoiltoPlant*IC_Is_RainSplash)
  }
  Total_Increase = In_Irr_Water + In_Irr_Splash + In_Rain_Splash
  Initial = Initial + Total_Increase #CFU/g
  if (i>1 | i<IR_Days){
    Initial= F_DieoffProduce (ContIn =Initial) #CFU/g
  }
  #print(Initial)
}







##Contamination Level in Irrigation Water
IR_WaterPerGramLett = rtruncnorm(1,a = 0, b = Inf, mean = 0.108, sd = 0.019) #ml of water per g of lettuce 
IR_ContIrrWater = runif(1,1,235) #CFU/100 ml
IR_RatioEC = 10^(rtruncnorm(1,a = -Inf, b = 0, mean = 1.9, sd = 0.6)) #Ratio of O157:H7 to E.coli
IC_ConcentrationIrr = (IR_ContIrrWater/100) * IR_RatioEC * IR_WaterPerGramLett #CFU/g





##Allende Contamination Event
IR_EcoliContSoil = rnorm(1,0.549,0.816) #log CFU/g
IR_EcoliSoilPlant = runif(IR_Days, 0.35,0.9) #Percentage



