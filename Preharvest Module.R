#Preharvest Logistics

#Selecting Season
IS_Seasons = c("Spring", "Winter", "Summer")
IR_SeasonSelected = sample(IS_Seasons, 1)

##Spring
if (IR_SeasonSelected == "Spring"){
  ##Growing Time in Spring
  IR_Days = round(rpert(1, 32.0, 36.8, 43.0),0)
  #Rainy Days in Spring
  IR_RainyDays = rpert(1, 1, 5.6, 14)
  #Sun hours per day
  IR_SunHoursDay =  rpert(1, 5, 10.4, 12)
  #Allende Die off Coefficient
  IS_Dieoff_Coeff<-(-0.52)
} 

##Winter
if (IR_SeasonSelected == "Winter"){
  ##Growing Time in Winter
  IR_Days = round(rpert(1, 51.0, 56.7, 67.0),0)
  #Rainy Days in Winter
  IR_RainyDays = rpert(1, 3, 10.6, 23)
  #Sun hours per day
  IR_SunHoursDay =  rpert(1, 6, 8.5, 11)
  #Allende Die off Coefficient
  IS_Dieoff_Coeff<-(-0.48)
} 

##Summer
if (IR_SeasonSelected == "Summer"){
  ## Growing Time in Summer
  IR_Days = round(rpert(1, 35, 40, 45),0)
  #Rainy Days in Summer
  IR_RainyDays = rpert(1, 1, 6, 11)
  #Sun hours per day ******MADE UP*******
  IR_SunHoursDay =  rpert(1, 7, 11, 13)
  #Allende Die off Coefficient
  IS_Dieoff_Coeff<-(-0.52)
} 



#Probability of a Sunny Day
IC_Pr_SunnyDay = 1-IR_RainyDays/IR_Days

##E coli levels in soil
IR_ContSoil = rnorm(1,0.549,0.816)
##Probability of irrigation water being contaminated
IR_Pr_EcoliIrrWater = 0.95
## Contamination in irrigation water
IR_ContIrrWater = (10^(rnorm(1,1.270, 0.567)))/100 #CFU/mL
## mL of water per gram of lettuce
IR_TransIrrWater = runif(1, 1.8, 21.6) #mL/g

##Probability of irrigation splashing
IR_Pr_IrrigationSplash<- rpert(1, 0.02, 0.04, 0.06)

##Probability of rain splashing
IR_Pr_RainSplash<- rpert(1, 0.02, 0.04, 0.06)

#Soil transferred by irrigation or by rain splash
IR_TransSoil = rbetagen(1, 0.4, 0.8, 0.05, 16.4) #g soil/g lettuce

#Probability of bacteria transferred from soil to plant
IR_Pr_EcoliSoiltoPlant = runif(1, 0.35, 0.9)

#Sampling Inputs

IS_PHSampling_Mass = 1500
IS_PHSampling_N = 60


#Dieoff_Preharvest- Bezanson et al, 2012 (Pang et al, 2017)
F_DieoffProduce_Pang2017 <-function(ContIn){
  if (ContIn == 0){
    ContRed = 0
  } else{
    Red = -(1/(2.45/24))^0.3
    ContRed = log10(ContIn) + Red
  }
  return(10^ContRed)
}

#Dieoff_Preharvest- Ottoson et al, 2011  (Allende at al, 2018)
F_DieoffProduce_Allende2018 <-function(ContIn){
  if (ContIn == 0){
    ContRed = 0
  } else{
    Red = IS_Dieoff_Coeff*IR_SunHoursDay/24
    ContRed = log10(ContIn) + Red
  }
  return(10^ContRed)
}

F_Sampling<-function(ContIn, Prev = 1){
  Grab_Weight = IS_PHSampling_Mass/IS_PHSampling_N
  P_Detect = 1-exp(-Grab_Weight*ContIn)
  V_Detection = rbinom(n = 60,size = 1,prob = P_Detect)
  if (Prev == 1){
    if(sum(V_Detection)>1){
      return (1)
    } else {
      return (0)
    }
  } else if(Prev < 1){
    V_Sampling = rbinom(n = 60,size = 1,prob = Prev)
    V_Total = V_Sampling*V_Detection
    if(sum(V_Total)>1){
      return (1)
    } else {
      return (0)
    }
  }
}


Sampling_Days = c(1, IR_Days%/%2, IR_Days-2)

Contamination = 0 #CFU/g
Sampling_Results<-c()
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
  if(i %in% Sampling_Days){
    Sampling_Results[length(Sampling_Results)+1]<-F_Sampling(ContIn = Contamination)
  }
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
  Contamination  = Contamination  + Total_Increase #CFU/g
  if (i>1 | i<IR_Days){
    Contamination = F_DieoffProduce_Allende2018 (ContIn =Contamination ) #CFU/g
  }
}
print(sum(Sampling_Results)/length(Sampling_Results))




