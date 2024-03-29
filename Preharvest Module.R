

###FUNCTIONS ####

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

##INPUTS ###

#Vector for Inputs
## General

IR_Days_V<- c()
IR_RainyDays_V<-c()
IR_SunHoursDay_V<-c()
IS_Dieoff_Coeff_V<-c()
## Water Quality
IR_Pr_EcoliIrrWater_V<-c()
IR_ContIrrWater_V<-c()
## Soil Quality
IR_ContSoil_V<-c()
## Irrigation
IR_TransIrrWater_V<-c()
IR_Pr_IrrigationSplash_v<-c()
## Rain
IR_Pr_RainSplash_V<-c()
## Rain and Irrigation
IR_TransSoil_V<-c()
IR_Pr_EcoliSoiltoPlant_V<-c()

#Output
Contamination_V<-c()

for (j in 1:10000){
  
  #Selecting Season
  IS_Seasons = c("Spring", "Winter", "Summer")
  IR_SeasonSelected = sample(IS_Seasons, 1)
  
  ####General Season Inputs
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
  
  ##Probability of a Sunny Day
  IC_Pr_SunnyDay = 1-IR_RainyDays/IR_Days
  
  ### Water Quality Inputs
  
  ##Probability of irrigation water being contaminated
  IR_Pr_EcoliIrrWater = 0.95
  ## Contamination in irrigation water
  IR_ContIrrWater = (10^(rnorm(1,1.270, 0.567)))/100 #CFU/mL
  
  ### Soil Quality Inputs
  
  ##E coli levels in soil
  IR_ContSoil = rnorm(1,0.549,0.816)
  
  ### Irrigation Inputs
  
  ## mL of water per gram of lettuce
  IR_TransIrrWater = runif(1, 1.8, 21.6) #mL/g
  ##Probability of irrigation splashing
  IR_Pr_IrrigationSplash<- rpert(1, 0.02, 0.04, 0.06)
  
  ### Rain Inputs
  
  ##Probability of rain splashing
  IR_Pr_RainSplash<- rpert(1, 0.02, 0.04, 0.06)
  
  ### Rain and Irrigation Splashing
  
  #Soil transferred by irrigation or by rain splash
  IR_TransSoil = rbetagen(1, 0.4, 0.8, 0.05, 16.4) #g soil/g lettuce
  #Probability of bacteria transferred from soil to plant
  IR_Pr_EcoliSoiltoPlant = runif(1, 0.35, 0.9)
  
  ### Intervention INPUTS ####
  
  
  ### Sampling Inputs
  
  IS_PHSampling_Mass = 1500
  IS_PHSampling_N = 60
  
  Sampling_Days = c(1, IR_Days%/%2, IR_Days-2)
  
  Contamination = 0 #CFU/g
  
  ### Irrigation Holding ##
  IC_Irrigation_Holding  = 1
  IR_Irrigation_Holding_days<-round(runif(1,2,8),0)
  
  
  ### Process Model ###
  
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
  
  #Vector for Inputs
  ## General
  IR_Days_V[j]<-IR_Days
  IR_RainyDays_V[j]<-IR_RainyDays
  IR_SunHoursDay_V[j]<-IR_SunHoursDay
  IS_Dieoff_Coeff_V[j]<-IS_Dieoff_Coeff
  ## Water Qual
  IR_Pr_EcoliIrrWater_V[j]<-IR_Pr_EcoliIrrWater
  IR_ContIrrWater_V[j]<-IR_ContIrrWater
  ## Soil Qual
  IR_ContSoil_V[j]<-IR_ContSoil
  ## Irrigation
  IR_TransIrrWater_V[j]<-IR_TransIrrWater
  IR_Pr_IrrigationSplash_v[j]<-IR_Pr_IrrigationSplash
  ## Rain
  IR_Pr_RainSplash_V[j]<-IR_Pr_RainSplash
  ## Rain and Irrigation
  IR_TransSoil_V[j]<-IR_TransSoil
  IR_Pr_EcoliSoiltoPlant_V[j]<-IR_Pr_EcoliSoiltoPlant
  
  #Output
  Contamination_V[j]<-Contamination
  
  #Detection Rate for Sampling
  Sampling_Detection_Rate = (sum(Sampling_Results)/length(Sampling_Results))
}





### Sensitivity Analysis
Output_df<-data.frame(IR_Days_V,IR_RainyDays_V,IR_SunHoursDay_V,
                     IS_Dieoff_Coeff_V,IR_Pr_EcoliIrrWater_V,
                     IR_ContIrrWater_V,IR_ContSoil_V,IR_TransIrrWater_V,
                     IR_Pr_IrrigationSplash_v,IR_Pr_RainSplash_V,
                     IR_TransSoil_V,IR_Pr_EcoliSoiltoPlant_V,
                     Contamination_V)

library(sensitivity)
library(ggplot2)

Pcc<-pcc(Output_df[,1:12], Output_df$Contamination_V, nboot = 100, conf = T)

Pcc_df<-Pcc$PCC

Pcc_df$Names<-rownames(Pcc_df)

Pcc_df %>% 
  ggplot(aes(x=fct_reorder(Names, abs(original)), y = original))+
  labs(y = "Partial Rank Correlation Coefficient", x = "Input Name", title = "Sensitivity: Final Contamination Levels")+
  coord_flip()+
  geom_col(fill= "skyblue", color= "black")+
  theme_bw()

  