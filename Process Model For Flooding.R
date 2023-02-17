#Process Model for Flooding Event

source("Library_Functions.R")
library("sensitivity")

Iteration_Number = 1000

#Input Collection Data frame for sensitivity
Sens_df = data.frame(matrix(NA, nrow = Iteration_Number,ncol = 9))

Sensitivity_Input_Names<-c("Days",
                           "Days_FEholding",
                           "FE_Cont",
                           "FE_Prev",
                           "Rain_Days",
                           "Sunny_Days",
                           "Soil_Trans",
                           "P_Bug_Plant",
                           "IrW_Sp"
      
                           
                           )
colnames(Sens_df)<-Sensitivity_Input_Names

for (i in 1:Iteration_Number){
  ###Model Inputs
  
  Lot_size = 45 #acres
  lbplot = 38000 
  serving_size = 270
  
  #Growing Season Length:
  Daysmin = 30
  Daysmode = 36
  Daysmax = 42
  Days = F_growing_season_days(min = Daysmin,mode = Daysmode,max = Daysmax ) #Input
  Sens_df$Days[i]<-Days
  
  #Length of Die off after flood
  Days_FEholdingmin = 20
  Days_FEholdingmax = 30
  Days_FEholding = runif(1,Days_FEholdingmin,Days_FEholdingmax) #input
  Sens_df$Days_FEholding[i]<-Days_FEholding
  
  #Flooding Water Contamination
  Cont_FE_Distribution = "Uniform"
  Cont_FE_min = 3
  Cont_FE_max = 4
  FE_Contam = get_value_dist(Distribution = "Uniform", params =c(Cont_FE_min,Cont_FE_max)) #input
  Sens_df$FE_Cont[i]<-FE_Contam
  
  #Prevalence Water Contamination
  Prev_FE_Distribution = "Uniform"
  Prev_FE_min = 0.8
  Prev_FE_max = 1
  FE_Prev = get_value_dist(Distribution = "Uniform", params =c(Prev_FE_min,Prev_FE_max)) #input
  Sens_df$FE_Prev[i]<-FE_Prev
  
  #Rain and Irrigation Splash Inputs
  Rain_Days = round(rpert(1, min = 4,mode = 6, max = 7)) #input
  Sens_df$Rain_Days[i]<-Rain_Days
  
  Sunny_Days =is_sunny(days =Days, raindays =Rain_Days) # input
  Sens_df$Sunny_Days[i]<-Sunny_Days
  
  Soil_Trans <- rbetagen(Days, 0.4, 0.8, 0.05, 16.4) #input
  Sens_df$Soil_Trans[i]<-Soil_Trans
    
  P_Bug_Plant = runif(Days, 0.35, 0.9) #Input
  Sens_df$P_Bug_Plant[i]<-P_Bug_Plant
  
  IrW_Sp <- rpert(Days, 0.02, 0.04, 0.06) #Input
  Sens_df$IrW_Sp[i]<-IrW_Sp
  
  P_Irr_Sp <- rbinom(Days, 1, IrW_Sp)
  
  #Harvesting Blade Cont
  Cont_Soil<-10^rtruncnorm(1,0,3.67,0.928,1.11) #E.coli CFU/g
  Ratio_O157_GE<-10^rtruncnorm(1,-Inf,0,-1.9,0.6)
  Soil_HB<-10.22 #g
  Total_CFU_Blade<-log10(Cont_Soil*Ratio_O157_GE*Soil_HB/1500) #CFU/g
  Tr_blade_lettuce<- 0.0013
  harvest_CC_factor<- runif(1,1,2)
  
  #Transport from farm to facility
  Trans_ff_Temp<-runif(1,18,22)
  Trans_ff_Time<-runif(1,1,2)
  
  #Hand Trimming
  Cont_Gloves = -Inf
  Tr_Gloves_Lettuce = runif(1,0.03,0.3)
  Tr_Lettuce_Gloves = runif(1,0.01,0.03)
  
  #Shredding Trimming
  Cont_Shredd = -Inf 
  Tr_Shredd_Lettuce = runif(1,0.16,0.28)
  Tr_Lettuce_Shredd = runif(1,0.0025,0.0053)
  
  #Conveyor Belt
  Cont_Belt = -Inf 
  Tr_Belt_Lettuce = runif(1,0.15,0.22)
  Tr_Lettuce_Belt = runif(1,0.000,0.0139)
  
  #Shaker Table
  Cont_Shaker = -Inf 
  Tr_Shaker_Lettuce = runif(1,0.06,0.30)
  Tr_Lettuce_Shaker = runif(1,0,0.0038)
  
  #Dewatering Centrifuge
  Cont_Centrifuge = -Inf 
  Tr_Centrifuge_Lettuce = runif(1,0.23,0.31)
  Tr_Lettuce_Centrifuge = runif(1,0,0.0159)
  
  #Transportation from packinghouse to retail
  Time_Packinhouse_Retail = runif(1,1,52) #hrs
  Temp_Packinhouse_Retail = rtriang(1,2,4,8) #°C
  
  #Retail Storage:
  Time_Packinhouse_Retail = runif(1,1,96) #hrs
  Temp_Packinhouse_Retail = rtriang(1,2,4,8) #°C
  
  #Trasnport Retail to Home
  Time_Retail_Home = runif(1,1,2) #hrs
  Temp_Retail_Home = 0.5*(Temp_Packinhouse_Retail+rnormTrunc(1,mean = 8.386,sd = 3.831,min = 0,max = 20)) #C
  
  #Home Storage
  Temp_HomeStorage = rnormTrunc(n=1,mean=3.45,sd=2.44,min=-5, max = 17.22) #°C
  Time_HomeStorage = rweibull(1,1.13,2.84)*24
  
  #Inputs for Sensitivity
  
  
  ###Process Steps
  
  #Step 1: Flooding Event, Contamination of soil
  Outs<-Initial_Cont_function2(Cont =FE_Contam , 
                               Prev= FE_Prev)
  
  Cont_s = Outs[1] 
  Prev_s= Outs[2] 
  
  #Step 2: Soil Die off
  Outs_s<-Infield_dieoff_soil(Cont = Cont_s,Prev = Prev_s,Days =Days_FEholding )
  
  Cont_s = Outs_s[1] 
  Prev_s= Outs_s[2] 
  
  #Step 3: Contamination Event from Soil, rain splash throughout season
  Outs =fc_rsp(contamsoil =Cont_s ,
               prev =Prev_s, 
               soiltrans = Soil_Trans, 
               ec2plant = P_Bug_Plant,
               sunny_yn = Sunny_Days)
  C_rsp =Outs[1] 
  
  Outs =fc_irrsp(contamsoil =Cont_s ,
                 prev =Prev_s, 
                 soiltrans = Soil_Trans, 
                 ec2plant = P_Bug_Plant,
                 irrsp_yn = P_Irr_Sp,
                 sunny_yn = Sunny_Days)
  C_irrsp =Outs[1] 
  
  Cont = log10(10^C_rsp + 10^C_irrsp)
  Prev = Prev_s
  
  
  #Step 4: In field die off lettuce:
  Outs = Infield_dieoff_lettuce(Cont= Cont,Prev = Prev,Days = Days)
  Cont = Outs[1]
  Prev = Outs[2]
  
  #Step 5: Harvesting
  Outs= F_Cross_Cont_Blades_Lettuce(Cont_P =Cont , Prev = Prev,Cont_Env = Total_CFU_Blade, Tr_a = 0, Tr_b= Tr_blade_lettuce, CC_fac = harvest_CC_factor)
  Cont = Outs[1]
  Prev = Outs[2]
  
  #Step 6: Transport from Farm to Facility
  Outs = F_Growth_Total(Cont = Cont, Prev = Prev,Temp = Trans_ff_Temp,Time = Trans_ff_Time)
  Cont = Outs[1]
  Prev = Outs[2]
  
  #Step 7: Facility Processing
  #Hand trimming
  Outs = F_Cross_Cont_Hands_Let(Cont_P =Cont, 
                                Prev=Prev,
                                Cont_Env =Cont_Gloves, 
                                Tr_a = Tr_Gloves_Lettuce, 
                                Tr_b= Tr_Lettuce_Gloves,
                                CC_fac = harvest_CC_factor)
  Cont = Outs[1]
  Prev = Outs[2]
  
  #Shredder
  Outs = F_Cross_Cont_Shredder_Let(Cont_P =Cont, 
                                   Prev=Prev,
                                   Cont_Env =Cont_Shredd, 
                                   Tr_a = Tr_Shredd_Lettuce, 
                                   Tr_b= Tr_Lettuce_Shredd, 
                                   CC_fac = harvest_CC_factor)
  Cont = Outs[1]
  Prev = Outs[2]
  
  #Conveyor Belt
  Outs = F_Cross_Cont_Belt_Let(Cont_P =Cont, 
                               Prev=Prev,
                               Cont_Env =Cont_Belt, 
                               Tr_a = Tr_Belt_Lettuce, 
                               Tr_b= Tr_Lettuce_Belt, 
                               CC_fac = harvest_CC_factor)
  Cont = Outs[1]
  Prev = Outs[2]
  
  
  #Conveyor Shaker
  Outs = F_Cross_Cont_Shaker_Let(Cont_P =Cont, 
                                 Prev=Prev,
                                 Cont_Env =Cont_Shaker, 
                                 Tr_a = Tr_Shaker_Lettuce, 
                                 Tr_b= Tr_Lettuce_Shaker, 
                                 CC_fac = harvest_CC_factor)
  Cont = Outs[1]
  Prev = Outs[2]
  
  
  #Dewatering Centrifuge
  Outs =  F_Cross_Cont_Centrifuge_Let(Cont_P =Cont, 
                                      Prev=Prev,
                                      Cont_Env =Cont_Centrifuge, 
                                      Tr_a = Tr_Centrifuge_Lettuce, 
                                      Tr_b= Tr_Lettuce_Centrifuge, 
                                      CC_fac = harvest_CC_factor)
  Cont = Outs[1]
  Prev = Outs[2]
  
  #Step 8:
  Outs = F_Growth_Total(Cont = Cont, Prev = Prev,Temp = Temp_Packinhouse_Retail,Time = Time_Packinhouse_Retail)
  Cont = Outs[1]
  Prev = Outs[2]
  
  #Retail Storage
  Outs = F_Growth_Total(Cont = Cont, Prev = Prev,Temp = Temp_Packinhouse_Retail,Time = Time_Packinhouse_Retail)
  Cont = Outs[1]
  Prev = Outs[2]
  
  #Home Storage
  Outs = F_Growth_Total(Cont = Cont, Prev = Prev,Temp = Temp_HomeStorage,Time = Time_HomeStorage)
  Cont = Outs[1]
  Prev = Outs[2]
  
  #Dose Response
  Total_Ill= Func_DR_RServing(Cont= Cont, Prev = Prev, Lot_Size =Lot_size , Lot_lb = lbplot, Serving_size = serving_size)
  
  Sens_df$X1[i]<-Total_Ill
}

PRCC<-sensitivity::pcc(Sens_df[,1:9],Sens_df$X1,nboot = 100)
plot(PRCC)
Sensplot<-PRCC$PCC
library(tidyverse)
Sensplot %>% 
  ggplot(aes(x =original, y = row.names(Sensplot)))+
  geom_col()+
  labs(y= "Parameter Name",
       x= "PRCC Index")+
  theme_bw()
