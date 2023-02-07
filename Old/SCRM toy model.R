
library(Rlab)
#library(extraDist)
library(truncnorm)
# Dose Response Function
library(reshape2)
library(tidyverse)
library(scales)

Func_DR_RServing<-function(dose){
  alpha<-0.0571 
  beta<-2.2183
  1-((gamma(alpha+beta)*gamma(beta+dose))/(gamma(beta)*gamma(alpha+beta+dose)))
}

Growth_Function<-function(Cont,growth){
  Cont_out = Cont+growth
  #Cont: Should be in log scale (log CFUs)
  #growth: is the expected growth in log CFUs.
}

Inactivation_Function<-function(Cont,Prev,logred){
  #Cont: Should be in log scale (log CFUs)
  #Prev: should be a number between 0 and 1
  #log red: is the expected log reduction.
  Cont_out = Cont-logred
  print(Cont_out)
  Prev_out = Prev*(1-(1-10^Cont_out/10^Cont)^(10^Cont))
  return(c(Cont_out, Prev_out))
}


Sample_func_Internal<-function(x, pdetect){
  #x is a vector of grabs
  result = ifelse(x == 1, rbern(n=1,prob=pdetect), 0)
  return(result)
}


Sampling_Function<-function(Cont,Prev, Ngrab, GrabWeight){
  Cont = 10^Cont #transforming from log CFU/g to CFU/g
  #Cont in concentration of CFU per gram
  #Grab weight should be in grams
  Pdetect = 1-exp(-Cont*GrabWeight) #probability of detecting based on cont levels
  #print(Pdetect)
  srs_hits = rbern(n=Ngrab,prob=Prev) #if cont randomly dist, hit based on prevalence
  #print(srs_hits)
  Outcomes = sapply(X= srs_hits , FUN = Sample_func_Internal, pdetect = Pdetect ) #was contamination detected for each grab?
  print(Outcomes)
  if (sum(Outcomes)>0){ #if any of the grabs comes back positive then detection happens. 
    #return (1)
    Prev = Prev*(0/(1-Prev+(Prev*0)))
  }
  return (Prev)
}


Cross_Cont_Function<-function(Cont_P, Cont_Env, Tr_a, Tr_b){
  #Cont P is the total number of cells in P
  #Cont Env is the total number of cells in the environment
  #Tr_a is the transfer coefficient from prod to env
  #Tr_b is the transfer coefficient from env to prod.
  Cont_P = 10^Cont_P # Converting to cells
  Cont_Env = 10^Cont_Env #Converting to cells
  
  N_prod <- ((1-Tr_a)*Cont_P) + (Tr_b*Cont_Env)
  print(N_prod)
  N_env <- (Tr_a*Cont_P ) + ((1-Tr_b)*Cont_Env)
  return(c(log10(N_prod),log10(N_env)))
}

Ill_List = c()

df_tracking = data.frame(matrix(ncol = 10, nrow = 0))
df_tracking_prev = data.frame(matrix(ncol = 10, nrow = 0))

for (i in 1:10000){
  ####Inputs####
  #Flood
  Flood_Cont = 1*10^8 #Total CFU/g
  Total_Prevalence = 1 #prevalence
  
  #Dieoff_flood
  Dieoff_soil_mean = 0.55 #log CFU/day
  Dieoff_soil_sd = 0.21 #log CFU/day
  Days_flood_planting = 20 #days between flood and planting
  
  #soil sampling
  Days_after_flood = 3 #Sampling of soil occurs X many days after flood
  soil_sampling = 1 #is soil sampling on? #INTERVENTION
  Ngrabs_soil= 60 #total grabs
  Nmass_soil = 25 #g
  
  #Soil to plant
  Tr_Soil_Lettuce = 0.17 #transfer between soil and produce. 
  
  #Produce Die off
  Total_preharvest_time = runif(1,30,38)
  Dieoff_produce_mean = 0.2 #log CFU/day
  Dieoff_produce_sd = 0.07 #log CFU/day
  
  #Product Sampling
  Days_PH = 24
  Days_after_planting = 20
  PH_Sampling = 0
  Ngrabs_produce = 60
  Nmass_produce = 25
  
  #Processing
  Produce_Wash = 0
  Produce_wash_min = 0.6
  Produce_wash_mode = 1.1
  Produce_wash_max = 1.4
  Wash_red = runif(1,Produce_wash_min,Produce_wash_max)
  
  #Hand Trimming
  Cont_Hands = -Inf 
  Tr_Gloves_Lettuce = runif(1,0.03,0.3)
  Tr_Lettuce_Gloves = runif(1,0.01,0.03)
  
  #Shredding Trimming
  Cont_Hands = -Inf 
  Tr_Shredd_Lettuce = runif(1,0.16,0.28)
  Tr_Lettuce_Shredd = runif(1,0.0025,0.0053)
  
  
  #Serving Size
  Serving_size_g = 85 #g
  
  #risk Characterization
  US_pop = 316085800
  Anual_con = 5888
  Ser_person = Anual_con/Serving_size
  Ser_year = US_pop*Ser_person
  
  Lb_Acre = 25000 #lb. 
  Serving_size_lb = Serving_size_g/454 #lb
  Total_servings_Acre = as.integer(25000/Serving_size_lb)
  
  
  #########PROCESS MODEL START #########
  
  Prevalence_tracker<-c()
  Log_Cont_tracker<-c()
  
  rtruncnorm(1, b= 0, mean = 0.55, sd= 0.22)
  
  
  #### Flooding ###
  #Initial contamination event in soil 
  log_cont_soil =log10(Flood_Cont)#contamination in log scale log CFU/g
  #Recording Metrics #1
  Prevalence_tracker<-c(Prevalence_tracker, Total_Prevalence)
  Log_Cont_tracker<-c(Log_Cont_tracker, log_cont_soil)
  
  #### Inactivation Flooding-Sampling ####
  #total soil die off day 
  total_soil_dieoff_day = rtruncnorm(1, a=0, mean = Dieoff_soil_mean, sd= Dieoff_soil_sd)
  #die off cont to sampling
  total_soil_dieoff = total_soil_dieoff_day*Days_after_flood
  #Inactivation function
  Outs_Inac =Inactivation_Function(Cont =log_cont_soil, Prev = Total_Prevalence, logred = total_soil_dieoff )
  #Contamination after soil die off to sampling
  log_cont_soil = Outs_Inac[1] #log CFU/g
  #prevalence in soil after die off
  Total_Prevalence = Outs_Inac[2] 
  #Recording Metrics #2
  Prevalence_tracker<-c(Prevalence_tracker, Total_Prevalence)
  Log_Cont_tracker<-c(Log_Cont_tracker, log_cont_soil)
  
  #### Sampling ####
  #sampling, something happens here is detected.....
  
  #### Inactivation Flooding-Planting ####
  #die off  sampling to planting
  total_soil_dieoff = total_soil_dieoff_day*(Days_flood_planting-Days_after_flood)
  #Contamination after soil die off to sampling
  Outs_Inac =Inactivation_Function(Cont =log_cont_soil, Prev = Total_Prevalence, logred = total_soil_dieoff)
  #Contamination after soil die off to sampling
  log_cont_soil = Outs_Inac[1] #log CFU/g
  #prevalence in soil after die off
  Total_Prevalence = Outs_Inac[2] # %units (g) with more than 1 CFU
  #Recording Metrics #3
  Prevalence_tracker<-c(Prevalence_tracker, Total_Prevalence)
  Log_Cont_tracker<-c(Log_Cont_tracker, log_cont_soil)
  
  #### Transfer from soil to plant ####
  #Assume prevalence stays the same.
  #Cross Contamination soil to plant
  CC_Outs = Cross_Cont_Function(Cont_P = -Inf,Cont_Env =log_cont_soil ,Tr_a = 0,Tr_b = 0.17)
  log_cont_prod = CC_Outs[1]
  #Recording Metrics #4
  Prevalence_tracker<-c(Prevalence_tracker, Total_Prevalence)
  Log_Cont_tracker<-c(Log_Cont_tracker,log_cont_prod)
  
  #### Pre-harvest inactivation - PH Sampling  ####
  #total soil die off day 
  total_produce_dieoff_day = (rtruncnorm(1,a=0,mean =Dieoff_produce_mean, sd= Dieoff_produce_sd))
  #die off cont to sampling
  total_produce_dieoff = total_produce_dieoff_day*Days_after_flood
  #Inactivation function
  Outs_Inac =Inactivation_Function(Cont =log_cont_prod, Prev = Total_Prevalence, logred = total_produce_dieoff)
  #Contamination after soil die off to sampling
  log_cont_prod = Outs_Inac[1] #log CFU/g
  #prevalence in soil after die off
  Total_Prevalence = Outs_Inac[2]
  #Recording Metrics #5
  Prevalence_tracker<-c(Prevalence_tracker, Total_Prevalence)
  Log_Cont_tracker<-c(Log_Cont_tracker, log_cont_prod)
  
  #####Product Sampling####
  if (PH_Sampling ==1){
    Total_Prevalence= Sampling_Function(Cont = log_cont_prod, Prev = Total_Prevalence, Ngrab = Ngrabs_produce, GrabWeight =Nmass_produce)
  }
  
  #Recording Metrics #6
  Prevalence_tracker<-c(Prevalence_tracker, Total_Prevalence)
  Log_Cont_tracker<-c(Log_Cont_tracker, log_cont_prod)
  
  
  #### PH Sampling - Harvest  ####
  #die off  sampling to planting
  total_produce_dieoff  = total_produce_dieoff_day*(Days_PH-Days_after_planting)
  #Contamination after soil die off to sampling
  Outs_Inac =Inactivation_Function(Cont =log_cont_prod, Prev = Total_Prevalence, logred = total_produce_dieoff)
  #Contamination after soil die off to sampling
  log_cont_prod = Outs_Inac[1] #log CFU/g
  #prevalence in soil after die off
  Total_Prevalence = Outs_Inac[2] # %units (g) with more than 1 CFU
  #Recording Metrics #7
  Prevalence_tracker<-c(Prevalence_tracker, Total_Prevalence)
  Log_Cont_tracker<-c(Log_Cont_tracker, log_cont_prod)
  
  ####Processing####
  
  ##manual Trimming
  CC_Outs = Cross_Cont_Function(Cont_P = log_cont_prod,Cont_Env =Cont_Hands ,Tr_a = Tr_Lettuce_Gloves,Tr_b = Tr_Gloves_Lettuce)
  log_cont_prod = CC_Outs[1]
  #Recording Metrics #8
  Prevalence_tracker<-c(Prevalence_tracker, Total_Prevalence)
  Log_Cont_tracker<-c(Log_Cont_tracker, log_cont_prod)
  
  #Wash
  if (Produce_Wash == 1){
    Outs_Inac =Inactivation_Function(Cont =log_cont_prod, Prev = Total_Prevalence, logred = Wash_red)
    #Contamination after soil die off to sampling
    log_cont_prod = Outs_Inac[1] #log CFU/g
    #prevalence in soil after die off
    Total_Prevalence = Outs_Inac[2]
  }
  #Recording Metrics #9
  Prevalence_tracker<-c(Prevalence_tracker, Total_Prevalence)
  Log_Cont_tracker<-c(Log_Cont_tracker, log_cont_prod)
  
  #Shredding
  CC_Outs = Cross_Cont_Function(Cont_P = log_cont_prod,Cont_Env =Cont_Hands ,Tr_a = Tr_Lettuce_Shredd,Tr_b = Tr_Shredd_Lettuce)
  log_cont_prod = CC_Outs[1]
  #Recording Metrics #10
  Prevalence_tracker<-c(Prevalence_tracker, Total_Prevalence)
  Log_Cont_tracker<-c(Log_Cont_tracker, log_cont_prod)
  
  
  ####Retial Storage ####
  
  
  #Serving Size
  
  
  
  #Dose Response.
  #Contamination Dose
  Con_Dose = 10^log_cont_prod*Serving_size_g
  
  #Probability of Illness based on Dose
  P_Ill =Func_DR_RServing(dose = Con_Dose)
  
  #Total Ill per Acre
  #Total_Ill_P = Total_servings_Acre*(P_Ill*Total_Prevalence)
  #Total_Ill_R = Total_Ill_P/
  Total_Ill_R=as.integer(rbinom(1,size = Total_servings_Acre,prob =Total_Prevalence*P_Ill)/26.1)
  Ill_List = c(Ill_List,Total_Ill_R)
  df_tracking<-rbind(df_tracking,Log_Cont_tracker)
  df_tracking_prev<-rbind(df_tracking_prev,Prevalence_tracker)
}

hist(Ill_List)
mean(Ill_List, na.rm =T )

colnames(df_tracking)<-seq(1:10)
df_tracking_melted<- melt(df_tracking)
df_tracking_melted$variable<-as.factor(df_tracking_melted$variable)

df_tracking_melted %>% 
  group_by(variable) %>% 
  summarise(mean= mean(value), q95 = quantile(value, 0.95),q05 = quantile(value, 0.05)) %>% 
  ggplot(aes(x= variable, y = mean,ymin = q05, ymax = q95))+
  geom_ribbon(aes(group=1), alpha = 0.2)+
  geom_line(aes(group=1))+
  geom_point()+
  labs(y= "Contamination (log CFU/g)", x= "Process stage")

colnames(df_tracking_prev)<-seq(1:10)
df_tracking_Prev_melted<- melt(df_tracking_prev)
df_tracking_Prev_melted$variable<-as.factor(df_tracking_Prev_melted$variable)

df_tracking_Prev_melted %>% 
  group_by(variable) %>% 
  summarise(mean= mean(value,na.rm=T), q95 = quantile(value, 0.95,na.rm=T),q05 = quantile(value, 0.05, na.rm=T)) %>% 
  ggplot(aes(x= variable, y = mean,ymin = q05, ymax = q95))+
  geom_ribbon(aes(group=1), alpha = 0.2)+
  geom_line(aes(group=1))+
  geom_point()+
  labs(y= "Contamination (log CFU/g)", x= "Process stage")  


#Baseline
Ill_List_Baseline<-Ill_List
df_tracking_Prev_melted_Baseline<-df_tracking_Prev_melted
df_tracking_melted_Baseline<-df_tracking_melted

#Washing
Ill_List_Washing<-Ill_List
df_tracking_Prev_melted_Washing<-df_tracking_Prev_melted
df_tracking_melted_Washing<-df_tracking_melted

#Sampling
Ill_List_Samp_1<-Ill_List
df_tracking_Prev_melted_Samp_1<-df_tracking_Prev_melted
df_tracking_melted_Samp_1<-df_tracking_melted



df_illness = data.frame("Baseline" =Ill_List_Baseline,
                        "Washing" = Ill_List_Washing,
                        "Sampling" =Ill_List_Samp_1 )\



df_illness_melted<-melt(df_illness)

df_illness_melted %>% 
  group_by(variable) %>% 
  summarise(mean=mean(value, na.rm=T),q95 = quantile(value, 0.95,na.rm=T),q05 = quantile(value, 0.05, na.rm=T))

df_illness_melted %>% 
  ggplot(aes(x= value, fill = variable))+
  geom_histogram(bins = 50)+
  labs(x= "Total Illness per Acre")+
  scale_y_continuous(trans = log10_trans())


###
df_tracking_melted_Baseline$type<-"Baseline"
df_tracking_melted_Washing$type<-"Washing"
df_tracking_melted_Samp_1$type<-"Sampling"

scenario_melted<-rbind(df_tracking_melted_Baseline,df_tracking_melted_Washing,df_tracking_melted_Samp_1)

scenario_melted %>% 
  group_by(variable, type) %>% 
  summarise(mean= mean(value), q95 = quantile(value, 0.95),q05 = quantile(value, 0.05)) %>% 
  ggplot(aes(x= variable, y = mean,ymin = q05, ymax = q95))+
  geom_ribbon(aes(group=type,fill= type), alpha = 0.2)+
  geom_line(aes(group=type, color= type))+
  geom_point(aes(color= type))+
  labs(y= "Contamination (log CFU/g)", x= "Process stage")

df_tracking_Prev_melted_Baseline$type<-"Baseline"
df_tracking_Prev_melted_Washing$type<-"Washing"
df_tracking_Prev_melted_Samp_1$type<-"Sampling"

scenario_melted_Prev<-rbind(df_tracking_Prev_melted_Baseline,df_tracking_Prev_melted_Washing,df_tracking_Prev_melted_Samp_1)


scenario_melted_Prev %>% 
  group_by(variable, type) %>% 
  summarise(mean= mean(value,na.rm=T), q95 = quantile(value, 0.95,na.rm=T),q05 = quantile(value, 0.05, na.rm=T)) %>% 
  ggplot(aes(x= variable, y = mean,ymin = q05, ymax = q95))+
  geom_ribbon(aes(group=type,fill= type), alpha = 0.2)+
  geom_line(aes(group=type, color= type))+
  geom_point(aes(color= type))+
  labs(y= "Prevalence", x= "Process stage")
