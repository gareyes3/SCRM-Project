
library(Rlab)
library(extraDist)

# Dose Response Function

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
  Prev_out = Prev*(1-(1-10^Cont_out/10^Cont)^(10^Cont))
  return(c(Cont_out, Prev_out))
}



####Inputs####
#Flood
Flood_Cont = 1*10^8 #Total CFU/g
Total_Prevalence = 1 #prevalence
#Dieoff_flood
Dieoff_soil_mean = 0.55 #log CFU/day
Dieoff_soil_sd = 0.21 #log CFU/day
Days_flood_planting = 20 #days between flood and planting
#soil sampling
Days_after_flood = 3
soil_sampling = 1
Ngrabs= 60 #total grabs
Nmass = 25 #g
#Soil to plant
Tr_Soil_Lettuce = 0.17

#Produce Die off
Total_preharvest_time = runif(1,30,38)
Dieoff_produce_mean = 0.2 #log CFU/day
Dieoff_produce_sd = 0.07 #log CFU/day

#### Flooding ###

#Initial contamination event in soil 
log_cont_soil =log10(Flood_Cont)#contamination in log scale log CFU/g
#total soil die off day 
total_soil_dieoff_day = (rnorm(1,Dieoff_soil_mean,Dieoff_soil_sd))
#die off cont to sampling
total_soil_dieoff = total_soil_dieoff_day*Days_after_flood
#Inactivation function
Outs_Inac =Inactivation_Function(Cont =log_cont_soil, Prev = Total_Prevalence, logred = total_soil_dieoff )
#Contamination after soil die off to sampling
log_cont_soil = Outs_Inac[1]
#prevalence in soil after die off
Total_Prevalence = Outs_Inac[2]

#Sampling
if (soil_sampling == 1){
   total_cells <- (10^log_cont_soil)*(Ngrabs*Nmass)
   poisson_ans <- as.numeric((rpois(0,total_cells)))
   P_detect <- (1-poisson_ans)
}

#dieoff  sampling to planting
total_soil_dieoff = total_soil_dieoff_day*(Days_flood_planting-Days_after_flood)
#Contamination after soil dieoff to sampling
log_cont_soil = log_cont_soil-total_soil_dieoff

#Transfer from soil to plant. 
log_cont_prod = log_cont_soil*0.17

#


#Dose Response.
Func_DR_RServing(dose = 125)
Total_Ill_P = rbern(n=1,prob=0.25)
Total_Ill_R = rbern(Total_Ill_P, 1/26.1)
