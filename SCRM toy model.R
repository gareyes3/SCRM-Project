
library(Rlab)
library(extraDist)

# Dose Response Function

Func_DR_RServing<-function(dose){
  alpha<-0.0571 
  beta<-2.2183
  1-((gamma(alpha+beta)*gamma(beta+dose))/(gamma(beta)*gamma(alpha+beta+dose)))
}

Inactivation_Function<-function(Cont,Prev,logred){
  
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

#Produce Dieoff
Total_preharvest_time = runif(1,30,38)
Dieoff_produce_mean = 0.2 #log CFU/day
Dieoff_produce_sd = 0.07 #log CFU/day

#### Flooding ###
log_cont_soil =log10(Flood_Cont)#contamination in log scale log CFU/g
#total soil dieoff day 
total_soil_dieoff_day = (rnorm(1,Dieoff_soil_mean,Dieoff_soil_sd))
#dieoff cont to sampling
total_soil_dieoff = total_soil_dieoff_day*Days_after_flood
#Contamination after soil dieoff to sampling
log_cont_soil = log_cont_soil-total_soil_dieoff
#prevalence in soil after dieoff
Total_Prevalence = 1*(1-((1-(10^7))/10^9)^(10^9))

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
