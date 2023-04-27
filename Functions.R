#Functions for SCRM Model. One function per model step.

#Basic Processes ---------------------------------------------------------------

##Inactivation
F_BP_Inactivation<-function(Cont,Prev,logred){
  #Cont: Should be in log scale (log CFUs)
  #Prev: should be a number between 0 and 1
  #log red: is the expected log reduction.
  Cont_out = Cont+logred
  Prev_out = Prev*(1-(1-10^Cont_out/10^Cont)^(10^Cont))
  return(c(Cont_out, Prev_out))
}

##Growth
F_BP_Growth<-function(Cont,Prev,Growth, popmax){
  #Cont: Should be in log scale (log CFUs)
  #Prev: should be a number between 0 and 1
  #growth: is the expected log reduction.
  #adapting population max
  Cont_out = Cont+Growth
  if (Cont_out>popmax){
    Cont_out<-popmax
  }
  return(c(Cont_out, Prev))
}

#Cross-Contamination
F_BP_CrossContamination<-function(Cont_P,Prev,Cont_Env, Tr_a, Tr_b, CC_fac){
  #Cont P is the total number of cells in P
  #Cont Env is the total number of cells in the environment
  #Tr_a is the transfer coefficient from prod to env
  #Tr_b is the transfer coefficient from env to prod.
  Cont_P = 10^Cont_P # Converting to cells
  Cont_Env = 10^Cont_Env #Converting to cells
  
  N_prod <- ((1-Tr_a)*Cont_P) + (Tr_b*Cont_Env)
  N_env <- (Tr_a*Cont_P ) + ((1-Tr_b)*Cont_Env)
  
  #Prevalence
  Prev_Out <- CC_fac*Prev/(1-Prev+CC_fac*Prev)
  return(c(log10(N_prod),Prev_Out))
}


#Model Steps -------------------------------------------------------------------

#Contamination Events
F_CE_Generic<-function(ContIn, ContCE){
  #ContIn = log CFU/g, contamination added by CE
  #ContCE = CFU/g, contamination before CE
  if(ContIn == -Inf){
    Cont_Out = log10(ContCE)
  } else {
    Cont_Out = ContIn + log10(ContCE)
  }
  return(Cont_Out)
}


#Dieoff_Preharvest
F_DieoffProduce <-function(ContIn){
  if (ContIn == 0){
    ContRed = 0
  } else{
    Red = -(1/(2.45/24))^0.3
    print(Red)
    ContRed = log10(ContIn) + Red
  }
  return(ContRed)
}





#Pre harvest Module

#Allende
Preharvest_Days = 45

IR_EcoliContSoil = rnorm(1,0.549,0.816) #log CFU/g
IR_EcoliSoilPlant = runif(IR_Days, 0.35,0.9) #Percentage

#Probability of a Sunny Day
IC_PrSunnyDay = 1-IR_RainyDays/IR_Days
IC_IsSunny = rbinom(IR_Days, 1, IC_PrSunnyDay)


Soil_Cont = 5 #CFU/g
IsSunny = rbinom(Preharvest_Days,IC_PrSunnyDay)
IsIrr_v = rbinom(Preharvest_Days,IC_PrIrr)
for (i in 1:Preharvest_Days){
  if (IsRainy == 1){
    
  }
   if (IsIrr == 1){
     
   }
}


