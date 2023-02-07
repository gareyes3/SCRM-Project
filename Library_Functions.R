#Library Functions
source("Library.R")
source("Inputs.R")
source("Nauta_Functions.R")

#Other Baseic Functions
Growth_Model<-function(Temp, Time){
  b= 0.023
  Tmin = 1.2
  k =-0.013
  if (Temp<5){
    growth =k*Time
  } else{
    growth =(b*(Temp-Tmin))^2/2.303
  }
  return (growth)
}



#Functions for specific unit operations
#Initial Contamination
Initial_Cont_function<-function(Cont_Distribution,Prev_Distribution,Params_Cont, Params_Pre){
  #log CFU/g
  if (Cont_Distribution == "Normal"){
    Contamination<-rnorm(1,Params_Cont[1],Params_Cont[2])
  } else if (Cont_Distribution == "Uniform") {
    Contamination<-runif(1,Params_Cont[1],Params_Cont[2])
  }
  
  if (Prev_Distribution == "Normal"){
    Prevalence<-rnorm(1,Params_Pre[1],Params_Pre[2])
  } else if (Prev_Distribution == "Uniform") {
    Prevalence<-runif(1,Params_Pre[1],Params_Pre[2])
  }
  
  return (c(Contamination, Prevalence))
}


#Infield Die-off Lettuce
Infield_dieoff_lettuce<-function (Cont,Prev,days_range){
  Days = round(runif(1,days_range[1],days_range[2]),0)
  Reduction = -(Days/(0.245/24))^0.3
  Outs = Inactivation_Function(Cont = Cont,Prev =Prev ,logred = Reduction)
  return (c(Outs[1],Outs[2]))
}

#Product Sampling Function

Sample_func_Internal<-function(x, pdetect){
  #x is a vector of grabs
  result = ifelse(x == 1, rbern(n=1,prob=pdetect), 0)
  return(result)
}

Produce_tesr_reject<-function (Cont, Prev, params){
  #params 1: sample mass
  #params 2: Ngrabs
  Pdetect = 1-exp(-Cont*(params[1]/params[2])) #probability of detecting based on cont levels
  srs_hits = rbern(n=params[2],prob=Prev) #if cont randomly dist, hit based on prevalence
  #print(srs_hits)
  Outcomes = sapply(X= srs_hits , FUN = Sample_func_Internal, pdetect = Pdetect ) #was contamination detected for each grab?
  print (Outcomes)
  if(sum(Outcomes)>0){
    Prev <- 0
  }
  return (c(Cont, Prev))
}

F_Cross_Cont_Blades_Lettuce<-function(Cont_P, Prev,Cont_Env = Total_CFU_Blade, Tr_a = 0, Tr_b= Tr_blade_lettuce, CC_fac = harvest_CC_factor){
  Outs = Cross_Cont_Function(Cont_P,Prev, Cont_Env, Tr_a, Tr_b, CC_fac)
  return(c(Outs[1], Outs[2]))
}

#Total Growth
F_Growth_Total<-function(Cont, Prev,Temp,Time){
  Growth = Growth_Model(Temp,Time)
  Outs = Growth_Function(Cont =Cont,Prev =Prev,Growth = Growth,popmax = 7)
  return (c(Outs[1], Outs[2]))
}


#Hand trimming
F_Cross_Cont_Hands_Let<-function(Cont_P, Prev,Cont_Env =Cont_Gloves, Tr_a = Tr_Gloves_Lettuce, Tr_b= Tr_Lettuce_Gloves, CC_fac = harvest_CC_factor){
  Outs = Cross_Cont_Function(Cont_P,Prev, Cont_Env, Tr_a, Tr_b, CC_fac)
  return(c(Outs[1], Outs[2]))
}

#Shredder
F_Cross_Cont_Shredder_Let<-function(Cont_P, Prev,Cont_Env =Cont_Shredd, Tr_a = Tr_Shredd_Lettuce, Tr_b= Tr_Lettuce_Shredd, CC_fac = harvest_CC_factor){
  Outs = Cross_Cont_Function(Cont_P,Prev, Cont_Env, Tr_a, Tr_b, CC_fac)
  return(c(Outs[1], Outs[2]))
}

#Conveyor Belt
F_Cross_Cont_Belt_Let<-function(Cont_P, Prev,Cont_Env =Cont_Belt, Tr_a = Tr_Belt_Lettuce, Tr_b= Tr_Lettuce_Belt, CC_fac = harvest_CC_factor){
  Outs = Cross_Cont_Function(Cont_P,Prev, Cont_Env, Tr_a, Tr_b, CC_fac)
  return(c(Outs[1], Outs[2]))
}


#Conveyor Shaker
F_Cross_Cont_Shaker_Let<-function(Cont_P, Prev,Cont_Env =Cont_Shaker, Tr_a = Tr_Shaker_Lettuce, Tr_b= Tr_Lettuce_Shaker, CC_fac = harvest_CC_factor){
  Outs = Cross_Cont_Function(Cont_P,Prev, Cont_Env, Tr_a, Tr_b, CC_fac)
  return(c(Outs[1], Outs[2]))
}


#Conveyor Centrigure
F_Cross_Cont_Centrifuge_Let<-function(Cont_P, Prev,Cont_Env =Cont_Centrifuge, Tr_a = Tr_Centrifuge_Lettuce, Tr_b= Tr_Lettuce_Centrifuge, CC_fac = harvest_CC_factor){
  Outs = Cross_Cont_Function(Cont_P,Prev, Cont_Env, Tr_a, Tr_b, CC_fac)
  return(c(Outs[1], Outs[2]))
}


#Dose Response Function
Func_DR_RServing<-function(Cont, Prev, Lot_Size, Lot_lb, Serving_size){
  Total_lb = Lot_Size*Lot_lb
  Total_Servings = (Total_lb*454)/Serving_size
  dose = (10^Cont)*Serving_size
  alpha<-0.267
  beta<-229.2928
  pr_illness = (1-(1+dose/beta)^-alpha)*Prev
  N_cases = Total_Servings*pr_illness
  return (N_cases)
}

