#Library Functions
source("Library.R")
#source("Inputs.R")
source("Nauta_Functions.R")


#Helper Functions
get_value_dist<-function(Distribution, params){
  if (length(params) ==2){
    if (Distribution == "Uniform"){
      out = runif(1,params[1], params[2])
    } else if (Distribution == "Normal"){
      out = rnorm(1,params[1], params[2])
    }
  }
  if (length(params) ==3){
    if (Distribution == "Pert"){
      out = rpert(1,params[1], params[2], params[3] )
    } else if (Distribution == "Pert"){
      out = rtriang(1,params[1], params[2], params[3] )
    }
  }
  return (out)
}




#Other Basic Functions
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

#Select growing season length (days)
F_growing_season_days<-function(min,mode,max){
  days<-round(mc2d::rpert(n=1,min=min,mode=mode,max=max),0)
  return (days)
}


#Functions for specific unit operations ------------------


##Generic Contamination Function
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

Initial_Cont_function2<-function(Cont, Prev){

  return (c(Cont, Prev))
}



#Soil Die off
Infield_dieoff_soil<-function (Cont,Prev,Days){
  #Days = round(runif(1,days_range[1],days_range[2]),0)
  Reduction = -(Days/(137/24))^0.96
  Outs = Inactivation_Function(Cont = Cont,Prev =Prev ,logred = Reduction)
  return (c(Outs[1],Outs[2]))
}


#Initial Contamination Irrigation Water
F_Cont_Ir_Water<-function(){
  Trans_irr_water = rpert(Days, 1.8,21.6)
}


#Irrigation and rain splash conts functions
is_sunny = function(days, raindays){
  Sp_P_Sun <- 1 - (raindays/days)
  Sp_Sun <- rbinom(days, 1, Sp_P_Sun)
  return(Sp_Sun)
}

fc_rsp <-  function(contamsoil,prev, soiltrans, ec2plant, sunny_yn){
  P_Rain_Sp <- 1
  C_RSp <- (10^(contamsoil) * soiltrans * ec2plant * P_Rain_Sp * (1 - sunny_yn))
  szn_sum <- sum(C_RSp)
  log_trans <-  round(log10(szn_sum),2)
  return(c(log_trans, prev))
}

fc_irrsp <-  function(contamsoil, prev, soiltrans, ec2plant,irrsp_yn, sunny_yn){
  C_IrrSp <- (10^(contamsoil) * soiltrans * ec2plant * irrsp_yn * sunny_yn)
  szn_sum <- sum(C_IrrSp)
  log_trans <-  round(log10(szn_sum),2)
  return(c(log_trans, prev))
}


#Infield Die-off Lettuce
Infield_dieoff_lettuce<-function (Cont,Prev,Days){
  #Days = round(runif(1,days_range[1],days_range[2]),0)
  Reduction = -(Days/(0.245/24))^0.3
  Outs = Inactivation_Function(Cont = Cont,Prev =Prev ,logred = Reduction)
  return (c(Outs[1],Outs[2]))
}

#Product Sampling Function

Sample_func_Internal<-function(x, pdetect){
  #x is a vector of grabs
  result = ifelse(x == 1, rbinom(n=1,size = 1,prob=pdetect), 0)
  return(result)
}

Produce_tesr_reject<-function (Cont, Prev, params){
  #params 1: sample mass
  #params 2: Ngrabs
  Cont_Nlog = 10^Cont
  Pdetect = 1-exp(-Cont_Nlog*(params[1]/params[2])) #probability of detecting based on cont levels
  print(Pdetect)
  srs_hits = rbinom(n=params[2], size = 1,prob=Prev) #if cont randomly dist, hit based on prevalence
  #print(srs_hits)
  Outcomes = sapply(X= srs_hits , FUN = Sample_func_Internal, pdetect = Pdetect ) #was contamination detected for each grab?
  print(sum(Outcomes, na.rm=T))
  if(sum(Outcomes, na.rm=T)>0){
    Prev <- 0
  }
  return (c(Cont, Prev))
}

rbern(n=60,prob=0.1)
rbinom(n=60,1,0.1)

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


#Conveyor Centrifuge
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
  pr_illness = (1-(1+dose/beta)^-alpha)#*Prev
  print(pr_illness)
  N_cases = Total_Servings*pr_illness
  print(N_cases)
  Total = floor(N_cases) + rbinom(1,1,N_cases%%1)
  return (Total)
}
