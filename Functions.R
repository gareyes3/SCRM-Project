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

F_CE_Flooding<-function(Cont){
  
}