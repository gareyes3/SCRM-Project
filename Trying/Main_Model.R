library(Rlab)

source("Nauta_Functions.R")
source("Library_Functions.R")
source("Inputs.R")

############# MODEL ##################

#####Contamination event #random
IC_Res<-Initial_Cont_function(Cont_Distribution = Cont_Dist,
                      Prev_Distribution = Prev_Dist,
                      Params_Cont = Cont_Params,
                      Params_Pre = Prev_Params)
contamination<-IC_Res[1]
prevalence<-IC_Res[2]

#####Infield Dieoff Section:
IFD_Res<-Infield_dieoff_lettuce(Cont = contamination,Prev = prevalence,Days =infield_dieoff_lettuce_Days )

contamination<-IFD_Res[1]
prevalence<-IFD_Res[2]

#####Harvest Sampling
HS_Res<-Produce_tesr_reject(Cont =contamination , Prev =prevalence , params = c(1500,60))

contamination<-HS_Res[1]
prevalence<-HS_Res[2]

#####Harvesting
H_Res<-F_Cross_Cont_Blades_Lettuce(Cont_P = contamination, Prev = prevalence)

contamination<-H_Res[1]
prevalence<-H_Res[2]

#####Transportation from Farm to Packinghouse
TFP_Res<-F_Growth_Total(Cont =contamination , Prev =prevalence,Temp = Time_Farm_Packinhouse, Time= Temperature_farm_Packinhouse)

#####Processing
Wash_Res<-Inactivation_Function(Cont =contamination , Prev =prevalence,logred = logred_wash)

contamination<-Wash_Res[1]
prevalence<-Wash_Res[2]

#Hand Trimming
CCHT_Res<-F_Cross_Cont_Hands_Let(Cont_P = contamination, Prev = prevalence)

contamination<-CCHT_Res[1]
prevalence<-CCHT_Res[2]

#Shredder CC
CCSh_Res<-F_Cross_Cont_Shredder_Let(Cont_P = contamination, Prev = prevalence)

contamination<-CCSh_Res[1]
prevalence<-CCSh_Res[2]

# Belt
CCB_Res<-F_Cross_Cont_Belt_Let(Cont_P = contamination, Prev = prevalence)

contamination<-CCB_Res[1]
prevalence<-CCB_Res[2]

#Shaker Belt
CCSh_Res<-F_Cross_Cont_Shaker_Let(Cont_P = contamination, Prev = prevalence)

contamination<-CCSh_Res[1]
prevalence<-CCSh_Res[2]

#Centrifuge
CCC_Res<-F_Cross_Cont_Centrifuge_Let(Cont_P = contamination, Prev = prevalence)

contamination<-CCC_Res[1]
prevalence<-CCC_Res[2]

#####Transportation from Facility to Retail

TFR_Res<-F_Growth_Total(Cont =contamination , Prev =prevalence,Temp = Temp_Packinhouse_Retail, Time= Time_Packinhouse_Retail)

contamination<-TFR_Res[1]
prevalence<-TFR_Res[2]

####Dose Response 



