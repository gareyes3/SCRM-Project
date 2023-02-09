source("Library.R")

###########Inputs ####################

#Initial Contamination
Cont_Dist = "Uniform"
Cont_Params = c(3,7) #min 3 and #max 7

Prev_Dist = "Uniform"
Prev_Params =  c(0.8,0.9)

#Days that the lettuce remains in the field
infield_dieoff_lettuce_Days = round(runif(1,15,20))

#Soil Contamination Harvest
Cont_Soil<-10^rtruncnorm(1,0,3.67,0.928,1.11) #Ecoli CFU/g
Ratio_O157_GE<-10^rtruncnorm(1,-Inf,0,-1.9,0.6)
Soil_HB<-10.22 #g
Total_CFU_Blade<-log10(Cont_Soil*Ratio_O157_GE*Soil_HB/1500) #CFU/g
Tr_blade_lettuce<- 0.0013
harvest_CC_factor<- runif(1,1,2)

#Transportation from farm-to-packinghouse
Time_Farm_Packinhouse = 2 #hrs
Temperature_farm_Packinhouse = 5 #°C


#Processing Parameters
logred_wash = -(rpert(1,min = 0.6,mode = 1,max = 1.6))

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


#Dose Response
Total_Acres = 45
lb_acre = 38000
serving_size = 170 #g
servings_per_acre = (Total_Acres*lb_acre*454)/serving_size



