#Toy Model Together
source("Package Library.R")
source("Functions.R")

source("Inputs.R")

#Initial Contamination
Contamination = -Inf #log CFU/g

#Generic Contamination Event
Contamination = F_CE_Generic(ContIn = Contamination, ContCE = IC_ConcentrationIrr)

#Die-off


