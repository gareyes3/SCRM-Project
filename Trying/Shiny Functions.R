Modules_list <- c(
  "Select Unit Operation"= "SEL",
  "Flooding Event - Soil Contamination" = "FE",
  "Dieoff: soil" = "SDO",
  "Initial Unknown Contamination Event" = "CE",
  #"Contaminated Irrigation Water" = "CE_IW",
  "InField Dieoff" = "FDO",
  "Harvest Sampling" = "HS",
  "Harvesting Blade Contamination" = "HC"
  #"Dose Response" = "DR"
)


f_options = function(input, Mod_list){
  #CE, FDO, HS, HC, 
  if (input == "CE"){
    fluidRow(
      column( width = 12,
        selectInput(inputId = "CEdist",
                    label = "Select Contamination Distribution",
                    choices = list("Uniform", "Normal"),
                    selected = Modules_list[1]
                    
        ),
        #Conditional panel if this happens
        conditionalPanel(condition = "input.CEdist == 'Uniform'",
                         splitLayout(
                           numericInput(inputId = "Unifmin", 
                                        label = "Min log CFU/g", 
                                        value = 6),
                           numericInput(inputId = "Unifmax", 
                                        label = "Max log CFU/g", 
                                        value = 7))
        ),
        conditionalPanel(condition = "input.CEdist == 'Normal'",
                         splitLayout(
                           numericInput(inputId = "Normmean", 
                                        label = "Mean log CFU/g", 
                                        value = 5),
                           numericInput(inputId = "Normsd", 
                                        label = "SD log CFU/g", 
                                        value = 2))
        ), 
        offset = 1
      ))
  } else if (input == "FDO"){
    fluidRow(
      column(width = 12,
             splitLayout(
               numericInput(inputId = "FDO_Min", 
                            label = "Min Preharvest length (Days)", 
                            value = 20),
               numericInput(inputId = "FDO_Max", 
                            label = "Max Preharvest length (Days)", 
                            value = 30)), 
             offset = 1
             )
    )
  } else if (input == "HS"){
    fluidRow(
      column(width = 12,
             splitLayout(
               numericInput(inputId = "HS_Mass", 
                            label = "Total Mass Sampled (g)", 
                            value = 1500),
               numericInput(inputId = "HS_Grabs", 
                            label = "Number of grabs (#)", 
                            value = 60)) , 
             offset = 1
      )
    )
  } else if (input == "HC"){
    fluidRow(
      column(width = 12,
             splitLayout(
               numericInput(inputId = "HC_Tr_B_L", 
                            label = "Transfer Coefficient Blade to Lettuce", 
                            value = 0.0013),
               numericInput(inputId = "HC_Tr_L_B", 
                            label = "Transfer Coefficient Lettuce to Blade", 
                            value = 0)),
             offset = 1
      )
    )

  } else if (input == "FE"){
    column(width = 12,
           splitLayout(
             numericInput(inputId = "FE_Mean_Cont", 
                          label = "Mean Contamination in Soil after flood (CFU/g)", 
                          value = 0.0013),
             numericInput(inputId = "FE_SD_Cont", 
                          label = "SD Contamination in Soil after flood (CFU/g)", 
                          value = 0)),
           splitLayout(
             numericInput(inputId = "FE_Mean_Prev", 
                          label = "Minimum Prevalence in Soil", 
                          value = 0.0013),
             numericInput(inputId = "FE_SD_Prev", 
                          label = "Maximum Prevalence in Soil", 
                          value = 0)),
           offset = 1
    )
  }
}

f_add_selection =function(sequence){
  print("hellow")
  fluidRow(
    column( width = 8,
            selectInput(inputId = paste0("s",sequence),
                        label = paste0("Step ",sequence,":"),
                        choices = Modules_list,
                        selected = Modules_list[1]
            ),
            uiOutput(outputId = paste0("ui_ops",sequence)),
    ),
   
          column( width = 2,
                  actionButton(inputId = paste0("as_",sequence),
                               label = "add step"),
                  style = "padding: 0px; margin: 26px 0px 0px 0px;"
          )
    )
}