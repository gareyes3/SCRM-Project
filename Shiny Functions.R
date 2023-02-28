Modules_list <- c(
  "Select Model Step"= "SEL",
  "Flooding Event: (Water contmaination to Soil)" = "FE",
  "Dieoff: soil" = "SDO",
  "Contamination: Irrigation Splash (Soil - Produce)" = "CE_IS",
  "Contamination: Rain Splash (Soil - Produce)" = "CE_RS",
  "Contamination: Own Event" = "CE",
  #"Contaminated Irrigation Water" = "CE_IW",
  "Dieoff: Produce" = "FDO",
  "Harvest Sampling: Produce" = "HS",
  "Cross-Contamination: Harvesting Blade" = "HC"
  #"Dose Response" = "DR"
)


f_options = function(input, Mod_list){
  #CE, FDO, HS, HC, 
  if (input == "CE"){
    fluidRow(
      column( width = 12,
        selectInput(inputId = "CEdist_cont",
                    label = "Select Contamination Distribution",
                    choices = list("Uniform", "Normal"),
                    selected = Modules_list[1]
                    
        ),
        #Conditional panel if this happens
        conditionalPanel(condition = "input.CEdist_cont == 'Uniform'",
                         splitLayout(
                           numericInput(inputId = "CE_cont1", 
                                        label = "Min log CFU/g", 
                                        value = 0.002),
                           numericInput(inputId = "CE_cont2", 
                                        label = "Max log CFU/g", 
                                        value = 0.003))
        ),
        conditionalPanel(condition = "input.CEdist_cont == 'Normal'",
                         splitLayout(
                           numericInput(inputId = "CE_cont1", 
                                        label = "Mean log CFU/g", 
                                        value = 0.002),
                           numericInput(inputId = "CE_cont2", 
                                        label = "SD log CFU/g", 
                                        value = 0.001))
        ),
        selectInput(inputId = "CEdist_prev",
                    label = "Select Prevalence Distribution",
                    choices = list("Uniform", "Normal"),
                    selected = Modules_list[1]
                    
        ),
        #Conditional panel if this happens
        conditionalPanel(condition = "input.CEdist_prev == 'Uniform'",
                         splitLayout(
                           numericInput(inputId = "CE_prev1", 
                                        label = "Min Prevalence", 
                                        value = 0.5),
                           numericInput(inputId = "CE_prev2", 
                                        label = "Max Prevalence", 
                                        value = 1))
        ),
        conditionalPanel(condition = "input.CEdist_prev == 'Normal'",
                         splitLayout(
                           numericInput(inputId = "CE_prev1", 
                                        label = "Mean Prevalence", 
                                        value = 0.5),
                           numericInput(inputId = "CE_prev2", 
                                        label = "SD Prevalence", 
                                        value = 0.2))
        ),
        offset = 1
      ))
  } else if (input == "FE"){
    fluidRow(
      column(width = 12,
             selectInput(inputId = "FEdist_cont",
                         label = "Select Contamination Distribution",
                         choices = list("Uniform", "Normal"),
                         selected = Modules_list[1]
                         
             ),
             #Conditional panel if this happens
             conditionalPanel(condition = "input.FEdist_cont == 'Uniform'",
                              splitLayout(
                                numericInput(inputId = "FE_cont1", 
                                             label = "Min log CFU/g", 
                                             value = 3.0),
                                numericInput(inputId = "FE_cont2", 
                                             label = "Max log CFU/g", 
                                             value = 4.0))
             ),
             conditionalPanel(condition = "input.FEdist_cont == 'Normal'",
                              splitLayout(
                                numericInput(inputId = "FE_cont1", 
                                             label = "Mean log CFU/g", 
                                             value = 3.0),
                                numericInput(inputId = "FE_cont2", 
                                             label = "SD log CFU/g", 
                                             value = 0.4))
             ),
             selectInput(inputId = "FEdist_prev",
                         label = "Select Prevalence Distribution",
                         choices = list("Uniform", "Normal"),
                         selected = Modules_list[1]
                         
             ),
             #Conditional panel if this happens
             conditionalPanel(condition = "input.FEdist_prev == 'Uniform'",
                              splitLayout(
                                numericInput(inputId = "FE_prev1", 
                                             label = "Min Prevalence", 
                                             value = 0.5),
                                numericInput(inputId = "FE_prev2", 
                                             label = "Max Prevalence", 
                                             value = 1))
             ),
             conditionalPanel(condition = "input.FEdist_prev == 'Normal'",
                              splitLayout(
                                numericInput(inputId = "FE_prev1", 
                                             label = "Mean Prevalence", 
                                             value = 0.5),
                                numericInput(inputId = "FE_prev2", 
                                             label = "SD Prevalence", 
                                             value = 0.2))
             ), 
             offset = 1
             )
    )
  } else if (input == "CE_IS"){

  }
  else if(input == "FDO"){
    
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

  }  else if (input == "SDO"){
    column (width = 12,
            splitLayout(
              numericInput(inputId = "Day_FE_Min", 
                           label = "Min Days between Flooding and Planting", 
                           value = 30),
              numericInput(inputId = "Day_FE_Max", 
                           label = "Max Days between Flooding and Planting", 
                           value = 60)
            ))
  }
}



f_add_selection =function(sequence){
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