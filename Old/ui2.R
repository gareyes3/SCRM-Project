library(shiny)
library(plotly)
library(shinydashboard)


sidebar = dashboardSidebar(
  
  sidebarMenu(
    id = "sidebarMenu",
    menuItem(text = "STEC Flexible Model", 
             tabName = "FlexibleModel")
  )
)



Modules_list <- c(
  "Select Unit Operation"= "SEL",
  "Initial Unknown Contamination Event" = "CE",
  #"Contaminated Irrigation Water" = "CE_IW",
  "InField Dieoff" = "FDO",
  "Harvest Sampling" = "HS",
  "Harvesting Blade Contamination" = "HC"
  #"Dose Response" = "DR"
)



flexible_model = fluidRow(
  box(title = "Felible Risk Model Steps", solidHeader = TRUE, status = "primary",
      #Lot Information
      fluidRow(
        column(width = 10,
               h4("Lot Information")
        )
      ),
      
      fluidRow(
        column(width = 10,
               splitLayout(
                 numericInput(inputId = "Lot_Size",
                              label ="Lot Size (Acres)",
                              value = 45),
                 numericInput(inputId = "lb_per_lot",
                              label ="lb per lot",
                              value = 38000)
               )
        )
      ),
      
      #Lot Information
      fluidRow(
        column(width = 10,
               h4("Select Model Steps")
        )
      ),
      
      #Step 1
      fluidRow(
        column( width = 10,
                selectInput(inputId = "s1",
                            label = "Step 1:",
                            choices = Modules_list,
                            selected = Modules_list[1]
                ),#input 1
                conditionalPanel(condition="input.s1 == 'CE'",
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
                                 )
                ), 
                conditionalPanel(condition="input.s1 == 'FDO'",
                                 splitLayout(
                                   numericInput(inputId = "FDO_Min", 
                                                label = "Min Preharvest length (Days)", 
                                                value = 20),
                                   numericInput(inputId = "FDO_Max", 
                                                label = "Max Preharvest length (Days)", 
                                                value = 30))   
                ), #FDO Condition
                conditionalPanel(condition="input.s1 == 'HS'",
                                 splitLayout(
                                   numericInput(inputId = "HS_Mass", 
                                                label = "Total Mass Sampled (g)", 
                                                value = 1500),
                                   numericInput(inputId = "HS_Grabs", 
                                                label = "Number of grabs (#)", 
                                                value = 60))   
                ), #HS Condition
                conditionalPanel(condition="input.s1 == 'HC'",
                                 splitLayout(
                                   numericInput(inputId = "HC_Tr_B_L", 
                                                label = "Transfer Coefficient Blade to Lettuce", 
                                                value = 0.0013),
                                   numericInput(inputId = "HC_Tr_L_B", 
                                                label = "Transfer Coefficient Lettuce to Blade", 
                                                value = 0))   
                )
        )
      ),
      
      fluidRow(
        column(width = 10,
               h4("Dose Response")
        )
      ),
      
      fluidRow(
        column(width = 6,
               numericInput(inputId = "portion_size",
                            label = "Serving Size (g)",
                            value = 170
                            
               )
        )
      ),
      
      
      splitLayout(
        actionButton(inputId = "visualize_model", label = "Visualize"),
        actionButton(inputId = "iterate_model", label = "Iterate")
      )
      
      
  ) ,
  box(title = "Summary and Output", solidHeader = TRUE, status = "primary",
      #Stuff for outputs
      plotOutput(outputId = "s1"),
      p(),
      uiOutput(outputId = "prout")
      
  )
  
  
)


body = dashboardBody(
  tabItems(
    tabItem(tabName = "FlexibleModel", flexible_model)
  )
)    


shinyUI(dashboardPage(
  dashboardHeader(title = "Food Safety Risk Model"),
  sidebar,
  body
))




