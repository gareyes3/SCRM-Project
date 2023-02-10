library(shiny)
library(plotly)
library(shinydashboard)


source("Library_Functions.R")
source("Inputs.R")
source("Shiny Functions.R")

sidebar = dashboardSidebar(
  
  sidebarMenu(
    id = "sidebarMenu",
    menuItem(text = "STEC Flexible Model", 
             tabName = "FlexibleModel")
  )
)




flexible_model = fluidRow(
  box(title = "Felible Risk Model Steps", solidHeader = TRUE, status = "primary",
      #Lot Information
      fluidRow(
        column(width = 10,
               h4("Initial Information for Model Setup")
        )
      ),
      
      fluidRow(
        column(width = 10,
               splitLayout(
                 numericInput(inputId = "Lot_Size",
                              label ="Lot Size (Acres)",
                              value = 45),
                 numericInput(inputId = "lb_per_lot",
                              label ="Pounds per Acre",
                              value = 38000)
               )
        )
      ),
      
      fluidRow(
        column(width = 10,
               h5("Growing Season Length (Days)"),
               splitLayout(
                 numericInput(inputId = "Day_min",
                              label ="Minumum (Days)",
                              value = 34),
                 numericInput(inputId = "Day_Mode",
                              label ="Most Likely (Days)",
                              value = 36.8),
                 numericInput(inputId = "Day_Max",
                              label ="Maximum (Days)",
                              value = 42)
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
        column( width = 8,
                selectInput(inputId = "s1",
                            label = "Step 1:",
                            choices = Modules_list,
                            selected = Modules_list[1]
                ),#input 1
                uiOutput(outputId = "ui_ops")

        ),
        column( width = 2,
                actionButton(inputId = "as_1",
                              label = "add step"),
                style = "padding: 0px; margin: 26px 0px 0px 0px;"
        )
      ),
      uiOutput(outputId = "Process_Step_2"),
      uiOutput(outputId = "Process_Step_3"),
      uiOutput(outputId = "Process_Step_4"),
      uiOutput(outputId = "Process_Step_5"),
      uiOutput(outputId = "Process_Step_6"),
      uiOutput(outputId = "Process_Step_7"),
      
      
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




