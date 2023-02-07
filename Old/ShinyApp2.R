library(shiny)
library(plotly)
library(shinydashboard)


sidebar = dashboardSidebar(
  
  sidebarMenu(
    id = "sidebarMenu",
    menuItem(text = "Introduction", 
             tabName = "intro"),
    menuItem(text = "Inputs", 
             menuItem(text = "Smart Version", 
                      tabName = "v_smart"), 
             menuItem(text = "Manual Version", 
                      tabName = "v_manual", 
                      menuSubItem(text = "2D", tabName = "2D"), 
                      menuSubItem(text = "3D", tabName = "3D")),
             tabName = "inputs"),
    menuItem(text = "Outputs", 
             menuSubItem(text = "Visualization", tabName = "vis"),
             menuSubItem(text = "Data Export", tabName = "export"),
             tabName = "outputs")
  )
)

body = dashboardBody(
  
  tabItems(
    tabItem(tabName = "Flexible Model", flexible_model),
  )
)    


shinyUI(dashboardPage(
  
  dashboardHeader(title = "Food Safety Sampling"),
  sidebar,
  body
))



Modules_list <- c(
  "Initial Contamination Event" = "CE",
  "InField Dieoff" = "FDO",
  "Harvest Sampling" = "HS",
  "Harvest" = "H"
)

ui <- fluidPage(
  # Application title
  titlePanel("Process Model"),
  
  flexible_model=fluidRow(
    column( width = 10,
            selectInput(inputId = "s1",
                        label = "Step 1",
                        choices = Modules_list,
                        selected = Modules_list[1]
            ), #input 1
            conditionalPanel(condition="input.s1 == 'CE'",
              selectInput(inputId = "CEdist",
                          label = "Select Contamination Distribution",
                          choices = list("Uniform", "Normal"),
                          selected = Modules_list[1]
                          
              ),
              #Conditional pannel if this happens
              conditionalPanel(condition = "input.CEdist == 'Uniform'",
                               splitLayout(
                                 numericInput(inputId = "Unifmin", 
                                              label = "Min", 
                                              value = 3),
                                 numericInput(inputId = "Unifmin", 
                                              label = "Max", 
                                              value = 7))
              ),
              conditionalPanel(condition = "input.CEdist == 'Normal'",
                               splitLayout(
                                 numericInput(inputId = "Normmean", 
                                              label = "Mean", 
                                              value = 5),
                                 numericInput(inputId = "Normsd", 
                                              label = "SD", 
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
            ),
            conditionalPanel(condition="input.s1 == 'HS'",
                             splitLayout(
                               numericInput(inputId = "HS_Mass", 
                                            label = "Total Mass Sampled (g)", 
                                            value = 1500),
                               numericInput(inputId = "HS_Grabs", 
                                            label = "Number of grabs (#)", 
                                            value = 60))   
            ),
            conditionalPanel(condition="input.s1 == 'H'",
                             splitLayout(
                               numericInput(inputId = "HS_Mass", 
                                            label = "Total Mass Sampled (g)", 
                                            value = 1500),
                               numericInput(inputId = "HS_Grabs", 
                                            label = "Number of grabs (#)", 
                                            value = 60))   
            )
            
            

    ) 
  ),#step 1

  
  #Output 
  fluidRow(
           column(width = 10,
                  selectInput(inputId = "s2",
                              label = "Step 2",
                              choices = Modules_list,
                              selected = Modules_list[2]
                  )
           )

  )
  
)#fluid page main level


# Define server logic
server <- function(input, output, session) {
}


# Run the application 
shinyApp(ui = ui, server = server)