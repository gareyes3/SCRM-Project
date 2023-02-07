#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Create lists for dropdown
Modules_list <- c(
  "Initial Contamination Event" = "CE",
  "InField Dieoff" = "FDO",
  "Harvest Sampling" = "HS"
)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Process Model"),
  
# Sidebar
#splitLayout(
  fluidRow(
    column(width=10,
      # Select first step
      selectInput(inputId = "s1",
                  label = "Step 1",
                  choices = Modules_list,
                  selected = Modules_list[1]
      ),
      conditionalPanel(condition = "input.s1 == 'CE'",
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
                       ),#conditional panel 1
                       conditionalPanel(condition = "input.CEdist == 'Normal'",
                                        splitLayout(
                                          numericInput(inputId = "Normmean", 
                                                       label = "Mean", 
                                                       value = 5),
                                          numericInput(inputId = "Normsd", 
                                                       label = "SD", 
                                                       value = 2))
                       ) #conditional panel 2
      
      ), #conditional panel out
      # Select second step
      selectInput(inputId = "s2",
                  label = "Step 2",
                  choices = Modules_list,
                  selected = Modules_list[2]
      )
    )
  ),
  mainPanel(
    selectInput(inputId = "s2",
                label = "Step 2",
                choices = Modules_list,
                selected = Modules_list[2]
    )   
  )#mainpannel
#) #sidebar
) #fluidpage

# Define server logic
server <- function(input, output, session) {
}

# Run the application 
shinyApp(ui = ui, server = server)