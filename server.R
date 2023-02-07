library(shiny)
library(shinydashboard)
library(shinyjs)

source("Library_Functions.R")
source("Inputs.R")




Select_Event<-function(inputs, globalinputs, globalinputs2){
  
  if (inputs == "CE"){
    if (globalinputs$CEdist == "Uniform"){
      out = Initial_Cont_function(Cont_Distribution = globalinputs$CEdist,
                                  Prev_Distribution = Prev_Dist,
                                  Params_Cont = c(globalinputs$Unifmin, globalinputs$Unifmax),
                                  Params_Pre = Prev_Params)
    } else if (globalinputs$CEdist == "Normal") {
      out = Initial_Cont_function(Cont_Distribution = globalinputs$CEdist,
                                  Prev_Distribution = Prev_Dist,
                                  Params_Cont = c(globalinputs$Normmean, globalinputs$Normsd),
                                  Params_Pre = Prev_Params)
    }
    
  } else if (inputs == "FDO") {
    out =Infield_dieoff_lettuce(Cont = globalinputs2$outs[1],
                                Prev = globalinputs2$outs[2],
                                days_range = c(globalinputs$FDO_Min,globalinputs$FDO_Max ) )
  } else if (inputs == "HS"){
    out = Produce_tesr_reject(Cont = globalinputs2$outs[1],
                              Prev = globalinputs2$outs[2] , params = c(1500,60))
  } else if (inputs == "DR"){
    out= Func_DR_RServing(Cont = globalinputs2$outs[1],
                          Prev = globalinputs2$outs[2],
                          Lot_Size = globalinputs$Lot_Size, 
                          Lot_lb =globalinputs$lb_per_lot, 
                          Serving_size = globalinputs$portion_size)
  }
  return (out)
}

shinyServer(function(input, output, session) {
  
  observeEvent(eventExpr = {input$iterate_model}, handlerExpr = {
    for (i in 1:100){
      source("Inputs.R")
      global<-reactiveValues(outs =c(0,0), outs2 = "Nine", Drs = c())
      
      observeEvent(eventExpr = input$s1, handlerExpr = {
        global$outs = Select_Event(inputs =input$s1,globalinputs = input, globalinputs2 = global)
      }
      )
      observeEvent(eventExpr = input$s2, handlerExpr = {
        global$outs = Select_Event(inputs =input$s2,globalinputs = input,globalinputs2 = global )
      }
      )
      
      #Last step mus be Dr
      observeEvent(eventExpr = input$s3, handlerExpr = {
        global$outs = Select_Event(inputs =input$s3,globalinputs = input,globalinputs2 = global )
        global$Drs<-c(global$Drs,global$outs)
      }
      )
    }
    output$prout =renderText(paste("The probability of an Outbreak is ", (sum(global$Drs>2)/length(global$Drs>2))))   
    output$s1=renderPlot(hist(global$Drs))
  }
  )
})


