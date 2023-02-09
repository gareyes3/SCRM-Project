library(shiny)
library(shinydashboard)
library(shinyjs)

source("Library_Functions.R")
source("Inputs.R")
source("Shiny Functions.R")



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
                              Prev = globalinputs2$outs[2] , params = c(globalinputs$HS_Mass,globalinputs$HS_Grabs))
  } else if (inputs == "HC"){
    out= F_Cross_Cont_Blades_Lettuce(Cont_P = globalinputs2$outs[1], Prev = globalinputs2$outs[2])
  } else if (inputs == "FE"){
    out= F_Cross_Cont_Blades_Lettuce(Cont_P = globalinputs2$outs[1], Prev = globalinputs2$outs[2])
  }
  return (out)
}

shinyServer(function(input, output, session) {
  
  rvals1<-reactiveValues(sequence = 1)
  
  # input 1 event update AI
  #For extra information of step 1" 
  observeEvent(eventExpr = {input$s1}, handlerExpr = {
    output$ui_ops = renderUI(expr={f_options(input = input$s1)})
  })
  
  #add event 2
  observeEvent(eventExpr = {input$as_1}, handlerExpr = {
    output$Process_Step_2 = renderUI(expr={f_add_selection(sequence=2)})
  })
  
  #add inputs event 2
  observeEvent(eventExpr = {input$s2}, handlerExpr = {
    output$ui_ops2 = renderUI(expr={f_options(input = input$s2)})
  })
  
  #add event 3
  observeEvent(eventExpr = {input$as_2}, handlerExpr = {
    output$Process_Step_3 = renderUI(expr={f_add_selection(sequence=3)})
  })
  
  #add inputs event 3
  observeEvent(eventExpr = {input$s3}, handlerExpr = {
    output$ui_ops3 = renderUI(expr={f_options(input = input$s3)})
  })
  
  #add event 4
  observeEvent(eventExpr = {input$as_3}, handlerExpr = {
    output$Process_Step_4 = renderUI(expr={f_add_selection(sequence=4)})
  })
  
  #add inputs event 4
  observeEvent(eventExpr = {input$s4}, handlerExpr = {
    output$ui_ops4 = renderUI(expr={f_options(input = input$s4)})
  })
  
  #add event 5
  observeEvent(eventExpr = {input$as_4}, handlerExpr = {
    output$Process_Step_5 = renderUI(expr={f_add_selection(sequence=5)})
  })
  
  #add inputs event 5
  observeEvent(eventExpr = {input$s5}, handlerExpr = {
    output$ui_ops5 = renderUI(expr={f_options(input = input$s5)})
  })
  
  #add event 6
  observeEvent(eventExpr = {input$as_5}, handlerExpr = {
    output$Process_Step_6 = renderUI(expr={f_add_selection(sequence=6)})
  })
  
  #add inputs event 6
  observeEvent(eventExpr = {input$s6}, handlerExpr = {
    output$ui_ops6 = renderUI(expr={f_options(input = input$s6)})
  })
  
  
  
  observeEvent(eventExpr = {input$iterate_model}, handlerExpr = {
    
    for (i in 1:100){
      source("Inputs.R")
      
    
      global<-reactiveValues(outs =c(0,0), Drs = c())
      
      #Event 1
      observeEvent(eventExpr = input$s1, handlerExpr = {
        global$outs = Select_Event(inputs =input$s1,globalinputs = input, globalinputs2 = global)
      }
      )
      
      #Event 2
      observeEvent(eventExpr = input$s2, handlerExpr = {
        global$outs = Select_Event(inputs =input$s2,globalinputs = input,globalinputs2 = global )
      }
      )
      
      #Event 3
      observeEvent(eventExpr = input$s3, handlerExpr = {
        global$outs = Select_Event(inputs =input$s3,globalinputs = input,globalinputs2 = global )
        #print(global$outs)
      }
      )
      
      #Event 4
      observeEvent(eventExpr = input$s3, handlerExpr = {
        global$outs = Select_Event(inputs =input$s3,globalinputs = input,globalinputs2 = global )
        #print(global$outs)
      }
      )
      
      
      
      
      #Dose Response
      observeEvent(eventExpr = input$portion_size, handlerExpr ={
        global$outs = Func_DR_RServing(Cont = global$outs[1],
                                       Prev = global$outs[2],
                                       Lot_Size = input$Lot_Size, 
                                       Lot_lb =input$lb_per_lot, 
                                       Serving_size = input$portion_size)
        global$Drs<-c(global$Drs,global$outs) 
      })

    }
    output$prout =renderText(paste("The probability of an Outbreak is ", (sum(global$Drs>=2)/length(global$Drs))))   
    output$s1=renderPlot(hist(global$Drs,main="Illness 100 Iterations",
                              xlab="Number of Ilness",
                              ylab="Count",))
  }
  )
})


