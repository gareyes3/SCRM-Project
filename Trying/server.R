library(shiny)
library(shinydashboard)
library(shinyjs)

source("Library_Functions.R")
source("Inputs.R")
source("Shiny Functions.R")



Select_Event<-function(inputs, globalinputs, globalinputs2){

  
  #FE, CE, FDO, HS, HC, 
  if (inputs == "FE"){
    two_params<-c("Uniform", "Normal")
    three_params<-c("Pert", "Triangular")
    
    if (globalinputs$FEdist_cont%in%two_params && globalinputs$FEdist_prev%in%two_params ){
      out = Initial_Cont_function(Cont_Distribution = globalinputs$FEdist_cont,
                                  Prev_Distribution = globalinputs$FEdist_prev,
                                  Params_Cont = c(globalinputs$FE_cont1, globalinputs$FE_cont2),
                                  Params_Pre = c(globalinputs$FE_prev1,globalinputs$FE_prev2))
      
    } else if (globalinputs$FEdist_cont%in%two_params && globalinputs$FEdist_prev%in%three_params ){
      out = Initial_Cont_function(Cont_Distribution = globalinputs$FEdist_cont,
                                  Prev_Distribution = globalinputs$FEdist_prev,
                                  Params_Cont = c(globalinputs$FE_cont1, globalinputs$FE_cont2),
                                  Params_Pre = c(globalinputs$FE_prev1,globalinputs$FE_prev2, globalinputs$FE_prev3))
      
    }else if (globalinputs$FEdist_cont%in%three_params && globalinputs$FEdist_prev%in%two_params ){
      out = Initial_Cont_function(Cont_Distribution = globalinputs$FEdist_cont,
                                  Prev_Distribution = globalinputs$FEdist_prev,
                                  Params_Cont = c(globalinputs$FE_cont1, globalinputs$FE_cont2,globalinputs$FE_cont3 ),
                                  Params_Pre = c(globalinputs$FE_prev1,globalinputs$FE_prev2))
      
    } else if(globalinputs$FEdist_cont%in%three_params && globalinputs$FEdist_prev%in%three_params ){
      out = Initial_Cont_function(Cont_Distribution = globalinputs$FEdist_cont,
                                  Prev_Distribution = globalinputs$FEdist_prev,
                                  Params_Cont = c(globalinputs$FE_cont1, globalinputs$FE_cont2,globalinputs$FE_cont3 ),
                                  Params_Pre = c(globalinputs$FE_prev1,globalinputs$FE_prev2,globalinputs$FE_prev3))
    }
    
  } else if (inputs == "CE"){
    two_params<-c("Uniform", "Normal")
    three_params<-c("Pert", "Triangular")
    
    if (globalinputs$CEdist%in%two_params && globalinputs$CEdist_prev%in%two_params ){
      out = Initial_Cont_function(Cont_Distribution = globalinputs$CEdist,
                                  Prev_Distribution = globalinputs$CEdist_prev,
                                  Params_Cont = c(globalinputs$CE_cont1, globalinputs$CE_cont2),
                                  Params_Pre = c(globalinputs$CE_prev1,globalinputs$CE_prev2))
    } else if (globalinputs$CEdist%in%two_params && globalinputs$CEdist_prev%in%three_params ){
      out = Initial_Cont_function(Cont_Distribution = globalinputs$CEdist,
                                  Prev_Distribution = globalinputs$CEdist_prev,
                                  Params_Cont = c(globalinputs$CE_cont1, globalinputs$CE_cont2),
                                  Params_Pre = c(globalinputs$CE_prev1,globalinputs$CE_prev2, globalinputs$CE_prev3))
    }else if (globalinputs$CEdist%in%three_params && globalinputs$CEdist_prev%in%two_params ){
      out = Initial_Cont_function(Cont_Distribution = globalinputs$CEdist,
                                  Prev_Distribution = globalinputs$CEdist_prev,
                                  Params_Cont = c(globalinputs$CE_cont1, globalinputs$CE_cont2,globalinputs$CE_cont3 ),
                                  Params_Pre = c(globalinputs$CE_prev1,globalinputs$CE_prev2))
    } else if(globalinputs$CEdist%in%three_params && globalinputs$CEdist_prev%in%three_params ){
      out = Initial_Cont_function(Cont_Distribution = globalinputs$CEdist,
                                  Prev_Distribution = globalinputs$CEdist_prev,
                                  Params_Cont = c(globalinputs$CE_cont1, globalinputs$CE_cont2,globalinputs$CE_cont3 ),
                                  Params_Pre = c(globalinputs$CE_prev1,globalinputs$CE_prev2,globalinputs$CE_prev3))
    }
    
  } else if (inputs == "FDO") {
    out =Infield_dieoff_lettuce(Cont = globalinputs2$outs[1],
                                Prev = globalinputs2$outs[2],
                                Days = globalinputs2$days )
  } else if (inputs == "HS"){
    out = Produce_tesr_reject(Cont = globalinputs2$outs[1],
                              Prev = globalinputs2$outs[2] , params = c(globalinputs$HS_Mass,globalinputs$HS_Grabs))
  } else if (inputs == "HC"){
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
      
      global<-reactiveValues(outs =c(0,0), Drs = c(), days =NULL)
      
      #Loading days per of preharvest
      observeEvent(eventExpr =input$Day_min, handlerExpr = {
        global$days = F_growing_season_days(min = input$Day_min, mode = input$Day_Mode, max = input$Day_Max)
      })
      
      #Event 1
      observeEvent(eventExpr = input$s1, handlerExpr = {
        print(global$outs)
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
                              ylab="Count"))
    output$contprev = renderText(global$outs)
  }
  )
})


