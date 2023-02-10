


"For adding addional steps"
observeEvent(eventExpr = {input[[paste0("as_",rvals1$sequence)]]}, handlerExpr = {
  rvals1$sequence<-rvals1$sequence+1
  print(rvals1$sequence)
  output[[paste0("Process_Step_",rvals1$sequence)]] = renderUI(expr={f_add_selection(sequence=rvals1$sequence)})
})


observeEvent(eventExpr = {input[[paste0("s",rvals1$sequence)]]}, handlerExpr = {
  output[[paste0("ui_ops",rvals1$sequence)]] = renderUI(expr={f_options(input = input[[paste0("s",rvals1$sequence)]])})
})


