library(shiny)

shinyServer(function(input, output) {

  
  colorkey <- list("X" = "Black",
                   "O" = "White")
  
  values <- reactiveValues()
  sim_mat <- refresh_sim()
  output$hexplot <- renderPlot({
    hexplot(sim_mat, colorkey)
  }) 
  
  observeEvent(input$regen_hex, {
    sim_mat <- refresh_sim()
    output$hexplot <- renderPlot({
      hexplot(sim_mat, colorkey)
    }) 
  })
    
  observeEvent(input$self_play, {
    sim_mat <- self_play(isolate(sim_mat))
    
    output$hexplot <- renderPlot({
      hexplot(sim_mat, colorkey)
    }) 
  })
  
  
  
  
})
