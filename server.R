library(shiny)

shinyServer(function(input, output) {

  
  colorkey <- list("X" = "Black",
                   "O" = "White")
  
  observeEvent(input$regen_hex, {
    sim_mat <- refresh_sim()
    output_mat <- self_play(sim_mat)
    
    output$hexplot <- renderPlot({
      hexplot(output_mat, colorkey)
    }) 
  })
    
  
    
    
 

  
  
  
  
})
