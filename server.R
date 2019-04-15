library(shiny)

shinyServer(function(input, output) {

  
  colorkey <- list("X" = "Black",
                   "O" = "White")
  
  sim_mat <- reactiveVal()
  clean_sim <- refresh_sim()
  sim_mat(clean_sim) 
  
  
  # values <- reactiveValues()
  # sim_mat <- refresh_sim()
  
  
  
  output$hexplot <- renderPlot({
    hexplot(sim_mat(), colorkey)
  }) 
  
  isolate({
    observeEvent(input$regen_hex, {
      clean_sim <- refresh_sim()
      sim_mat(clean_sim) 
      output$hexplot <- renderPlot({
        hexplot(sim_mat(), colorkey)
      })
    })
  })
  
  observeEvent(input$self_play, {
    sp_mat <- self_play(sim_mat())
    sim_mat(sp_mat)
    output$hexplot <- renderPlot({
      hexplot(sim_mat(), colorkey)
    }) 
  })
  
  observeEvent(input$plot_click, {
    print(input$plot_click$x)
    print(input$plot_click$y)
    
    x <- floor(input$plot_click$x)
    y <- 5 - floor(input$plot_click$y)
    new_entry <- y + 5 * x
    
    update_mat <- sim_mat()
    update_mat[new_entry] <- "O"
    sim_mat(update_mat)
    
    output$hexplot <- renderPlot({
      hexplot(sim_mat(), colorkey)
    })
  })
  
  
  
})
