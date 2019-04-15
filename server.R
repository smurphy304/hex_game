library(shiny)

shinyServer(function(input, output) {

  
  colorkey <- list("X" = "Black",
                   "O" = "White")
  
  sim_mat <- reactiveVal()
  clean_sim <- refresh_sim()
  sim_mat(clean_sim) 
  
  reactive({
    mat_size <- length(sim_mat()[1,])
  })
  
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
    print(input$plot_click)
    print("x:")
    print(input$plot_click$x)
    print("y:")
    print(input$plot_click$y)
    
    
    
    ## Scaling click area to plot area and number of hex chunks
    half_hexes <- input$mat_size * 2 + 1
    left_range <- input$plot_click$range$left
    
    y <- floor(input$plot_click$y)
    x_hexrange <- (input$plot_click$range$right - left_range)/
      half_hexes
    x <- (input$plot_click$coords_img$x - left_range - (y %% 2)*x_hexrange) %/%
      (x_hexrange*2)
    
    # x <- floor(input$plot_click$x - (floor(input$plot_click$y) %% 2)/2)
    
    
    
    
    ## Adding target entry to sim_mat & replotting
    update_mat <- sim_mat()
    new_entry <- (input$mat_size - y) + (input$mat_size * x)
    if(is.na(update_mat[new_entry])) {
      update_mat[new_entry] <- "O"
    }
    sim_mat(update_mat)
    
    output$hexplot <- renderPlot({
      hexplot(sim_mat(), colorkey)
    })
  })
  
  
  
})
