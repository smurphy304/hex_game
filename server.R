library(shiny)

shinyServer(function(input, output) {
  turn <- reactiveVal(value = 1, label = "Turn")
  
  
  colorkey <- list("X" = "Black",
                   "O" = "White")
  print("Turn")
  print("Done")
  sim_mat <- reactiveVal()
  isolate({
    clean_sim <- refresh_sim(mat_size = input$mat_size)
    sim_mat(clean_sim)
  })
  
   
  
  reactive({
    mat_size <- length(sim_mat()[1,])
  })
  
  # values <- reactiveValues()
  # sim_mat <- refresh_sim()
  
  output$hexplot <- renderPlot({
    hexplot(sim_mat(), colorkey)
  })
  
  # Refresh the Hex game & grid
  isolate({
    observeEvent(input$regen_hex, {
      print(input$mat_size)
      clean_sim <- refresh_sim(mat_size = input$mat_size)
      sim_mat(clean_sim) 
      turn(1)
      output$hexplot <- renderPlot({
        hexplot(sim_mat(), colorkey)
      })
    })
  })
  # AI finish game
  observeEvent(input$self_play, {
    sp_mat <- self_play(sim_mat(), turn())
    sim_mat(sp_mat)
    output$hexplot <- renderPlot({
      hexplot(sim_mat(), colorkey)
    })
    if(winner_select(sim_mat())) {
      shinyalert("Winner!", "A player has won the game.", type = "success")
    }
  })
  # AI turn handling
  observeEvent(input$playturn, {
    new_sim <- contra_turn(sim_mat(), turn())
    sim_mat(new_sim)
    turn(turn() + 1)
    output$hexplot <- renderPlot({
      hexplot(sim_mat(), colorkey)
    })
    if(winner_select(sim_mat())) {
      shinyalert("Winner!", "A player has won the game.", type = "success")
    }
  })
  
  # Player moves handling
  observeEvent(input$plot_click, {
    print(input$plot_click)
    print("x:")
    print(input$plot_click$x)
    print("y:")
    print(input$plot_click$y)
    
    tval <- turn()
    
    ## Scaling click area to plot area and number of hex chunks
    half_hexes <- input$mat_size * 2 + 1
    left_range <- input$plot_click$range$left
    
    y <- floor(input$plot_click$y)
    x_hexrange <- (input$plot_click$range$right - left_range)/
      half_hexes
    x <- (input$plot_click$coords_img$x - left_range - (y %% 2)*x_hexrange) %/%
      (x_hexrange*2) # Source of poor highlighting issue
    
    print("x, y, & hexrange")
    print(x)
    print(y)
    print(x_hexrange)
    ## Adding target entry to sim_mat & replotting
    update_mat <- sim_mat()
    new_entry <- (input$mat_size - y) + (input$mat_size * x)
    print("size & coord")
    print(input$mat_size)
    print(new_entry)
    if(is.na(update_mat[new_entry])) {
      if(turn() %% 2 == 0) move <- "X" else move <- "O"
      update_mat[new_entry] <- move
      turn(turn() + 1)
    }
    sim_mat(update_mat)
    
    output$hexplot <- renderPlot({
      hexplot(sim_mat(), colorkey)
    })
    
    if(winner_select(sim_mat())) {
      shinyalert("Winner!", "A player has won the game.", type = "success")
    }
    
  })
})
