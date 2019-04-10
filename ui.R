library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Welcome to the Hex Game!"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("regen_hex",
                   "Refresh hex game")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("hexplot", width = "60%")
    )
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
))
