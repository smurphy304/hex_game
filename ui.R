library(shiny)
library(shinyalert)
source("hex_game.R")

shinyUI(fluidPage(
  useShinyalert(),

  # Application title
  titlePanel("Welcome to the Hex Game!"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("playturn",
                   "AI Turn"),
      actionButton("regen_hex",
                   "Refresh hex game"),
      actionButton("self_play",
                   "Randomly Finish Game"),
      sliderInput("mat_size",
                  "Number of Rows/Columns on the board",
                  min = 1, max = 10, value = 5)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("hexplot",
                 height = "400px", width = "600px",
                 dblclick = "plot_click")
    )
  )
))
