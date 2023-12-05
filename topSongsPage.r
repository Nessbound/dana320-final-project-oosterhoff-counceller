# Hey Connor! Noah here. Wanted to set you up with a cute little starting framework, so here it is!
# The ui is what is used to design the front end itself; essentially makes the blocks of the frontend
# The server is where the graph is done, among other things
# Run this page by clicking you cursor at the bottom of the page, then using "ctrl+shift+enter"
# If you need help, ask me, or look at my page. Thanks in advance for your hard work! ^-^b

# Import libraries
library(tidyverse)
library(shiny)
library(ggplot2)

# Load in the music data
musicData <- get(load("Data/data-music.RData"))

# Set up shiny ui
ui <- fluidPage(

)

# Set up shiny server
server <- function(input, output, session) {

}

# Run shiny app
shinyApp(ui, server)