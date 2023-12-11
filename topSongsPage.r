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
  # Title
  titlePanel(
    h1(
      "Top Songs App",
      align = "center"
      )
    ),
  
  # Layout
  layout(
    
    sidebarPanel(
      
      #TODO: Add attribute selection - The attribute of focus, i.e. energy, danceability, most plays, etc.
      #TODO: Maybe add a way for multiple categories to be selected? Songs can then be displayed in bar clusters.
      #      Each song has a cluster of 1-3 bars, and each bar is the rating for the selected attributes. - Would need a way to determine top order if multiple attributes are selected
      #TODO: Add filter options - Genres, artists, etc.
      
    ),
    
    mainPanel(
      
      #TODO: Display list of top x number of songs from filtered selection, sorted desc by category
      #TODO: Display song titles and artists in big/bold letters, then less important info in smaller sub-text underneath song title/artist. - i.e. album, genre, year, etc.
      #TODO: Display related graphs/plots
      
    )
    
  )
  
  
  
)

# Set up shiny server
server <- function(input, output, session) {

}

# Run shiny app
shinyApp(ui, server)