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
print(musicData)

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
  sidebarLayout(
    
    sidebarPanel(
      
      # Filters
      h3(
        "Filter",
        align = "center"
      ),
      
      # Slider for Top N
      sliderInput(
        "topN",
        "Number of Top Songs to Display:",
        min = 1,
        max = 100,
        value = 10
      ),
      
      # Select category
      selectizeInput(
        "attributeOfFocus",
        "Focus Category",
        choices = NULL
      ),
      
      # Select genre
      selectizeInput(
        "genre",
        "Genre",
        choices = NULL
      )
      
      #TODO: Maybe add a way for multiple categories to be selected? Songs can then be displayed in bar clusters.
      #      Each song has a cluster of 1-3 bars, and each bar is the rating for the selected attributes. - Would need a way to determine top order if multiple attributes are selected
      #TODO: Add filter options - artists, etc.
      
    ),
    
    # Main section for displaying list and plot
    mainPanel(
      
      plotOutput("topSongsPlot"),
      
      # Display list of top songs
      htmlOutput("topSongsList")
      
      #TODO: Display list of top x number of songs from filtered selection, sorted desc by category
      #TODO: Display song titles and artists in big/bold letters, then less important info in smaller sub-text underneath song title/artist. - i.e. album, genre, year, etc.
      #TODO: Display related graphs/plots
      
    )
    
  )
  
)

# Set up shiny server
server <- function(input, output, session) {
  
  # Create drop down for attribute of focus
  updateSelectizeInput(
    session, 
    'attributeOfFocus', 
    # TODO: Use better way - I know there is a way, but can't remember how
    choices = c(
      "danceability",
      "energy",
      "speechiness",
      "acousticness",
      "instrumentalness",
      "liveness",
      "popularity"
    ),
    server = TRUE
  )
  
  observeEvent(req(input$attributeOfFocus), {
    updateSelectizeInput(
      session, 
      'genre', 
      choices = c( '[ALL]', 
                   sort(unique(
                     musicData$genre
                   ))
      ),
      server = TRUE
    )
  })
  
  # Create plot and update top songs list
  output$topSongsPlot <- renderPlot({
    
    # Prepare filtered data
    filteredMusicData <- prepareFilteredData(musicData, input$attributeOfFocus, input$genre, input$topN)
    
    # Display graph
    ggplot(
      filteredMusicData,
      aes(
        x = song,
        y = score
      )
    ) + 
      geom_col() +
      labs(
        title = "Top Songs"
      )
  })
  
  # Display top songs list
  output$topSongsList <- renderUI({
    # Prepare filtered data
    topSongsList <- prepareFilteredData(musicData, input$attributeOfFocus, input$genre, input$topN)
    
    # Format the list into an HTML table
    topSongsTable <- paste(
      "<table style='width:100%; border-collapse: collapse;'>",
      "<tr style='background-color: #f2f2f2;'><th>Song</th><th>Artist</th><th>Album</th><th>Genre</th><th>Score</th></tr>",
      apply(topSongsList, 1, function(row) {
        paste("<tr><td>", row[1], "</td><td>", row[2], "</td><td>",
              row[3], "</td><td>", row[4], "</td><td>",
              row[5], "</td></tr>")
      }),
      "</table>"
    )
    
    # Use HTML function to interpret the HTML code
    HTML(topSongsTable)
  })
  
}

# Helper function to prepare filtered data
prepareFilteredData <- function(musicData, attributeOfFocus, selectedGenre, topN) {
  
  filteredMusicData <- musicData |>
    mutate(song = track_name,
           artist = artist_name,
           album = album_name,
           score = .data[[attributeOfFocus]]
    ) |>
    select(song, artist, album, genre, score) |>
    arrange(desc(score)) |>
    distinct(song, .keep_all = TRUE)
  
  if (!(selectedGenre == '[ALL]')) {
    filteredMusicData <- filteredMusicData |>
      filter(genre == selectedGenre)
  }
  
  filteredMusicData <- filteredMusicData |>
    slice(1:topN)

  return(filteredMusicData)
}

# Run shiny app
shinyApp(ui, server)

# Notes
# - Ordering of bars on chart
# - UI: Switch between tabs
