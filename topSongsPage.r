# Hey Connor! Noah here. Wanted to set you up with a cute little starting framework, so here it is!
# The ui is what is used to design the front end itself; essentially makes the blocks of the frontend
# The server is where the graph is done, among other things
# Run this page by clicking you cursor at the bottom of the page, then using "ctrl+shift+enter"
# If you need help, ask me, or look at my page. Thanks in advance for your hard work! ^-^b

# Import libraries
library(tidyverse)
library(shinyjs)
library(shiny)
library(ggplot2)

# Load in the music data
musicData <- get(load("Data/data-music.RData"))
print(musicData)

# Set up shiny ui
ui <- fluidPage(
  
  # Initialize shinyjs
  useShinyjs(),
  
  # Title
  titlePanel(
    h1(
      "Top Songs App", 
       align = "center"
      )
  ),
  
  # Main section for displaying list and plot
  mainPanel(
    
    # Add a button to toggle the filters popup
    actionButton("toggleFilters", "Filters", style = "margin-bottom: 20px; margin-left: 20px;"),
    
    plotOutput("topSongsPlot"),
    
    # Display list of top songs
    htmlOutput("topSongsList")
    
    ),
  
  # Filters popup
  shinyjs::hidden(
    div(
      id = "filterPopup",
      div(
        # Filters
        h3(
          "Filters",
          align = "center"
        ),
        
        # Slider for Top N
        sliderInput(
          "topN",
          "Number of Songs",
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
        ),
      ),
      
      style = "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); padding: 20px; background-color: white; border: 2px solid #ddd; border-radius: 10px;"
    )
  )
)

# Set up shiny server
server <- function(input, output, session) {
  
  # Create drop down for attribute of focus
  updateSelectizeInput(
    session, 
    'attributeOfFocus',
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
                   sort(unique(musicData$genre))
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
    
    # Format the list into an HTML structure
    topSongsHTML <- paste(
      "<div>",
      sapply(seq_len(nrow(topSongsList)), function(i) {
        paste(
          "<div style='border: 2px solid #ddd; border-radius: 32px; margin: 16px; display: flex; align-items: center;'>",   # Outer container for song "object"
          "<div style='font-size: 48px; color: #509; font-weight: bold; margin-left: 16px; margin-right: 12px;'>", i,   # Number text
          "</div>",
          "<div style='font-size: 48px; color: #eee; margin-right: 16px;'>", "|",   # Divider line
          "</div>",
          "<div style='flex-grow: 1; text-align: left;'>",   # Container for song and artist text
          "<div style='font-size: 24px; color: #000; font-weight: bold;'>", topSongsList[i, "song"],   # Song text
          "</div>",
          "<div style='font-size: 16px; color: #000;'>", topSongsList[i, "artist"],   # Artist text
          "</div>",
          "</div>",
          "</div>",
          sep = ""
        )
      }),
      "</div>"
    )
    
    # Use HTML function to interpret the HTML code
    HTML(topSongsHTML)
  })
  
  # Add shinyjs code to toggle the filters popup
  observeEvent(input$toggleFilters, {
    shinyjs::toggle("filterPopup")
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
