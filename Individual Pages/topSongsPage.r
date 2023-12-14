# Import libraries
library(tidyverse)
library(shinyjs)
library(shiny)
library(ggplot2)
library(fmsb)
library(plotly)

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
  
  # Display list and plot
  fluidRow(
    column(12,
           # Add a button to toggle the filters popup
           actionButton("toggleFilters", "Filters", style = "margin-bottom: 20px; margin-left: 20px;")
    )
  ),
  
  fluidRow(
    # Filters popup
    shinyjs::hidden(
      div(
        id = "filterPopup",
        align = "center",
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
        
        style = "padding: 20px; background-color: white; border: 2px solid #ddd; border-radius: 10px;"
      )
    )
  ),
  
  fluidRow(
    column(12,
           plotlyOutput("topSongsPlot"),
           
           # Display list of top songs
           htmlOutput("topSongsList")
    )
  )
)

# Set up shiny server
server <- function(input, output, session) {
  
  # Initialize reactive values to store filters
  rv <- reactiveValues(
    attributeOfFocus = "Danceability",  # Set default value
    genre = "[ALL]"  # Set default value
  )
  
  # Create drop-down for attribute of focus
  observe({
    updateSelectizeInput(
      session, 
      'attributeOfFocus',
      choices = c(
        "Danceability",
        "Energy",
        "Speechiness",
        "Acousticness",
        "Instrumentalness",
        "Liveness"
      ),
      selected = rv$attributeOfFocus
    )
  })
  
  # Update genre drop-down based on attribute of focus
  observe({
    updateSelectizeInput(
      session, 
      'genre', 
      choices = c('[ALL]', sort(unique(musicData$genre))),
      selected = rv$genre
    )
  })
  
  # Create plot and update top songs list
  output$topSongsPlot <- renderPlotly({
    
    # Prepare filtered data
    filteredMusicData <- prepareFilteredData(musicData, input$attributeOfFocus, input$genre, input$topN)
    
    # Calculate dynamic y-axis limits - set them slightly below and above min amd max respectively
    y_min <- min(filteredMusicData$score) * 0.999
    y_max <- max(filteredMusicData$score) * 1.001
    
    # Create a ggplot object
    base_plot <- ggplot(filteredMusicData, aes(x = reorder(song, -score), y = score, text = paste("Song: ", song, "<br>", input$attributeOfFocus, ": ", score))) +
      geom_bar(stat = "identity", fill = "#550099") +
      labs(title = "Top Songs", x = "Song", y = input$attributeOfFocus) +
      theme_minimal() +
      theme(axis.text.x = element_blank())  # Remove x-axis text
    
    # Convert ggplot object to plotly
    topSongsPlot <- ggplotly(base_plot, tooltip = c("text"), hoverinfo = "x+y+text")
    
    # Set dynamic y-axis limits in layout
    topSongsPlot <- layout(topSongsPlot, yaxis = list(range = c(y_min, y_max)))
    
    # Return the interactive plot
    topSongsPlot
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
           score = as.numeric(.data[[tolower(attributeOfFocus)]])
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
