# Import libraries
library(tidyverse)
library(shinyjs)
library(shiny)
library(ggplot2)
library(plotly)

# Load in the music data
musicData <- get(load("Data/data-music.RData"))

# Set up shiny ui
ui <- fluidPage(

    # Initialize shinyjs
    useShinyjs(),

    # Title
    titlePanel(h1(
        "Spotify Application",
        align = "center"
    )),
    
    # Layout
    tabsetPanel(

        # Comparison tab
        tabPanel(

            # Tab title
            title = "Song Comparison",

            sidebarPanel(

                # Song one label
                h3(
                    "Song One Options",
                    align = "center"
                ),

                # Select artist one
                selectizeInput(
                    "firstArtist",
                    "Artist",
                    choices = NULL
                ),

                # Select album one
                selectizeInput(
                    "firstAlbum",
                    "Album",
                    choices = NULL
                ),
                
                # Select song one
                selectizeInput(
                    "firstSong",
                    "Song",
                    choices = NULL
                ),

                # Song two label
                h3(
                    "Song Two Options",
                    align = "center"
                ),

                # Select artist two
                selectizeInput(
                    "secondArtist",
                    "Artist",
                    choices = NULL
                ),

                # Select album two
                selectizeInput(
                    "secondAlbum",
                    "Album",
                    choices = NULL
                ),

                # Select song two
                selectizeInput(
                    "secondSong",
                    "Song",
                    choices = NULL
                )
            ),

            # Display section
            mainPanel(
                plotOutput("comparisonPlot"),
                p("The displayed values are the winning value between the two songs. For example, if song A has 
                a dancability of 0.3, and song B has a dancability of 0.5, the dancability of song B will be shown.")
            )
        ),

        # Top songs tab
        tabPanel(

            # Tab title
            title = "Top Songs",

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
    )
)

# Set up shiny server
server <- function(input, output, session) {

    # Create drop down data for first song data
    updateSelectizeInput(
        session, 
        'firstArtist', 
        choices = musicData$artist_name, 
        server = TRUE
    )

    observeEvent(req(input$firstArtist), {
        updateSelectizeInput(
            session, 
            'firstAlbum', 
            choices = unique(
                subset(
                    musicData, 
                    musicData$artist_name == input$firstArtist
                )$album_name
            ), 
            server = TRUE
        )
    })
    observeEvent(req(input$firstAlbum), {
        updateSelectizeInput(
            session, 
            'firstSong', 
            choices = unique(
                subset(
                    musicData, 
                    musicData$artist_name == input$firstArtist &
                    musicData$album_name == input$firstAlbum
                )$track_name
            ), 
            server = TRUE
        )
    })

    # Create drop down data for second song data
    updateSelectizeInput(
        session, 
        'secondArtist', 
        choices = musicData$artist_name, 
        server = TRUE
    )

    observeEvent(req(input$secondArtist), {
        updateSelectizeInput(
            session, 
            'secondAlbum', 
            choices = unique(
                subset(
                    musicData, 
                    musicData$artist_name == input$secondArtist
                )$album_name
            ), 
            server = TRUE
        )
    })
    observeEvent(req(input$secondAlbum), {
        updateSelectizeInput(
            session, 
            'secondSong', 
            choices = unique(
                subset(
                    musicData, 
                    musicData$artist_name == input$secondArtist &
                    musicData$album_name == input$secondAlbum
                )$track_name
            ), 
            server = TRUE
        )
    })

   # Create plot
    output$comparisonPlot <- renderPlot({

        # BUILD PLOT DATA
        sectionsToSnip <- c(
            "danceability",
            "energy",
            "speechiness",
            "acousticness",
            "instrumentalness",
            "liveness"
        )
        songOne <- musicData %>% subset(track_name == input$firstSong)
        songOne <- songOne[1,]
        songTwo <- musicData %>% subset(track_name == input$secondSong)
        songTwo <- songTwo[1,]
        graphFrame <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(graphFrame) <- c("Name", "Value", "Winner")
        for(i in 1:length(songOne)) {
            if(names(songOne[i]) %in% sectionsToSnip) {
                name = names(songOne[i])
                if(songOne[[i]] > songTwo[[i]]) {
                    value <- abs(songOne[[i]]) * -1
                    winner <- input$firstSong
                }
                else {
                    value <- abs(songTwo[[i]])
                    winner <- input$secondSong
                }
                graphFrame <- rbind(
                    graphFrame,
                    data.frame(
                        "Name" = name,
                        "Value" = value,
                        "Winner" = winner
                    )
                )
            }
        }

        # DISPLAY GRAPH
        ggplot(
            graphFrame,
            aes(
                x = Name,
                y = Value,
                fill = Winner
            )
        ) + 
        geom_bar(
            stat = "identity"
        ) +
        coord_flip() +
        ylim(
            -1.0, 
            1.0
        ) +
        theme(
            plot.title = element_text(hjust = 0.5)
        ) +
        labs(
            title = "Song Comparison By Categories",
            y = "Category Score",
            x = ""
        )
    })

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
