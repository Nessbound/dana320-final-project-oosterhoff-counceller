# Import libraries
library(tidyverse)
library(shiny)
library(ggplot2)

# Load in the music data
musicData <- get(load("Data/data-music.RData"))

# Set up shiny ui
ui <- fluidPage(
    # Title
    titlePanel(h1(
        "Song Comparison App",
        align = "center"
    )),
    
    # Layout
    sidebarLayout(

        # Input section
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
            plotOutput("comparisonPlot")
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
}

# Run shiny app
shinyApp(ui, server)
