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
            
            # Select song one
            selectizeInput(
                "firstSong",
                h4("Song One"),
                choices = NULL
            ),

            # Select song two
            selectizeInput(
                "secondSong",
                h4("Song Two"),
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

    # Create drop down data
    updateSelectizeInput(session, 'firstSong', choices = musicData$track_name, server = TRUE)
    updateSelectizeInput(session, 'secondSong', choices = musicData$track_name, server = TRUE)

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
        )
    })
}

# Run shiny app
shinyApp(ui, server)
