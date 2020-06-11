devtools::install_github("charlie86/spotifyr")
library(shiny)
library(spotifyr)
library(scrobbler)
library(tidyverse)
Sys.setenv(SPOTIFY_CLIENT_ID = '*************************')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '*************************')
users <- read_rds("./user-dat/users.csv")

# username <- "noah14noah"
# 
# my_scrobble <- download_scrobbles(username = username, api_key = "*************************")
# 
# saveRDS(my_scrobble, str_c("./user-dat/", username, ".csv"))

getTrackUID <- function(songInfo){
    print(songInfo)
    songString <- songInfo$song_title
    artistString <- songInfo$artist
    #print(str_c(song_title, artist))
    theTrack <- as.data.frame(search_spotify(songString, type = "track")) %>% 
        dplyr::select(artists, id, name) %>% 
        rename(UID = id, song_name = name) %>% 
        unnest(cols = artists, keep_empty = FALSE) %>% 
        dplyr::select(song_name, name, UID) %>% 
        filter(tolower(artistString) == tolower(name)) %>% 
        filter(tolower(songString)== tolower(song_name))
        
    return(theTrack$UID[1])
}

getUIDS <- function(scrobbleDF){
    makeUnique <- scrobbleDF %>% 
        dplyr::select(song_title, artist) 
    
    makeUnique <- unique(makeUnique) %>%
        rowwise %>%
        do(X = as_tibble(.) ) %>%
        ungroup
    
    makeUnique <- makeUnique %>% 
        dplyr::mutate(UID = map(X, getTrackUID)) #%>% 
        unnest(cols = c(X, UID)) 
    print("so close")
    scrobbleDF <- left_join(scrobbleDF, makeUnique, by = c("song_title" = "song_title", "artist" = "artist"))
    
    return(scrobbleDF)
}

access_token <- get_spotify_access_token()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            textInput("username",
                      "Enter username:"),
            passwordInput("key",
                          "Last.FM API Key: "),
            actionButton("go",
                         "View my scrobbles")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$go, {
        username <- input$username
        key <- input$key
        if(is_empty(users[users == username])){
            my_scrobble <- download_scrobbles(username = username, api_key = key)
            scrobble_UID <- getUIDS(my_scrobble)
            users <- append(users, username)
            saveRDS(users, "./user-dat/users.csv")
        }else{
            last_scrobble <- read_rds(str_c("./user-dat/", username, ".csv"))
            scrobble_UID <- update_scrobbles(last_scrobble,
                             "date_unix",
                             username,
                             key) %>% 
                unique()
        }
        saveRDS(scrobble_UID, str_c("./user-dat/", username, ".csv"))
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
