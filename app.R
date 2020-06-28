library(shiny) 
library(spotifyr)
library(scrobbler)
library(tidyverse)

users <- read_rds("./user-dat/users.csv")

#Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
#Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')

access_token <- get_spotify_access_token()

#username <- "***************"
# 
 #my_scrobble <- download_scrobbles(username = username, api_key = "*****************")
# 
# saveRDS(my_scrobble, str_c("./user-dat/", username, ".csv"))

getTrackUID <- function(song_title, artist){
    glimpse(song_title)
    glimpse(artist)
    # songString <- songInfo$song_title
    # artistString <- songInfo$artist
    #print(str_c(song_title, artist))
    UIDS <- c()
    
    for(i in seq.int(1, length(song_title))){
        # print(i)
        # print(song_title[i])
        # print(artist[i])
        a_UID <- search_spotify(song_title[i], type = "track") %>% 
            dplyr::select(artists, id, name) %>% 
            rename(UID = id, song_name = name) %>% 
            unnest(cols = artists) %>% 
            dplyr::select(song_name, name, UID) %>% 
            filter(str_replace(tolower(artist[i]), " & ", " and ") == str_replace(tolower(name), " & ", " and ")) %>% 
            filter(str_replace(tolower(song_title[i]), " & ", " and ") == str_replace(tolower(song_name), " & ", " and "))
        UIDS <- append(UIDS, a_UID$UID[1])
        #print(UIDS)
    }

    
    # theTrack <- as.data.frame(search_spotify("Hello", type = "track")) %>% 
    #     dplyr::select(artists, id, name) %>% 
    #     rename(UID = id, song_name = name) %>% 
    #     unnest(cols = artists) %>% 
    #     dplyr::select(song_name, name, UID) %>% 
    #     filter(tolower(artist[i]) == tolower(name)) %>% 
    #     filter(tolower(song_title[i])== tolower(song_name))
        
    return(UIDS)
}

getUIDS <- function(scrobbleDF){
    makeUnique <- scrobbleDF %>% 
        dplyr::select(song_title, artist) 
    
    
    #Remove duplicate songs and convert each row to dataframe
    makeUnique <- unique(makeUnique) #%>%
        # rowwise %>%
        # do(X = as_tibble(.) ) %>%
        # ungroup
    
    makeUnique <- makeUnique %>% 
        dplyr::mutate(UID = getTrackUID(song_title, artist)) 
    
    print("so close")
    scrobbleDF <- left_join(scrobbleDF, makeUnique, by = c("song_title" = "song_title", "artist" = "artist"))
    
    return(scrobbleDF)
}





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Spotify Sentiment Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("username",
                      "Enter username:"),
            passwordInput("key",
                          "Last.FM API Key: "),
            actionButton("go",
                         "View my scrobbles")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          # plotOutput("distPlot")
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
            my_scrobble <- update_scrobbles(last_scrobble,
                             "date_unix",
                             username,
                             key) %>% 
                unique()
        }
        saveRDS(scrobble_UID, str_c("./user-dat/", username, ".csv"))
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application
shinyApp(ui = ui, server = server)
