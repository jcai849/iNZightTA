#####################################################
#################### Raw Data    ####################
#####################################################

# This file contains the server code that 
# imports the raw text data from the APIs/XML URL 
# (token validation stuff in here and tidying up the data frames)


twitter_token <- eventReactive(input$get_twitter_token,
                               {
                                 create_token(
                                   app = input$appname,
                                   consumer_key = input$key,
                                   consumer_secret = input$secret,
                                   access_token = input$access_token,
                                   access_secret = input$access_secret)
                               })

raw_data <- eventReactive(input$gather_data, {
  
  if (input$import_from == "Twitter"){
    if (input$type == "user2") {
      ###########################
        withCallingHandlers({
          shinyjs::html(id = "text", html = "")
          tweets <- get_timeline(input$user,
                                 n = input$num_tweets, token = twitter_token())
        },
        message = function(m) {
          shinyjs::html(id = "text", html = m$message, add = TRUE)
        },
        warning = function(m) {
          shinyjs::html(id = "text", html = m$message, add = TRUE)
        })
      
      ###########################
      
      if (input$include_retweets == FALSE){
        tweets <- tweets %>% filter(is_retweet == FALSE)
      }
    }
    
    else if (input$type == "hashtag") {
      tweets <- search_tweets(input$user, lang = "en",
                              n = input$num_tweets, include_rts = input$include_retweets,
                              token = twitter_token())
    }
    tweets <- tweets %>% select(status_id, text, is_retweet, hashtags, mentions_screen_name)
    tweets$mentions_screen_name <- unlist(lapply(tweets$mentions_screen_name, paste, collapse = " "))
    tweets$hashtags <- unlist(lapply(tweets$hashtags, paste, collapse = " "))
    tweets <- tweets %>% rename(id = status_id)
    tweets
  }
  
  else if (input$import_from == "Project Gutenberg"){
    chosen_title <- unlist(lapply(str_split(input$gutenberg_work, " :by: "), function(x){x[1]}))
    id = numeric(0)
    
    for (i in 1:length(chosen_title)){
      id_list <- gutenberg_metadata %>%
        filter(title %in% chosen_title[i]) %>%
        select(gutenberg_id)
      id <- c(id, id_list$gutenberg_id[1])
    }
    
    gutenberg_metadata %>% filter(gutenberg_id %in% id) -> title_inf
    
    downloaded_text <- gutenberg_download(id, mirror = "http://aleph.gutenberg.org")
    downloaded_text <- left_join(downloaded_text, title_inf, by = "gutenberg_id") %>%
      select(text, title, author)
    names(downloaded_text)[names(downloaded_text) == "title"] <- "id"
    downloaded_text
  }
  
  else if (input$import_from == "Upload .txt, .csv, .xlsx, or .xls file"){
    inzightta::import_files(input$file1$datapath, input$file1$name)# %>%
      #mutate(id = basename(input$file1$name))
  }
  
  else if (input$import_from == "Spotify/Genius"){
    
    spotify_access_token <- eventReactive(input$get_spot_token, {
      get_spotify_access_token(input$spotify_id, input$spotify_secret)
    })
    
    GENIUS_API_TOKEN <- eventReactive(input$get_spot_token,
                                      {
                                        input$genius_api_token
                                      })
    
    ##### returns different dfs based on user input
    lyrics_with_audio = reactive({
      #####
      if (input$type_spotify == "songs") {
        song_tib <- tibble(title = character(0), line = character(0), text = character(0))
        
        singers <- reactive({trimws(unlist(strsplit(input$artist, split = ",")))})
        songs <- reactive({trimws(unlist(strsplit(input$song_title, split = ",")))})
        
        for (i in 1:length(singers())){
          song_tib <- rbind(song_tib, get_lyrics(singers()[i], songs()[i]))
        }
        
        names(song_tib)[names(song_tib) == "title"] <- "id"
        
        v <- vector(mode = "list", length = 2)
        v[[1]] <- song_tib
        v[[2]] <- NULL
        v
      }
      
      else if (input$type_spotify == "album"){
        albums <- reactive({trimws(unlist(strsplit(input$album_title, split = ",")))})
        df <- get_album_data(input$artist, albums(), authorization = get_spotify_access_token(input$spotify_id, input$spotify_secret)) %>%
          filter(!is.null(lyrics)) %>%
          unnest(lyrics)
        
        names(df)[names(df) == "lyric"] <- "text"
        
        df <- df %>%
          select(valence, mode_name, album_name, track_name, line, text) %>%
          rename(id = track_name)
        
        df$valence <- cut(df$valence, 5, labels = FALSE)
        
        v <- vector(mode = "list", length = 2)
        v[[1]] <- df
        v[[2]] <- NULL
        v
      }
      
      else{
        playlist_audio <- get_playlist_audio_features(input$spotify_username, input$playlist_id,
                                                      authorization = spotify_access_token())
        
        # artists = character(0)
        # for (i in 1:nrow(playlist_audio)){
        #   artists[i] <- playlist_audio[[44]][[i]]$name[1]
        # }
        # 
        # playlist_audio$artist <- artists
        
        playlist_audio$artist <- unlist(lapply(playlist_audio[[44]], function(x) x[["name"]][1]))
        
        ##### Fix track name
        playlist_audio <- playlist_audio %>%
          select(artist, track.name, valence, mode_name) %>%
          mutate(track.name = str_replace_all(track.name,
                                              "[(].+[)]?.|\\[.+\\]|FEAT.+| - .+ Edit", "")) %>%
          mutate(track.name = stri_trans_general(track.name, "Latin-ASCII"))
        
        
        ###### collect the lyrics from Genius
        playlist <- playlist_audio %>%
          add_genius(artist, track.name, type = "lyrics") %>%
          select(-valence, -mode_name)
        
        playlist_merged <- left_join(playlist, playlist_audio, by = c('track.name', 'artist')) %>%
          filter(!is.na(lyric)) %>%
          rename(text = lyric) %>%
          select(valence, mode_name, track_title, artist, line, text) %>%
          rename(id = track_title)
        
        playlist_merged$valence <- cut(playlist_merged$valence, 5, labels = FALSE)
        
        ##### Which songs were not collected?
        not_collected <- playlist_audio[!playlist_audio$track.name %in% playlist$track.name, c("artist", "track.name")]
        
        v <- vector(mode = "list", length = 2)
        v[[1]] <- playlist_merged
        v[[2]] <- not_collected
        v
      }
    })
    
    lyrics_with_audio()
  }
  
  else if (input$import_from == "stuff.co.nz Comments"){
    rss <- reactive({
      
      rss_tibb <- tidyfeed(input$rss_link) %>%
        select(item_description)
      
      names(rss_tibb)[names(rss_tibb) == "item_description"] <- "text"
      
      rss_tibb$comment_id <- 1:nrow(rss_tibb)
      names(rss_tibb)[names(rss_tibb) == "comment_id"] <- "id"
      rss_tibb
    })
    
    rss()
  }
  
  else if (input$import_from == "The Guardian Articles"){
    
    guardian_articles <- reactive({
      if (input$guardian_sections == "all"){
        results <- get_guardian(input$guardian_keywords,
                                from.date = input$guardian_dates[1],
                                to.date = input$guardian_dates[2],
                                api.key= input$guardian_api)
      }
      
      else {
        results <- get_guardian(input$guardian_keywords, section = input$guardian_sections,
                                from.date= input$guardian_dates[1],
                                to.date= input$guardian_dates[2],
                                api.key= input$guardian_api)
      }
      
      results <- results %>%
        select(headline, webPublicationDate, body) %>%
        rename(text = body) %>%
        rename(id = headline)
      
      results
      
    })
    
    guardian_articles()
    
  }
  
  else if (input$import_from == "Reddit"){
    
    if (input$type_reddit == "comments_url"){
      reddit_df <- RedditExtractoR::reddit_content(input$url) %>%
        select(comm_date,comment_score,user,comment,id, structure) %>%
        rename(text = comment)
      
      if (input$nest_level == TRUE){
        reddit_df <- reddit_df %>%
          filter(stringr::str_detect(structure, "_", negate = FALSE)) %>%
          select(-structure)
      }
      
      else {
        reddit_df <- reddit_df %>%
          select(-structure)
      }
      
      reddit_df
    }
    
    else{
      if (input$nest_level == TRUE) {
        getPushshiftDataRecursive(postType = input$type_reddit, title = input$title, q = input$q,
                                  subreddit = input$subreddit, after = input$after, before = input$before,
                                  nest_level = 1)
      }
      else {
        getPushshiftDataRecursive(postType = input$type_reddit, title = input$title, q = input$q,
                                  subreddit = input$subreddit, after = input$after, before = input$before)
      }
    }
    
  }
  
})


observeEvent(input$gather_data, {
  output$imported_show <- DT::renderDataTable({
    if (input$import_from == "Spotify/Genius"){raw_data()[[1]]}
    else {raw_data()}
  })
})

output$downloadData_imported <- downloadHandler(
  filename = function() {
    paste("raw", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(raw_data(), file, row.names = FALSE)
  }
)

