#####################################################
#################### Raw Data    ####################
#####################################################

# This file contains the server code that 
# imports the raw text data from the APIs/XML URL 
# (token validation stuff in here and tidying up the data frames)


twitter_token <- eventReactive(input$get_twitter_token,
                               {
                                 rtweet::create_token(
                                   app = input$appname,
                                   consumer_key = input$key,
                                   consumer_secret = input$secret,
                                   access_token = input$access_token,
                                   access_secret = input$access_secret)
                               })

raw_data <- eventReactive(input$gather_data, {
  if (input$import_from == "Twitter"){
    if (input$tw_type == "user2") {
      query <- reactive({trimws(unlist(strsplit(input$given_user, split = "%")))})
      q <- query()

      withCallingHandlers({
          shinyjs::html(id = "text", html = "")
          tweets <- rtweet::get_timeline(q,
                                 n = input$num_tweets, token = twitter_token())
          
          # how many tweets collected
          for (i in 1:length(q)){
            message(paste(sum(tweets$screen_name == substring(q[i], 2)), "tweets collected from", q[i]))
          }

        },
        message = function(m) {
          shinyjs::html(id = "text", html = m$message, add = TRUE)
        },
        warning = function(m) {
          shinyjs::html(id = "text", html = m$message, add = TRUE)
      })
      ###########################
      
      if (input$include_retweets == FALSE){
        tweets <- tweets %>% dplyr::filter(is_retweet == FALSE)
      }
      
      tweets <- tweets %>% dplyr::select(screen_name, status_id, text, is_retweet, 
                                         hashtags, mentions_screen_name, created_at) %>% 
        dplyr::group_by(screen_name) %>%
        dplyr::arrange(created_at) %>%
        dplyr::ungroup()
    }
    
    else {
      query <- reactive({trimws(unlist(strsplit(input$given_hashtag, split = "%")))})
      q <- query()
      withCallingHandlers({
        shinyjs::html(id = "text", html = "")
    
          tweets <- rtweet::search_tweets2(q, 
                                  n = input$num_tweets, include_rts = input$include_retweets,
                                  token = twitter_token(), lang = "en")
          tweets <- tweets %>% dplyr::select(screen_name, status_id, text,
                                             is_retweet, hashtags, mentions_screen_name, query, created_at) %>%
            dplyr::group_by(query) %>%
            dplyr::arrange(created_at) %>%
            dplyr::ungroup()
      
      # how many collected 
          for (i in 1:length(q)){
            message(paste(sum(tweets$query == q[i]), "tweets collected with hashtag", q[i]))
          }
      },
      message = function(m) {
        shinyjs::html(id = "text", html = m$message, add = TRUE)
      },
      warning = function(m) {
        shinyjs::html(id = "text", html = m$message, add = TRUE)
      })
    }
    tweets$mentions_screen_name <- unlist(lapply(tweets$mentions_screen_name, paste, collapse = " "))
    tweets$hashtags <- unlist(lapply(tweets$hashtags, paste, collapse = " "))
    tweets <- tweets %>% dplyr::rename(id = status_id)
    
    tweets
  }
  
  else if (input$import_from == "Project Gutenberg"){
    chosen_title <- unlist(lapply(stringr::str_split(input$gutenberg_work, " :by: "), function(x){x[1]}))
    id = numeric(0)
    
    for (i in 1:length(chosen_title)){
      id_list <- gutenbergr::gutenberg_metadata %>%
        dplyr::filter(title %in% chosen_title[i]) %>%
        dplyr::select(gutenberg_id)
      id <- c(id, id_list$gutenberg_id[1])
    }
    
    gutenbergr::gutenberg_metadata %>% dplyr::filter(gutenberg_id %in% id) -> title_inf
    
    downloaded_text <- gutenbergr::gutenberg_download(id, mirror = "http://aleph.gutenberg.org")
    downloaded_text <- dplyr::left_join(downloaded_text, title_inf, by = "gutenberg_id") %>%
      dplyr::select(text, title, author)
    names(downloaded_text)[names(downloaded_text) == "title"] <- "id"
    downloaded_text
  }
  
  else if (input$import_from == "Upload .txt, .csv, .xlsx, or .xls file"){
    inzightta::import_files(input$file1$datapath, input$file1$name)# %>%
      #mutate(id = basename(input$file1$name))
  }
  
  else if (input$import_from == "Spotify/Genius"){
    
    spotify_access_token <- eventReactive(input$get_spot_token, {
      spotifyr::get_spotify_access_token(input$spotify_id, input$spotify_secret)
    })
    
    GENIUS_API_TOKEN <- eventReactive(input$get_spot_token,
                                      {
                                        input$genius_api_token
                                      })
    
    ##### returns different dfs based on user input
    lyrics_with_audio = reactive({
      #####
      if (input$type_spotify == "songs") {
        song_tib <- tibble::tibble(title = character(0), line = character(0), text = character(0))
        
        singers <- reactive({trimws(unlist(strsplit(input$artist, split = "%")))})
        songs <- reactive({trimws(unlist(strsplit(input$song_title, split = "%")))})
        
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
        albums <- reactive({trimws(unlist(strsplit(input$album_title, split = "%")))})
        df <- spotifyr::get_album_data(input$artist, albums(), 
                                       authorization = spotifyr::get_spotify_access_token(input$spotify_id, 
                                                                                          input$spotify_secret)) %>%
          dplyr::filter(!is.null(lyrics)) %>%
          tidyr::unnest(lyrics)
        
        names(df)[names(df) == "lyric"] <- "text"
        
        df <- df %>%
          dplyr::select(valence, mode_name, album_name, track_name, line, text) %>%
          dplyr::rename(id = track_name)
        
        df$valence <- cut(df$valence, 5, labels = FALSE)
        
        v <- vector(mode = "list", length = 2)
        v[[1]] <- df
        v[[2]] <- NULL
        v
      }
      
      else{
        playlist_audio <- spotifyr::get_playlist_audio_features(input$spotify_username, input$playlist_id,
                                                      authorization = spotify_access_token())
        
        
        playlist_audio$artist <- unlist(lapply(playlist_audio[[44]], function(x) x[["name"]][1]))
        
        ##### Fix track name
        playlist_audio <- playlist_audio %>%
          dplyr::select(artist, track.name, valence, mode_name) %>%
          dplyr::mutate(track.name = stringr::str_replace_all(track.name,
                                              "[(].+[)]?.|\\[.+\\]|FEAT.+| - .+ Edit", "")) %>%
          dplyr::mutate(track.name = stringi::stri_trans_general(track.name, "Latin-ASCII"))
        
        
        ###### collect the lyrics from Genius
        playlist <- playlist_audio %>%
          genius::add_genius(artist, track.name, type = "lyrics") %>%
          dplyr::select(-valence, -mode_name)
        
        playlist_merged <- dplyr::left_join(playlist, playlist_audio, by = c('track.name', 'artist')) %>%
          dplyr::filter(!is.na(lyric)) %>%
          dplyr::rename(text = lyric) %>%
          dplyr::select(valence, mode_name, track_title, artist, line, text) %>%
          dplyr::rename(id = track_title)
        
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
      
      rss_tibb <- tidyRSS::tidyfeed(input$rss_link) %>%
        dplyr::select(item_description)
      
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
        results <- GuardianR::get_guardian(input$guardian_keywords,
                                from.date = input$guardian_dates[1],
                                to.date = input$guardian_dates[2],
                                api.key= input$guardian_api)
      }
      
      else {
        results <- GuardianR::get_guardian(input$guardian_keywords, section = input$guardian_sections,
                                from.date= input$guardian_dates[1],
                                to.date= input$guardian_dates[2],
                                api.key= input$guardian_api)
      }
      
      results <- results %>%
        dplyr::select(headline, webPublicationDate, body) %>%
        dplyr::rename(text = body) %>%
        dplyr::rename(id = headline)
      
      results
      
    })
    
    guardian_articles()
    
  }
  
  else if (input$import_from == "Reddit"){
    
    if (input$type_reddit == "comments_url"){
      reddit_df <- RedditExtractoR::reddit_content(input$url) %>%
        dplyr::select(comm_date,comment_score,user,comment,id, structure) %>%
        dplyr::rename(text = comment) %>%
        dplyr::arrange(comm_date)
      
      if (input$nest_level == TRUE){
        reddit_df <- reddit_df %>%
          dplyr::filter(stringr::str_detect(structure, "_", negate = FALSE)) %>%
          dplyr::select(-structure)
      }
      
      else {
        reddit_df <- reddit_df %>%
          dplyr::select(-structure)
      }
      
      reddit_df
    }
    
    else{
      if (input$nest_level == TRUE) {
        ###########################
        withCallingHandlers({
          shinyjs::html(id = "text", html = "")
          getPushshiftDataRecursive(postType = input$type_reddit, title = input$title, q = input$q,
                                    subreddit = input$subreddit, after = input$after, before = input$before,
                                    nest_level = 1)
        },
        message = function(m) {
          shinyjs::html(id = "text", html = m$message, add = TRUE)
        },
        warning = function(m) {
          shinyjs::html(id = "text", html = m$message, add = TRUE)
        })
      }
      else {
        withCallingHandlers({
          shinyjs::html(id = "text", html = "")
          getPushshiftDataRecursive(postType = input$type_reddit, title = input$title, q = input$q,
                                    subreddit = input$subreddit, after = input$after, before = input$before)
        },
        message = function(m) {
          shinyjs::html(id = "text", html = m$message, add = TRUE)
        },
        warning = function(m) {
          shinyjs::html(id = "text", html = m$message, add = TRUE)
        })
      }
    }
    
  }
  
})


observeEvent(input$gather_data, {
  output$imported_show <- DT::renderDataTable({
    if (input$import_from == "Spotify/Genius"){
      DT::datatable(raw_data()[[1]], options = list(paging = TRUE, searching = FALSE))
      }
    else if (input$import_from == "The Guardian Articles") {
      DT::datatable(raw_data(), options = list(paging = TRUE, searching = FALSE))
      }
    else {
      DT::datatable(raw_data(), options = list(paging = TRUE, searching = FALSE))
    }
  })
})


output$downloadData_imported <- downloadHandler(
  filename = function() {
    paste("raw", ".csv", sep = "")
  },
  content = function(file) {
    utils::write.csv(raw_data(), file, row.names = FALSE)
  }
)

