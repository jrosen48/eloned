
get_replies <-
  function(df) {
    processed_df <- process_tweets(dd)
    
    replies <-
      dplyr::filter(
        processed_df,
        .data$is_reply
      )
    
    replies <-
      dplyr::select(replies,
                    sender = .data$screen_name,
                    receiver = .data$reply_to_screen_name,
                    date_time = .data$created_at
      )
    
    if (nrow(replies) == 0) {
      replies <-
        dplyr::mutate(replies,
                      sender = as.character(.data),
                      receiver = as.character(.data)
        )
    }
    
    replies <-
      dplyr::mutate(replies,
                    edge_type = "reply"
      )
    
    replies
  }

get_retweets <-
  function(df) {
    processed_df <- process_tweets(df)
    
    RTs <-
      dplyr::filter(
        processed_df,
        .data$is_retweet
      )
    
    RTs <-
      dplyr::select(RTs,
                    sender = .data$screen_name,
                    receiver = .data$retweet_screen_name,
                    date_time = .data$created_at
      )
    
    if (nrow(RTs) == 0) {
      RTs <- dplyr::mutate(RTs,
                           sender = as.character(.data),
                           receiver = as.character(.data)
      )
    }
    
    RTs <-
      dplyr::mutate(RTs,
                    edge_type = "retweet"
      )
    
    RTs
  }

get_quotes <-
  function(df) {
    processed_df <- process_tweets(df)
    
    quotes <-
      dplyr::filter(
        processed_df,
        .data$is_quote
      )
    
    quotes <-
      dplyr::select(quotes,
                    sender = .data$screen_name,
                    receiver = .data$quoted_screen_name,
                    date_time = .data$created_at
      )
    
    if (nrow(quotes) == 0) {
      quotes <-
        dplyr::mutate(quotes,
                      sender = as.character(.data),
                      receiver = as.character(.data)
        )
    }
    
    quotes <-
      dplyr::mutate(quotes,
                    edge_type = "quote-tweet"
      )
    
    quotes
  }

get_mentions <-
  function(df) {
    processed_df <- process_tweets(df)
    unnested_df <- tidyr::unnest(
      data = processed_df,
      cols = .data$mentions_screen_name
    )
    
    mentions <-
      dplyr::select(unnested_df,
                    sender = .data$screen_name,
                    receiver = .data$mentions_screen_name,
                    date_time = .data$created_at
      )
    
    mentions <-
      dplyr::filter(
        mentions,
        !is.na(.data$receiver)
      )
    
    if (nrow(mentions) == 0) {
      mentions <- dplyr::mutate(mentions,
                                sender = as.character(.data),
                                receiver = as.character(.data)
      )
    }
    
    mentions <-
      dplyr::mutate(mentions,
                    edge_type = "mention"
      )
    
    mentions
  }

create_edgelist <-
  function(df) {
    processed_df <- process_tweets(df)
    
    if (!is.list(processed_df$mentions_screen_name)) {
      processed_df$mentions_screen_name <-
        stringr::str_split(processed_df$mentions_screen_name, " ")
    }
    
    reply_edges <- get_replies(processed_df)
    retweet_edges <- get_retweets(processed_df)
    quote_edges <- get_quotes(processed_df)
    mention_edges <- get_mentions(processed_df)
    
    full_edgelist <-
      tibble::tibble(
        sender = character(),
        receiver = character(),
        edge_type = character()
      )
    
    full_edgelist <-
      dplyr::bind_rows(
        full_edgelist,
        reply_edges,
        retweet_edges,
        quote_edges,
        mention_edges
      )
    
    full_edgelist
  }
