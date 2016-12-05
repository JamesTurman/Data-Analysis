library(shiny)
library(tm)
library(wordcloud)
library(twitteR)
shinyServer(function(input, output, session) {
  
  setup_twitter_oauth(consumer_key = "TyBYUZVmp9kK0RuOO7gWM67S5", 
                      consumer_secret = "z06LS7pPIPtkPc3suUDhgsqTuwVePVaayAf6uejMkk2n0uszy8")
  
  token <- get("oauth_token", twitteR:::oauth_cache)
  token$cache()
  
  output$currentTime <- renderText({invalidateLater(1000, session) 
    paste("Current time is: ",Sys.time())})
  
  observe({
    
    invalidateLater(60000,session)
    
    count_positive = 0
    count_negative = 0
    count_neutral = 0
    
    positive_text <- vector()
    negative_text <- vector()
    neutral_text <- vector()
    
    vector_users <- vector()
    vector_sentiments <- vector()
    
    tweets_result = ""
    
    tweets_result = searchTwitter("college")
    for (tweet in tweets_result){
      print(paste(tweet$screenName, ":", tweet$text))
      
      vector_users <- c(vector_users, as.character(tweet$screenName));
      
      if (grepl("Good", tweet$text, ignore.case = TRUE) == TRUE | 
        grepl("fun", tweet$text, ignore.case = TRUE) | grepl("Awesome", tweet$text, ignore.case = TRUE) |
        grepl("great", tweet$text, ignore.case=TRUE)| grepl("win",tweet$text,ignore.case=TRUE)){
        count_positive = count_positive + 1
        
        vector_sentiments <- c(vector_sentiments, "Positive")
        positive_text <- c(positive_text, as.character(tweet$text))
        
      } else if (grepl("Boring", tweet$text, ignore.case = TRUE) | grepl("lame", tweet$text, ignore.case = TRUE)
                 | grepl("lose",tweet$text,ignore.case=TRUE) | grepl("lost",tweet$text,ignore.case=TRUE)) { 
        count_negative = count_negative + 1
        
        vector_sentiments <- c(vector_sentiments, "Negative")
        negative_text <- c(negative_text, as.character(tweet$text))
        
      } else {
        count_neutral = count_neutral + 1
        print("neutral")
        vector_sentiments <- c(vector_sentiments, "Neutral")
        neutral_text <- c(neutral_text, as.character(neutral_text))
      }
    }
    
    df_users_sentiment <- data.frame(vector_users, vector_sentiments)
    
    output$tweets_table = renderDataTable({
      df_users_sentiment
    })
    
    output$distPlot <- renderPlot({
      
      results = data.frame(tweets = c("Positive", "Negative", "Neutral"), 
                           numbers = c(count_positive,count_negative,count_neutral))
      
      barplot(results$numbers, names = results$tweets, xlab = "Sentiment", ylab = "Counts", 
              col = c("Green","Red","Blue"))
      
      if (length(positive_text) > 0){
        
        output$positive_wordcloud <- renderPlot({ wordcloud(paste(positive_text, collapse=" "), 
        random.color=TRUE, min.freq = 0,  max.words=10 ,colors=brewer.pal(8, "Dark2")) }) 
      }
      
      if (length(negative_text) > 0) {
        
        output$negative_wordcloud <- renderPlot({ wordcloud(paste(negative_text, collapse=" "), 
        random.color=TRUE, min.freq = 0, max.words=10 ,colors=brewer.pal(8,"Set3")) }) 
      }
      
      
      if (length(neutral_text) > 0){
        
        output$neutral_wordcloud <- renderPlot({ wordcloud(paste(neutral_text, collapse=" "), 
       min.freq = 0, random.color=TRUE , max.words=10 ,colors=brewer.pal(8, "Dark2")) }) 
      }
      
    })
  })
})