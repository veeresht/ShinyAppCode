
library(base64enc)
library(twitteR)
library(shiny)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)

load("twitter_tokens")
setup_twitter_oauth(consumer_key, consumer_secret,
                    access_token, access_secret)
sessionInfo()
shinyServer(function(input, output) {

    output$tweetsWordCloud <- renderPlot({
        
        twitter_user_name <- input$user_name
        number_of_tweets <- input$ntweets
        exclude_replies <- !(input$include_replies == "yes")
        include_retweets <- (input$include_retweets == "yes")
        color_palette <- input$color_palette
        
        tweets_list <- userTimeline(twitter_user_name, n = number_of_tweets, 
                                    includeRts = include_retweets,
                                    excludeReplies = exclude_replies)
    
        tweets_df <- twListToDF(tweets_list)
        
        # build a corpus, and specify the source to be character vectors 
        tweets_Corpus <- Corpus(VectorSource(tweets_df$text))
        tweets_Corpus <- tm_map(tweets_Corpus,
                           content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                           mc.cores=1)
        # convert to lower 
        tweets_Corpus <- tm_map(tweets_Corpus, content_transformer(tolower))
        #tweets_Corpus <- tm_map(tweets_Corpus, tolower)
        # remove URLs
        removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
        tweets_Corpus <- tm_map(tweets_Corpus, content_transformer(removeURL)) 
        #tweets_Corpus <- tm_map(tweets_Corpus, removeURL) 
        # remove anything other than english letters or space
        removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
        tweets_Corpus <- tm_map(tweets_Corpus, content_transformer(removeNumPunct))
        #tweets_Corpus <- tm_map(tweets_Corpus, removeNumPunct)
        # remove punctuation
        tweets_Corpus <- tm_map(tweets_Corpus, removePunctuation)
        # remove numbers
        tweets_Corpus <- tm_map(tweets_Corpus, removeNumbers)
        # # add two extra stop words: "available" and "via"
        tweetsStopwords <- c(stopwords('english'), "like", "absolutely", "want", 
                         "ive", "im", "go", "seen", "via", "get", "amp", "will") 
        # remove stopwords from corpus
        tweets_Corpus <- tm_map(tweets_Corpus, removeWords, tweetsStopwords, lazy=TRUE)
        # remove extra whitespace
        tweets_Corpus <- tm_map(tweets_Corpus, stripWhitespace, lazy=TRUE)
        # Keep a copy
        tweets_CorpusCopy <- tweets_Corpus
        # Extract Stem Words
        tweets_Corpus <- tm_map(tweets_Corpus, stemDocument)
        
        stemCompletion2 <- function(x, dictionary) {
            x <- unlist(strsplit(as.character(x), " "))
            # Unexpectedly, stemCompletion completes an empty string to
            # a word in dictionary. Remove empty string to avoid above issue. 
            x <- x[x != ""]
            x <- stemCompletion(x, dictionary=dictionary)
            x <- paste(x, sep="", collapse=" ") 
            PlainTextDocument(stripWhitespace(x))
        }

        tweets_Corpus <- tm_map(tweets_Corpus, content_transformer(stemCompletion2), 
                                dictionary=tweets_CorpusCopy, lazy=TRUE)
        tweets_Corpus <- Corpus(VectorSource(tweets_Corpus))
        tdm <- TermDocumentMatrix(tweets_Corpus, control = list(wordLengths = c(1, Inf)))

        m <- as.matrix(tdm)
        # calculate the frequency of words and sort it by frequency 
        word.freq <- sort(rowSums(m), decreasing = T)
        # colors
        pal <- brewer.pal(9, color_palette)
        pal <- pal[-(1:4)]
        
        # plot word cloud
        wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
                  random.order = F, colors = pal, scale=c(8,.2))
    })
})
