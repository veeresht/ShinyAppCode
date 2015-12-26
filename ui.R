
library(shiny)
library(RColorBrewer)

shinyUI(fluidPage(

  # Application title
  titlePanel("Tweets Word Cloud Generator"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        p("This a Shiny Application to visualize any 
          Twitter user's tweets using a word cloud."),
        
        tags$ul(
            tags$li("Enter a valid Twitter username."),
            tags$li("Choose the among various options 
                    available to customize the wordcloud.")
        ),
        
        textInput("user_name", "Twitter UserName", "benhamner"),
        sliderInput("ntweets", "Number of Previous Tweets",
                    min = 100,
                    max = 1000,
                    step = 100,
                    value = 100),
        selectInput("include_retweets", "Include Re-tweets?", c("Yes", "No"), "No"),
        selectInput("include_replies", "Include Replies?", c("Yes", "No"), "No"),
        selectInput("color_palette", "Color Palette", rownames(brewer.pal.info), "YlOrRd")
        
    ),

    # Show a plot of the generated wordcloud
    mainPanel(
        plotOutput("tweetsWordCloud") 
    )
  )
))
