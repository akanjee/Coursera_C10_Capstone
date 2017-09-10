# Name: Coursera Data Scientist Capstone Project - Predictive Text Shiny App
# Author: Amyn Kanjee
# Date: Sept 9th, 2017


library(shiny)
library(ngram)
library(NLP)
library(tm)

# Load the Corpus Quad-Gram Corpus for matching - data has been pre-processed to speed up time
quad_word_freq <- readRDS("quad_word_freq2.rds")


ui <- fluidPage(
    
    h3("Coursera Data Scientist Program - Capstone - Predicitive Word App"),
    ### Setup the Page to take the input and output the potential solutions

    ### Text Input
    h4("Please enter your text here so that the app can predict the next word"),
    textInput('input1',label = "",value = ""),
    
    ### Action Button to Tell the system to generate the results
    #actionButton("go", "Predict Words"),
    
    ### Table to show the Potential Solutions
    #h4("You entered:"),
    #textOutput("InputText"),

    h4("There are this many words in the input"),
    textOutput("WordCount"),
    
    h4("The top 10 predictions are:"),
    tableOutput("PS")
    

    
)

server <- function(input, output) {
    
    ### Show the user what they entered
    #output$InputText <- eventReactive(input$go,input$input1)
    
    ### Determine how many words there are - excluding numbers
    
    #output$WordCount <- eventReactive(input$go,wordcount(removeNumbers(input$input1)))
    output$WordCount <- renderText(wordcount(removeNumbers(input$input1)))
    
    
    ### Determine how many words there are - excluding numbers
    


    
    ### Find the potential solutions as long as there are 3 words in the sentence
    
    #output$PS <- eventReactive(input$go,
    #            {
    #                if (wordcount(removeNumbers(input$input1))>=3) {
    #                   Test <- Potenital_Solutions(input$input1,quad_word_freq)
    #                   Test
    #                    
    #                } else {
    #                    c("You don't have enough words to do the search")
    #                }
    #            }
    #        )


    output$PS <-renderTable(
        {
        
            if (wordcount(removeNumbers(input$input1))>=3) 
                {
                    Question <-input$input1
                    Question <- stripWhitespace(Question)
                    Question <- removePunctuation(Question)
                    Question <- removeNumbers(Question)    
                    
                    # Break the question into individual components and count the words so that we can identify the three words to input
                    Question <- do.call(rbind, strsplit(Question, ' '))
                    Question <- as.character(Question)
                    n <- wordcount(Question)
                    
                    # Identify the last 3 words in the sentence
                    word1 <- Question[n-2]
                    word2 <- Question[n-1]
                    word3 <- Question[n]
                    
                    
                    # Compare the last 3 words to the first 4 words in the N-Gram and display
                    solutions<-subset(quad_wordfreq, quad_wordfreq$one==word1 & quad_wordfreq$two == word2 & quad_wordfreq$three==word3)
                    head(solutions,10)
                }
            
         else 
             {
                c("You don't have enough words to do the search")
            }
    })
}
shinyApp(ui, server)