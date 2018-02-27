shinyUI(fluidPage(theme = shinytheme("darkly"),
        useShinyjs(),
        tags$div(
        titlePanel(div("Markov Chain and N-Grams Word Prediction - Data Science Capstone Project", align = "center")),
                navbarPage("Data Science Capstone Project",
                        tabPanel("Perform Your Prediction",
                                p(strong("1. Enter your text below."), align = "center", style = "font-size: 20px; margin-top: 80px"),
                                p(strong("2. Press the space bar to receive the predicted words."), align = "center", style = "font-size: 20px"),
                                p(strong("3. Add the word to the text just clicking on it."), align = "center", style = "font-size: 20px"),
                                tags$div(align = "center", 
                                        textInput("text", label = "", width = '85%'),
                                        tags$style("#text {font-size: 20px}"),
                                        uiOutput("wordOneButton", inline = TRUE),
                                        uiOutput("wordTwoButton", inline = TRUE),
                                        uiOutput("wordThreeButton", inline = TRUE)
                                        # add the signature
                                        )
                        ),
                        tabPanel("About the Application",
                                 div(style = "margin: 25px 200px 0px 200px",
                                 p("The application has been built for the final Capstone project of the Data Science Specialization provided 
                                        by the Johns Hopkins University through Coursera with the cooperation of SwiftKey.", 
                                   style = "font-size: 20px"),
                                 p("The aim of the application is text word prediction.", 
                                   style = "font-size: 20px; margin-bottom: 50px"),
                                 h3("NPL Modelling")),
                                 div(style = "margin: 25px 200px 0px 200px; font-size: 20px",
                                 p("This application was realized starting from 3 raw textual files downloaded from twitter,
                                   from some blogs and news websites."),
                                 p("In the very first moment, the data has been accurately cleaned. Then it was splitted in sub-sentences
                                   where the delimitor was the punctuation. At this point was created the Corpus and the data was consequently 
                                   processed to extract the document term matrix for the n-grams. At this point we calculated the frequency.
                                   Once the frequency was obtained we used the Markov chain model below to calculate the predicted word:"),
                                 headerPanel(withMathJax("$$P(w_i|w_1 w_2 ... w_{i-1})\\approx P(w_i|w_{i-1}... _{i-n})$$")),
                                 p("In this case we have been consider a maximum of 5-grams. The algorithm created works in the following way:"),
                                 p("1. It does some pre-cleaning of the text entered (i.e. convert to lower cases, substitute numbers with text"),
                                 p("2. If the sentence is composed by at least 4 words, it checks in the 5-grams database."),
                                 p("3. If there are no matches, it checks in the 4-grams database and so on until reach the 2-grams database"),
                                 p("4. If there are no matches even in the 2-grams database, it generates a random word from the first 1 thousand
                                   most common words in our original dataset.", style = "margin-bottom: 50px"),
                                 h3("Complete Analysis and further information")),
                                 div(style = "margin: 25px 200px 0px 200px; font-size: 20px",
                                 p("Find the complete analysis here: "),
                                 p("You can reach and deploy the application downloading the following repo: "),
                                 p("A small presentation was done here: ", style = "margin-bottom: 50px"),
                                 h3("What we did not cover")),
                                 div(style = "margin: 25px 200px 0px 200px; font-size: 20px",
                                 p("We do not deal with punctuation in the text entered;"),
                                 p("We do not perform any text correction on mispelled words;"),
                                 p("We do not perform single word prediction."))
                                 
                        )
                )
     
        )
))
