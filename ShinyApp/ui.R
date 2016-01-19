
library(shiny)

# Define UI for application that draws a histogram and options
shinyUI(
  navbarPage("Next Word Prediction Application",
             tabPanel("Application",
                      
                        
                        # Application title.
                        titlePanel("The Next Word"),
                        
                        sidebarLayout(
                          sidebarPanel(
                            textInput("obs", "Enter Your Statement Here:"),
                            
                            helpText("Note: This widget will use this statement to predict the most likely next word."),
                            
                            submitButton("Predict!")
                          ),
                          
                          mainPanel(
                            h4("Most Likely Next Word:"),
                            div(textOutput("result"), style = "color:red")
#                             br(),
#                             h3("The program guessed your word based on the following data:"),
#                             tableOutput("view")
                          )
                        )
                      ),
             tabPanel("Help",
                      titlePanel("How to use the application"),
                      p("This application tries to predict the next word of your sentence."),
                      p("It will filter out from the prediction the badwords you type"),          
                      p("The available choices are the following:")
             )
  )
)