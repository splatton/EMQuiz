#
# This app will attempt to predict what type of job an EM physician has based on their questionnaire inputs.
#

library(shiny)
library(shinyjs)
library(dplyr)
library(randomForest)

working_data <- read.csv("answerdb.csv", stringsAsFactors = TRUE)
predictor_data <- select(working_data, -Guess)
rf_model <- randomForest(Actual ~ ., data = predictor_data, mtry = 4, ntree = 2000)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    shinyjs::useShinyjs(),

    # Application title
    titlePanel("EM Doctor Job Predictor"),

    # Sidebar for instructions only 
    sidebarLayout(
        sidebarPanel(
            helpText("Toggle the slider bars and click SUBMIT when you are done to see what type of emergency medicine job we think you have! Make sure to tell us if we were right or wrong at the bottom!"),
            br(),
            helpText("Number of responses:", nrow(working_data)),
            br(),
            helpText("Overall accuracy:", mean(working_data$Actual == working_data$Guess)*100, "%")
        ),

        # Main body of quiz
        mainPanel(
            div(id = "quiz_form",
            inputPanel(
                selectInput("admin", "Do you have an administrative role (director or higher)?", choices = c('No', 'Yes')),
                selectInput("gender", "Select your gender:", choices = c('Male', 'Female', 'Other'))
            ),
           inputPanel(
               helpText("For the following questions, 1 = Strongly DISAGREE and 5 = Strongly AGREE."),
               br(),
               sliderInput("q1", "I am happy.", min = 1, max = 5, value = 3, step = 1),
               br(),
               sliderInput("q2", "Emergency medicine is a good career.", min = 1, max = 5, value = 3, step = 1),
               br(),
               sliderInput("q3", "I have lots of opportunities for growth.", min = 1, max = 5, value = 3, step = 1),
               br(),
               sliderInput("q4", "I am confident in my worth.", min = 1, max = 5, value = 3, step = 1),
               br(),
               sliderInput("q5", "I am paid fairly.", min = 1, max = 5, value = 3, step = 1),
               br(),
               sliderInput("q6", "I am valued as an individual.", min = 1, max = 5, value = 3, step = 1),
               br(),
               sliderInput("q7", "I am optimistic about the future of emergency medicine.", min = 1, max = 5, value = 3, step = 1),
               br(),
               sliderInput("q8", "My voice is important.", min = 1, max = 5, value = 3, step = 1),
               br()
           ),
           actionButton("submit_button", "SUBMIT"),
           br(),
           br()
            ),
           shinyjs::hidden(
               div(id = "displayguess",
                   wellPanel(
               helpText("Based on your answer, we predict your job is:"),
               textOutput("final_answer"),
               br(),
               br(),
               actionButton("correct_answer", "You got it right!"),
               actionButton("wrong_answer", "You got it wrong.")
                   )
               )
           ),
           shinyjs::hidden(
               div(id = "newanswer",
                   wellPanel(
               helpText("Select the best description of your actual job:"),
               br(),
               actionButton("actual_1", textOutput("unguessed1")),
               actionButton("actual_2", textOutput("unguessed2")),
               actionButton("actual_3", textOutput("unguessed3"))
           )
               )
           ),
           shinyjs::hidden(
               div(id = "thanks_message",
                   wellPanel(
                       helpText("Thanks for your help! Please share this application for the benefit of emergency physicians. You may close this window.")
                   ))
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    new_dataframed <- eventReactive(input$submit_button, {
        input_for_prediction <- data.frame(Happy = input$q1,
                   Career = input$q2,
                   Growth = input$q3,
                   Worth = input$q4,
                   Fairly = input$q5,
                   Valued = input$q6,
                   Optimistic = input$q7,
                   Voice = input$q8,
                   Gender = input$gender,
                   Admin = input$admin,
                   Actual = NA,
                   Guess = NA)
        input_for_prediction[,"Gender"] <- as.factor(input_for_prediction[,"Gender"])
        input_for_prediction[,"Admin"] <- as.factor(input_for_prediction[,"Admin"])
        input_for_prediction[,"Actual"] <- as.factor(input_for_prediction[,"Actual"])
        input_for_prediction[,"Guess"] <- as.factor(input_for_prediction[,"Guess"])
        joined_input <- rbind(input_for_prediction, working_data)
        joined_input[1,"Guess"] <- predict(rf_model, newdata = joined_input[1,], type = "response")[[1]]
        joined_input
    })
    
    observeEvent(input$submit_button, {
        shinyjs::toggle(id = "quiz_form", anim = TRUE)
        shinyjs::toggle(id = "displayguess", anim = TRUE)
    })
    
    output$final_answer <- eventReactive(input$submit_button, {
        new_dataframed()[1,"Guess"]
    })
    
    output$unguessed1 <- reactive({
        temp_unguessed <- unique(as.character(new_dataframed()$Guess))
        temp_unguessed <- temp_unguessed[!temp_unguessed %in% as.character(new_dataframed()[1,"Guess"])]
        temp_unguessed[1]
    })
    
    output$unguessed2 <- reactive({
        temp_unguessed <- unique(as.character(new_dataframed()$Guess))
        temp_unguessed <- temp_unguessed[!temp_unguessed %in% as.character(new_dataframed()[1,"Guess"])]
        temp_unguessed[2]
    })
    
    output$unguessed3 <- reactive({
        temp_unguessed <- unique(as.character(new_dataframed()$Guess))
        temp_unguessed <- temp_unguessed[!temp_unguessed %in% as.character(new_dataframed()[1,"Guess"])]
        temp_unguessed[3]
    })
    
    observeEvent(input$wrong_answer, {
        shinyjs::toggle(id = "newanswer", anim = TRUE)
        shinyjs::toggle(id = "displayguess", anim = TRUE)
    })
    
    observeEvent(input$correct_answer, {
        temp_newdata <- new_dataframed()
        temp_newdata[1,"Actual"] <- temp_newdata[1,"Guess"]
        write.csv(temp_newdata, "answerdb.csv", row.names = FALSE)
        new_dataframed <- data.frame()
        shinyjs::toggle(id = "displayguess", anim = TRUE)
        shinyjs::toggle(id = "thanks_message", anim = TRUE)
    })
    
    observeEvent(input$actual_1, {
        temp_newdata <- new_dataframed()
        temp_unguessed <- unique(as.character(new_dataframed()$Guess))
        temp_unguessed <- temp_unguessed[!temp_unguessed %in% as.character(new_dataframed()[1,"Guess"])]
        temp_newdata[1,"Actual"] <- temp_unguessed[1]
        write.csv(temp_newdata, "answerdb.csv", row.names = FALSE)
        new_dataframed <- data.frame()
        shinyjs::toggle(id = "newanswer", anim = TRUE)
        shinyjs::toggle(id = "thanks_message", anim = TRUE)
    })
    
    observeEvent(input$actual_2, {
        temp_newdata <- new_dataframed()
        temp_unguessed <- unique(as.character(new_dataframed()$Guess))
        temp_unguessed <- temp_unguessed[!temp_unguessed %in% as.character(new_dataframed()[1,"Guess"])]
        temp_newdata[1,"Actual"] <- temp_unguessed[2]
        write.csv(temp_newdata, "answerdb.csv", row.names = FALSE)
        new_dataframed <- data.frame()
        shinyjs::toggle(id = "newanswer", anim = TRUE)
        shinyjs::toggle(id = "thanks_message", anim = TRUE)
    })
    
    observeEvent(input$actual_3, {
        temp_newdata <- new_dataframed()
        temp_unguessed <- unique(as.character(new_dataframed()$Guess))
        temp_unguessed <- temp_unguessed[!temp_unguessed %in% as.character(new_dataframed()[1,"Guess"])]
        temp_newdata[1,"Actual"] <- temp_unguessed[3]
        write.csv(temp_newdata, "answerdb.csv", row.names = FALSE)
        new_dataframed <- data.frame()
        shinyjs::toggle(id = "newanswer", anim = TRUE)
        shinyjs::toggle(id = "thanks_message", anim = TRUE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
