#
# This app will attempt to predict what type of job an EM physician has based on their questionnaire inputs.
#

library(shiny)
library(shinyjs)
library(dplyr)
library(randomForest)
library(googledrive)

drive_auth(cache = ".secret")

outputDir <- "Responses"

saveData <- function(data) {
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    write.csv(
        x = data,
        file = file.path(outputDir, fileName), 
        row.names = FALSE, quote = TRUE
    )
}

loadData <- function() {
    # Read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = TRUE) 
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
}

working_data <- loadData()
owner_predictor_data <- working_data %>%
    select(-Guess.Ownership) %>%
    select(-Actual.Structure) %>%
    select(-Guess.Structure)
owner_model <- randomForest(Actual.Ownership ~ ., data = owner_predictor_data, ntree = 2000)
structure_predictor_data <- working_data %>%
    select(-Guess.Structure)
structure_model <- randomForest(Actual.Structure ~ ., data = structure_predictor_data, ntree = 2000)

# Define UI for application
ui <- fluidPage(
    
    shinyjs::useShinyjs(),

    # Application title
    titlePanel("EM Doctor Job Predictor Quiz"),

    # Sidebar for instructions only 
    sidebarLayout(
        sidebarPanel(
            helpText("Toggle the slider bars and click SUBMIT when you are done to see what type of emergency medicine job we think you have! Make sure to tell us if we were right or wrong at the bottom!"),
            br(),
            helpText("Number of responses:", nrow(working_data)),
            br(),
            helpText("Overall accuracy:", (mean(working_data$Actual.Ownership == working_data$Guess.Ownership)+mean(working_data$Actual.Structure == working_data$Guess.Structure))*50, "%")
        ),

        # Main body of quiz
        mainPanel(
            div(id = "quiz_form",
            inputPanel(
                selectInput("admin", "Do you have an administrative role (director or higher)?", choices = c('No', 'Yes')),
                selectInput("gender", "Select your gender:", choices = c('Male', 'Female', 'Other'))
            ),
           inputPanel(column(12,
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
               br(),
               sliderInput("q9", "I am secure in my career.", min = 1, max = 5, value = 3, step = 1),
               br()
           )),
           br(),
           actionButton("submit_button", "SUBMIT"),
           br(),
           br()
            ),
           shinyjs::hidden(
               div(id = "displayguess",
                   wellPanel(
               helpText("Based on your answers, we predict that - between employee or owner - you are an:"),
               textOutput("owner_answer"),
               br(),
               br(),
               div(id = "ownerguess",
               actionButton("correct_answer", "You got it right!"),
               actionButton("wrong_answer", "You got it wrong.")
               ),
               shinyjs::hidden(
               div(id = "structureguess",
                   helpText("Based on your answers and your job role, we predict your job is:"),
                   textOutput("structure_answer"),
                   br(),
                   br(),
                   actionButton("correct_structure_answer", "You got it right!"),
                   actionButton("wrong_structure_answer", "You got it wrong.")
                   )
               )
                   )
               )
           ),
           shinyjs::hidden(
               div(id = "newanswer",
                   wellPanel(
               selectInput("changedanswer", "Select the best description of your actual job:", choices = unique(as.character(working_data$Actual.Structure))),
               br(),
               actionButton("submit_change", "SUBMIT")
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

# Define server logic
server <- function(input, output) {

    v <- reactiveValues()
    v$initial <- data.frame()
    v$full <- data.frame()
    
    observeEvent(input$submit_button, {
        v$initial <- data.frame(Happy = input$q1,
                              Career = input$q2,
                              Growth = input$q3,
                              Worth = input$q4,
                              Fairly = input$q5,
                              Valued = input$q6,
                              Optimistic = input$q7,
                              Voice = input$q8,
                              Secure = input$q9,
                              Gender = input$gender,
                              Admin = input$admin,
                              Actual.Ownership = NA,
                              Guess.Ownership = NA,
                              Actual.Structure = NA,
                              Guess.Structure = NA)
        v$initial[,"Gender"] <- as.factor(v$initial[,"Gender"])
        v$initial[,"Admin"] <- as.factor(v$initial[,"Admin"])
        v$initial[,"Actual.Ownership"] <- as.factor(v$initial[,"Actual.Ownership"])
        v$initial[,"Guess.Ownership"] <- as.factor(v$initial[,"Guess.Ownership"])
        v$initial[,"Actual.Structure"] <- as.factor(v$initial[,"Actual.Structure"])
        v$initial[,"Guess.Structure"] <- as.factor(v$initial[,"Guess.Structure"])
        v$full <- rbind(v$initial, working_data)
        v$full[1,"Guess.Ownership"] <- predict(owner_model, newdata = v$full[1,], type = "response")[[1]]
    })
    
    observeEvent(input$submit_button, {
        shinyjs::toggle(id = "quiz_form", anim = TRUE)
        shinyjs::toggle(id = "displayguess", anim = TRUE)
    })
    
    output$owner_answer <- eventReactive(input$submit_button, {
        v$full[1,"Guess.Ownership"]
    })
    
    #This next part handles answers to the ownership question.
    
    observeEvent(input$wrong_answer, {
        shinyjs::toggle(id = "ownerguess", anim = TRUE)
        shinyjs::toggle(id = "structureguess", anim = TRUE)
        v$full[1,"Actual.Ownership"] <- levels(v$full$Guess.Ownership)[!levels(v$full$Guess.Ownership) %in% v$full[1,"Guess.Ownership"]]
        v$full[1,"Guess.Structure"] <- predict(structure_model, newdata = v$full[1,], type = "response")[[1]]
    })
    
    observeEvent(input$correct_answer, {
        shinyjs::toggle(id = "ownerguess", anim = TRUE)
        shinyjs::toggle(id = "structureguess", anim = TRUE)
        v$full[1,"Actual.Ownership"] <- v$full[1,"Guess.Ownership"]
        v$full[1,"Guess.Structure"] <- predict(structure_model, newdata = v$full[1,], type = "response")[[1]]
    })
    
    #This next part takes the output from the ownership question and uses it to guess the employment structure.
    
    output$structure_answer <- reactive(v$full[1,"Guess.Structure"])
    
    #This part handles the user input with regards to employment structure.
    
    observeEvent(input$correct_structure_answer, {
        v$full[1,"Actual.Structure"] <- v$full[1,"Guess.Structure"]
        temp_frame <- as_tibble(v$full)
        saveData(temp_frame[1,])
        shinyjs::toggle(id = "displayguess", anim = TRUE)
        shinyjs::toggle(id = "thanks_message", anim = TRUE)
    })
    
    observeEvent(input$wrong_structure_answer, {
        shinyjs::toggle(id = "displayguess", anim = TRUE)
        shinyjs::toggle(id = "newanswer", anim = TRUE)
    })
    
    observeEvent(input$submit_change, {
        v$full[1,"Actual.Structure"] <- input$changedanswer
        temp_frame <- as_tibble(v$full)
        saveData(temp_frame[1,])
        shinyjs::toggle(id = "newanswer", anim = TRUE)
        shinyjs::toggle(id = "thanks_message", anim = TRUE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
