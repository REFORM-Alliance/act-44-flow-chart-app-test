library(shiny)

# Define decision tree structure
questions <- list(
  "start" = list(
    question = "Do you like working with data?",
    choices = c("Yes", "No"),
    step_next = c("Yes" = "analysis", "No" = "creative")
  ),
  "analysis" = list(
    question = "Do you prefer coding?",
    choices = c("Yes", "No"),
    step_next = c("Yes" = "data_scientist", "No" = "data_analyst")
  ),
  "creative" = list(
    question = "Do you enjoy design?",
    choices = c("Yes", "No"),
    step_next = c("Yes" = "graphic_designer", "No" = "writer")
  ),
  "data_scientist" = "You might be a Data Scientist!",
  "data_analyst" = "You might be a Data Analyst!",
  "graphic_designer" = "You might be a Graphic Designer!",
  "writer" = "You might be a Writer!"
)

ui <- fluidPage(
  titlePanel("Decision Tree Guide"),
  mainPanel(
    uiOutput("question_ui"),
    uiOutput("result_ui")
  )
)

server <- function(input, output, session) {
  current_question <- reactiveVal("start")
  
  observeEvent(input$answer, {
    next_step <- questions[[current_question()]]$step_next[[input$answer]]
    if (!is.null(questions[[next_step]]$question)) {
      current_question(next_step)
    } else {
      current_question(next_step)
    }
  })
  
  output$question_ui <- renderUI({
    q <- questions[[current_question()]]
    if (is.list(q)) {
      tagList(
        h3(q$question),
        radioButtons("answer", "Choose one:", choices = q$choices),
        actionButton("next", "Next")
      )
    } else {
      NULL
    }
  })
  
  output$result_ui <- renderUI({
    if (!is.list(questions[[current_question()]])) {
      h3(questions[[current_question()]])
    }
  })
}

shinyApp(ui, server)