library(shiny)

decision_tree <- list(
  question = "Is the sky blue?",
  yes = list(
    question = "Are there white clouds?",
    yes = "Go outside for a picnic!",
    no = "Maybe stay inside and read."
  ),
  no = list(
    question = "Is it nighttime?",
    yes = "Go stargazing!",
    no = "Do something indoors."
  )
)

ui <- fluidPage(
  titlePanel("Decision Tree App"),
  uiOutput("question_ui"),
  uiOutput("result_ui")
)

server <- function(input, output, session) {
  tree <- decision_tree
  current_node <- reactiveVal(tree)
  
  output$question_ui <- renderUI({
    if (is.list(current_node())) {
      question <- current_node()$question
      radioButtons("answer", question, choices = c("Yes", "No"))
    } else {
      NULL
    }
  })
  
  observeEvent(input$answer, {
    if (input$answer == "Yes") {
      current_node(current_node()$yes)
    } else {
      current_node(current_node()$no)
    }
  })
  
  output$result_ui <- renderUI({
    if (is.character(current_node())) {
      h3(current_node())
    } else {
      NULL
    }
  })
}

shinyApp(ui = ui, server = server)