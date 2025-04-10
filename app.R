rm(list = ls())


####Read in Libraries####
library(shiny)
library(tidyverse)
library(shinythemes)
library(rsconnect)
library(shinyWidgets)


####Write Decision Tree####
decision_tree <- list(
  list(
    intro = TRUE,
    question = tags$div(
      tags$p(strong("Welcome to REFORM Alliance's Act 44 Early Termination tool.")),
      tags$p(HTML(paste0("This tool is designed to help users navigate the legal requirements of ",
                         "<a href='https://www.palegis.us/statutes/unconsolidated/law-information?sessYr=2023&sessInd=0&actNum=44' target='_blank'>Pennsylvania's Act 44</a>, ",
                         "which requires courts to assess people for early termination of probation or the modification of probation conditions.")
      )),
      tags$p("The tool is designed to help navigate what can be a complex law, but it is no substitute for legal advice and is designed for informational purposes only. Please consult a lawyer with any legal questions about your rights under Act 44."),
      tags$p(HTML(paste0("You can find the full text of the law ",
                         "<a href='https://www.palegis.us/statutes/unconsolidated/law-information?sessYr=2023&sessInd=0&actNum=44' target='_blank'>here</a>.")))
    ),
    next_question = "defendant_sentencing_date",
    question_id = "intro_page"
  ),
  list(
    question = "Was the defendant sentenced on or after June 11, 2024?",
    choices = c("Yes", "No"),
    question_id = "defendant_sentencing_date",
    next_question = list(
      "Yes" = "after_june_11_q1",
      "No" = "before_june_11_q1"
    )
  ),
  list(
    question = "Does the defendant's conviction include any of the following: 
    
                1) Crime of Violence OR
                2) Most homicide charges OR 
                3) Most sex offender registration offenses OR 
                4) Some domestic violence OR 
                5) Some stalking charges", 
    choices = c("Yes", "No"),
    question_id = "after_june_11_q1",
    next_question = list(
      "Yes" = "no_act_44_relief_yet_result",
      "No" = "after_june_11_q2"
    )
  ),
  list(
    question = "Did the Court determine any of the following:
    
                1) The underlying offense was a crime of violence, most sex offender registration offenses, some domestic violence or some stalking charges OR
                2) The defendant committed certain technical violations in 6 months prior to eligibility OR
                3) The defendant was convicted of any felony or 1st or 2nd degree misdemeanor while on probation OR
                4) Was the probationer sentenced to probation for an offense under 18 Pa.C.S. § 2701 (relating to simple assault) or 2709.1 (relating to stalking) against any of their family or household members?",
    question_list = list("q1" = "Was the probation convicted of any felony or 1st or 2nd degree misdemeanor while on probation or in custody for the underlying offense?",
                         "q2" = "Was the probationer sentenced to probation for a crime related to sex offender registration?",
                         "q3" = "Was the probation sentence to probation for a crime of violence?",
                         "q4" = "Was the probationer sentenced to probation for an offense under 18 Pa.C.S. § 2701 (relating to simple assault) or 2709.1 (relating to stalking) against any of their family or household members?"),
    choices = c("Yes", "No"),
    question_id = "before_june_11_q1",
    next_question = list(
      "Yes" = "no_act_44_relief_result",
      "No" = "before_june_11_q2"
    )
  ), 
  list(
    question = "Has the defendant completed any of the following: 
    
                1) 50% of their aggregate sentence OR 
                2) Two years on misdemeanor probation OR
                3) Four years on felony probation or misdemeanor probation based on mulitple distinct misdemeanor convictions with consecutive sentences
                
                MINUS any of the following: 
                
                1) 12 months if the probation sentence was consecutive to state prison sentence and person did not violate parole in their last 12  or more months OR
                2) 6 months for any educational achievement or other approved benchmark (can receive twice if on felony probation)
    
                AND completed a year of probation?",
    choices = c("Yes", "No"),
    question_id = "after_june_11_q2",
    next_question = list(
      "Yes" = "after_june_11_q3",
      "No" = "no_act_44_relief_yet_result"
    )
  ),
  list(
    question = "Is it June 11, 2025 or later, AND has the defendant completed at least 2 years on misdemeanor probation or 4 years on felony probation?",
    choices = c("Yes", "No"),
    question_id = "before_june_11_q2",
    next_question = list(
      "Yes" = "before_june_11_q2_mandate_result",
      "No" = "no_act_44_relief_yet_result"
    )
  ),
  # list(
  #   question = "Is it June 11, 2025 or later, AND has the defendant completed at least 2 years on misdemeanor probation or 4 years on felony probation?",
  #   question_list = list("q1" = "Did the conviction or convictions that led to probation include any felonies?",
  #                        "q2" = "When was the probationer sentenced to probation? If there were multiple sentencing dates, use the date that was first in time"),
  #   choices = c("Yes", "No"),
  #   date_question = "Yes",
  #   date_question_n = "q2", 
  #   question_id = "before_june_11_q2",
  #   next_question = list(
  #     "Yes" = "before_june_11_q2_mandate_result",
  #     "No" = "no_act_44_relief_yet_result"
  #   )
  # ),
  list(
    question = "At least 30 days prior to eligibility, probation office must serve a Probation Status Report on defendant, prosecutor, court, defense counsel, and registered victim. Must contain:

                1) Eligibility date AND
                2) Any technical violations within 6 months AND
                3) Convictions while on probation or in custody on underlying case AND
                4) Completion of programs AND
                5) Restitution payments AND
                6) A description of progress AND
                7) Recommendation that probation be terminated, conditions modified, or continue under current conditions",
    question_id = "after_june_11_q3",
    next_question = "after_june_11_q4"
  ),
  list(
    result = "Court gives prosecutor and defendant opportunity to provide input prior to decision
    
              AND 
    
              Court determines whether to terminate early or modify probation conditions by eligibility date (no grace period)

              Only legal requirement is that court must consider educational achievements and other approved benchmarks",
    question_id = "before_june_11_q2_mandate_result"
  ),
  list(
    question = "Did the defendant or prosecutor object to the recommendations within 30 days of the report?",
    choices = c("Yes", "No"),
    question_id = "after_june_11_q4",
    next_question = list(
      "Yes" = "after_june_11_q5",
      "No" = "after_june_11_q4_result"
    )
  ),
  list(
    result = "Recommendations MUST be implemented

              IF proof that PSR was validly served
              
              MAYBE early termination not permitted if defendant convicted of felony or 1st or 2nd degree misdemeanor while on probation or incarcerated on underlying offense
              Court must notify defendant, prosecutor, and registered victim",
    question_id = "after_june_11_q4_result"
  ),
  list(
    question = "Court must hold PRC. Court must notify defendant registered victim and prosecutor of date of PRC",
    question_id = "after_june_11_q5",
    next_question = "after_june_11_q6"
  ),
  list(
    question = "Did any of the following occur: 
    
                1) Defendant convicted of felony or 1st or 2nd degree misdemeanor while in probation or in custody for underlying offense 
                2) Court finds clear and convincing evidence defendant committed technical offense in the 6 months prior to PRC with identifiable threat to public safety
                3) Court finds preponderance of the evidence that in the 6 months prior to PRC defendant committed a technical offense in one of these categories:
                    - “Sexual in nature”
                    - “Assaultive behavior” or “credible threat to cause bodily injury to another”
                    - Possession or control of firearm or dangerous weapon
                    - Manufacture, sale, delivery, or possession with intent to sell drugs
                    - Absconded
                    - Unexcused and intentional failure to programming or conditions 3+ separate occasions",
    choices = c("Yes", "No"),
    question_id = "after_june_11_q6",
    next_question = list(
      "Yes" = "after_june_11_q6_result",
      "No" = "after_june_11_q7"
    )
  ),
  list(
    result = "NOT eligible for early termination

              BUT eligible for changed conditions
              
              AND court must provide reasons for denial in writing
              
              AND defendant must be given a new PRC within 6 months of the date of the violation UNLESS the basis of the prohibition was a conviction",
    question_id = "after_june_11_q6_result"
  ),
  list(
    question = "Did a court find any of the following: 
    
                1) Clear and convincing evidence of identifiable threat to public safety
                2) Preponderance of evidence that has not completed program
                3) Preponderance of evidence that has not paid restitution",
    choices = c("Yes", "No"),
    question_id = "after_june_11_q7",
    next_question = list(
      "Yes" = "after_june_11_q8",
      "No" = "after_june_11_q7_result"
    )
  ),
  list(
    result = "Court MUST grant early termination regardless of any recommendation",
    question_id = "after_june_11_q7_result"
  ),
  list(
    question = "NOT eligible for MANDATORY early termination
    
                BUT judge still has discretion to grant it, or change conditions
                
                AND court must provide reasons for denial in writing
                
                AND probationer is eligible for a PRC within 1 year of PRC date",
    # choices = character(0),
    question_id = "after_june_11_q8",
    next_question = "after_june_11_q9"
  ),
  list(
    question = "Was early termination denied solely because of a failure to pay restitution

                AND
                
                Did defendant pay at least 50% of restitution OR did they make a good faith effort to pay?",
    choices = c("Yes", "No"),
    question_id = "after_june_11_q9",
    next_question = list(
      "Yes" = "admin_probation_result",
      "No" = "early_termination_denied_result"
    )
  ),
  list(
    result = "Defendant must be placed on administrative probation",
    question_id = "admin_probation_result"
  ),
  list(
    result = "NOT eligible for MANDATORY early termination
    
              BUT judge still has discretion to grant it, or change conditions
              
              AND court must provide reasons for denial in writing
              
              AND probationer is eligible for a PRC within 1 year of PRC date",
    question_id = "early_termination_denied_result"
  ),
  list(
    result = "NO ACT 44 RELIEF YET",
    question_id = "no_act_44_relief_yet_result"
  ),
  list(
    result = "NO ACT 44 RELIEF",
    question_id = "no_act_44_relief_result"
  )
)


####Create App####
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Add a colorful theme
  tags$head(
    tags$style(HTML(
      ".question-text { font-size: 18px; font-weight: bold; margin-bottom: 10px; }
       .result-text { font-size: 20px; font-weight: bold; color: #28a745; margin-top: 20px; }
       .shiny-input-container { margin-bottom: 10px; }
       .btn-container { margin-top: 20px; }
       .logo-container { text-align: center; margin-bottom: 20px; margin-top: 10px; } /* Added margin-top */
       .logo { max-width: 200px; height: auto; }"
    ))
  ),
  
  # Add logo at the top
  div(class = "logo-container",
      img(src = "reform-logo-charcoal.png", class = "logo")
  ),
  
  titlePanel(strong("Act 44 Early Termination Tool")), # Make title bold
  uiOutput("quiz_ui"),
  div(class = "btn-container",
      uiOutput("button_ui")
  )
)

##Server
server <- function(input, output, session) {
  history <- reactiveVal(c(1))  # Track navigation history
  selected_answer <- reactiveVal(NULL)
  
  output$quiz_ui <- renderUI({
    current_index <- tail(history(), 1)
    current_question <- decision_tree[[current_index]]
    
    if("question" %in% names(current_question) & length(current_question$question_list) == 0) {
      tagList(
        div(class = "question-text", current_question$question),
        if("choices" %in% names(current_question) && length(current_question$choices) > 0) {
          radioButtons("answer", NULL, choices = current_question$choices)
        }
      )
    }else if("question" %in% names(current_question) & length(current_question$question_list) > 0 & length(current_question$date_question) == 0){
      current_question %>% 
        pluck("question_list") %>% 
        enframe() %>% 
        mutate(value = 
                 value %>% 
                 unlist()) %>% 
        pmap(
          ~tagList(
            div(class = "question-text", .y),  # The question text
            radioButtons(
              inputId = paste0("answer_", .x),  # Dynamic input ID based on question_id (e.g., "answer_q1")
              label = NULL,  # Label the radio buttons if needed
              choices = current_question$choices  # Choices for the radio buttons
            )
          )
        )
    }
    # else if("question" %in% names(current_question) & length(current_question$question_list) > 0 & 
    #          length(current_question$date_question) > 0 & current_question$date_question == "Yes"){
    #   current_question %>% 
    #     pluck("question_list") %>% 
    #     enframe() %>% 
    #     mutate(value = 
    #              value %>% 
    #              unlist()) %>% 
    #     pmap(
    #       ~tagList(
    #         div(class = "question-text", .y),  # The question text
    #         if(.x %in% current_question$date_question_n){
    #           dateInput(
    #             inputId = paste0("answer_", .x),
    #             label = "Select a date:",
    #             value = Sys.Date(),      # Default selected date
    #             min = "1980-01-01",      # Optional: min selectable date
    #             max = "2030-12-31",      # Optional: max selectable date
    #             format = "yyyy-mm-dd",   # Date format in the input box
    #             startview = "month",     # Can be "month", "year", or "decade"
    #             weekstart = 0,           # Day to start the week on (0 = Sunday)
    #             language = "en"          # Language for the calendar
    #           )
    #         }else{
    #           radioButtons(
    #             inputId = paste0("answer_", .x),  # Dynamic input ID based on question_id (e.g., "answer_q1")
    #             label = NULL,  # Label the radio buttons if needed
    #             choices = current_question$choices  # Choices for the radio buttons
    #           )
    #         }
    #       )
    #     )
    # }
    else if("result" %in% names(current_question)) {
      div(class = "result-text", current_question$result)
    }
  })
  
  output$button_ui <- renderUI({
    current_index <- tail(history(), 1)
    current_question <- decision_tree[[current_index]]
    buttons <- list()
    
    if(length(history()) > 1) {
      buttons <- append(buttons, list(actionButton("back_button", "Back", class = "btn btn-warning")))
    }
    
    if("result" %in% names(current_question)) {
      buttons <- append(buttons, list(actionButton("finish_button", "Finish", class = "btn btn-danger")))
    }else if(length(current_question$intro) > 0){
      buttons <- append(buttons, list(actionButton("start_button", "Start", class = "btn btn-success")))
    }else{
      buttons <- append(buttons, list(actionButton("next_button", "Next", class = "btn btn-primary")))
    }
    
    do.call(fluidRow, buttons)
  })
  
  observeEvent(input$answer, {
    current_index <- tail(history(), 1)
    current_question <- decision_tree[[current_index]]
    
    if(length(current_question$question_list) == 0){
      selected_answer(input$answer)
    }else if(length(current_question$question_list) > 0 & length(current_question$date_question) == 0){
      answer_ids <- names(input)[grepl("^answer_q", names(input))]
      
      responses <- sapply(answer_ids, function(id) input[[id]])  
      if("Yes" %in% responses) {
        selected_answer("Yes")
      }else{
        selected_answer("No")
      }
    }
    # else if(question_id == "after_june_11_q2"){
    #   if(input[["answer_q2"]] < as.Date("2025-06-11")){
    #     date = as.Date("2025-06-11")
    #   }else{
    #     if(input[["answer_q1"]] == "Yes"){
    #       date = as.Date(input[["answer_q2"]]) + month(4)
    #     }else{
    #       date = as.Date(input[["answer_q2"]] + month(2))
    #     }
    #   }
    # }
  })
  
  observeEvent(input$start_button, {
    current_question <- decision_tree[[1]]
    next_id <- current_question$next_question
    next_index <- which(map_chr(decision_tree, "question_id") == next_id)
    history(c(history(), next_index))
  })
  
  # observeEvent(input$next_button, {
  #   current_index <- tail(history(), 1)
  #   current_question <- decision_tree[[current_index]]
  #   
  #   if("choices" %in% names(current_question)) {
  #     next_id <- current_question$next_question[[selected_answer()]]
  #   }else{
  #     next_id <- current_question$next_question
  #   }
  #   
  #   next_index <- which(map_chr(decision_tree, "question_id") == next_id)
  #   history(c(history(), next_index))
  # })
  
  observeEvent(input$next_button, {
    current_index <- tail(history(), 1)
    current_question <- decision_tree[[current_index]]
    
    if("choices" %in% names(current_question)) {
      if(length(current_question$question_list) == 0) {
        next_id <- current_question$next_question[[selected_answer()]]
      }else{
        # Check if any of the multiple radio buttons have a "Yes" answer
        answer_ids <- names(input)[grepl("^answer_q", names(input))]
        responses <- sapply(answer_ids, function(id) input[[id]])
        if(any(responses == "Yes")) {
          next_id <- current_question$next_question[["Yes"]]
        }else{
          next_id <- current_question$next_question[["No"]]
        }
      }
    }else{
      next_id <- current_question$next_question
    }
    next_index <- which(map_chr(decision_tree, "question_id") == next_id)
    history(c(history(), next_index))
  })
  
  observeEvent(input$back_button, {
    if(length(history()) > 1) {
      history(history()[1:(length(history()) - 1)])  # Remove last step
    }
  })
  
  observeEvent(input$finish_button, {
    session$close()
  })
}

##Deploy App
shinyApp(ui = ui, server = server)