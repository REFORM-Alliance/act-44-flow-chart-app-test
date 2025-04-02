library(shiny)
library(tidyverse)

# Define the decision tree structure
# decision_tree <- list(
#   list(
#     question = "Do you prefer warm or cold weather?",
#     choices = c("Warm", "Cold"),
#     next_question = list(
#       "Warm" = "warm_weather_question",
#       "Cold" = "cold_weather_question"
#     )
#   ),
#   list(
#     question = "Do you like beaches?",
#     choices = c("Yes", "No"),
#     question_id = "warm_weather_question",
#     next_question = list(
#       "Yes" = "beach_result",
#       "No" = "mountain_result"
#     )
#   ),
#   list(
#     question = "Do you like snow sports?",
#     choices = c("Yes", "No"),
#     question_id = "cold_weather_question",
#     next_question = list(
#       "Yes" = "snow_result",
#       "No" = "cozy_result"
#     )
#   ),
#   list(result = "Beach vacation!", question_id = "beach_result"),
#   list(result = "Mountain hiking!", question_id = "mountain_result"),
#   list(result = "Ski trip!", question_id = "snow_result"),
#   list(result = "Cozy cabin stay!", question_id = "cozy_result")
# )

decision_tree <- list(
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
                3) The defendant was convicted of any felony or 1st or 2nd degree misdemeanor while on probation",
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
  list(
    question = "At least 30 days prior to eligibility, probation office must serve a Probation Status Report on defendant, prosecutor, court, defense counsel, and registered victim. Must contain:

                1) Eligibility date AND
                2) Any technical violations within 6 months AND
                3) Convictions while on probation or in custody on underlying case AND
                4) Completion of programs AND
                5) Restitution payments AND
                6) A description of progress AND
                7) Recommendation that probation be terminated, conditions modified, or continue under current conditions",
    # choices = character(0),
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
    # choices = character(0),
    question_id = "after_june_11_q5",
    next_question = "after_june_11_q6"
  ),
  list(
    question = "Did any of the following occur: 
    
                1) Defendant convicted of felony or 1st or 2nd degree misdemeanor while in probation or in custody for underlying offense 
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

ui <- fluidPage(
  titlePanel("Decision Tree App"),
  uiOutput("quiz_ui"),
  uiOutput("button_ui") # Add a UI output for the buttons
)

server <- function(input, output, session) {
  current_question_id <- reactiveVal(1) # Start with the first question
  selected_answer <- reactiveVal(NULL)
  
  output$quiz_ui <- renderUI({
    current_question <- decision_tree[[current_question_id()]]
    
    if("question" %in% names(current_question) & "choices" %in% names(current_question) & length(current_question$choices) > 0) {
      radioButtons(
        "answer",
        current_question$question,
        choices = current_question$choices
      )
    }else if("question" %in% names(current_question) & length(current_question$choices) == 0) {
      h3(current_question$question)
    }else if("result" %in% names(current_question)) {
      h3(current_question$result)
    }else {
      return(NULL)
    }
  })
  
  output$button_ui <- renderUI({
    current_question <- decision_tree[[current_question_id()]]
    if("result" %in% names(current_question)) {
      actionButton("finish_button", "Finish")
    }else {
      actionButton("next_button", "Next")
    }
  })
  
  observeEvent(input$answer, {
    selected_answer(input$answer)
  })
  
  observeEvent(input$next_button, {
    current_question <- decision_tree[[current_question_id()]]

    if(length(current_question$choices) == 0 & length(current_question$next_question) == 1){
      next_id <- current_question$next_question
      next_question_index <-
        decision_tree %>%
        map(~.x$question_id) %>%
        unlist() %>%
        str_which(paste0("^", next_id, "$"))
      
      current_question_id(next_question_index)
    }else if(length(current_question$choices) > 0){
      next_id <- current_question$next_question[[selected_answer()]]
      next_question_index <- 
        decision_tree %>% 
        map(~.x$question_id) %>% 
        unlist() %>% 
        str_which(paste0("^", next_id, "$"))
      
      current_question_id(next_question_index)
    }
  })
  
  observeEvent(input$finish_button, {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)