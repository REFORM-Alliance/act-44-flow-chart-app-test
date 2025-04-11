rm(list = ls())


####Read in Libraries####
library(shiny)
library(tidyverse)
library(shinythemes)
library(rsconnect)
library(shinyWidgets)
library(lubridate)
library(scales)
library(shinyjs)


####Helper Functions####
##Pretty Dates
format_pretty_date <- function(date, use_suffix = TRUE) {
  # Ensure it's a Date object
  date <- as.Date(date)
  
  day_num <- lubridate::day(date)
  day_str <- if (use_suffix) {
    scales::ordinal(day_num)
  } else {
    as.character(day_num)
  }
  
  formatted <- paste0(
    format(date, "%B "),  # Month full name
    day_str,
    ", ",
    format(date, "%Y")    # Year
  )
  
  return(formatted)
}

####Write Decision Tree####
decision_tree <- list(
  list(
    intro = TRUE,
    content_above_buttons = 
      tags$div(
        tags$p(strong("Welcome to REFORM Alliance's Act 44 Early Termination tool.")),
        tags$p(HTML(paste0("This tool is designed to help users navigate the legal requirements of ",
                           "<a href='https://www.palegis.us/statutes/unconsolidated/law-information?sessYr=2023&sessInd=0&actNum=44' target='_blank'>Pennsylvania's Act 44</a>, ",
                           "which requires courts to assess people for early termination of probation or the modification of probation conditions."))),
        tags$p("This tool is designed to be used by anyone interested in Act 44, from judges and probation officers to people on probation and their loved ones. For ease of use, please indicate whether you are a professional who routinely works in this field looking for guidance in applying this law, or a person or probation or their loved one who is looking to see how Act 44 impacts you. The tool will produce the same results no matter who you are, we just want to make this easier for you to use and provide the most relevant information possible.")
      ),
    content_below_buttons = 
      tags$div(
        tags$p("Please note that this tool is specifically designed to be used by people on their probation or others who do not professionally work in this system. For those who work professionally in this field, we recommend using the flow chart available below that walks through every step in the Act 44 process regarding early termination and modification of probation conditions."),
        tags$a(
          href = "act-44-early-termination-flow-chart.png",
          target = "_blank",
          tags$img(
            src = "act-44-early-termination-flow-chart.png",
            style = "width: 100%; max-width: 1200px; display: block; margin: 20px auto;"
          )
        ),
        tags$p(HTML(paste0("The tool and flowchart are designed to help navigate Act 44, but they are no substitute for legal advice and are designed for informational purposes only. Please consult a lawyer with any legal questions about your rights under Act 44. You can find the full text of the law ",
                           "<a href='https://www.palegis.us/statutes/unconsolidated/law-information?sessYr=2023&sessInd=0&actNum=44' target='_blank'>here</a>.")))
      ),
    next_question = "defendant_sentencing_date",
    question_id = "intro_page"
  ),
  list(
    question = 
      tags$div(
        tags$p(strong("First we need to ask when you or your loved one was sentenced. The type of relief that a person is eligible for under Act 44 changes dramatically depending on when they were sentenced.")),
        tags$p(HTML(paste0("Do you know the date you or your loved one was sentenced? If you do not, you can look it up ", 
                           "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank'>here</a>."))),
        tags$p("What date was you or your loved one sentenced?")
      ),
    choices = c("Yes", "No"),
    date_question = "Yes",
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
      "Yes" = "no_act_44_relief_result",
      "No" = "after_june_11_q2"
    )
  ),
  list(
    question = 
      tags$div(
        tags$p(HTML(paste0("Next we need to ask you some questions about the crime or crimes for which you or your loved one was sentenced. Do you know the crimes for which you or your loved one was convicted? If not, you can look them up ",
                           "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank'>here</a>."))),
        tags$p("Was the person sentenced to probation for one of the following types of crimes?")
      ),
    question_list = list("q1" = "A crime related to sex offender registration",
                         "q2" = paste0("A ", "<a href='https://www.legis.state.pa.us/cfdocs/legis/LI/consCheck.cfm?txtType=HTM&ttl=42&div=0&chpt=97&sctn=14&subsctn=0' target='_blank'>crime of violence</a>"),
                         "q3" = "Assault or stalking against a family or household member? This only counts if they were convicted under under 18 Pa.C.S. § 2701 (relating to simple assault) or 2709.1 (relating to stalking)"),
    choices = c("Yes", "No"),
    question_id = "before_june_11_q1",
    next_question = 
      list(
        "Yes" = "no_act_44_relief_result",
        "No" = "before_june_11_q1_1"
      )
  ),
  list(
    question =
      tags$div(
        tags$p("Now we need to ask you about you or your loved one's behavior while on probation. Eligibility for certain kinds of benefits under Act 44 depends on whether you or your loved one had any arrests, convictions, or violations while on probation or while in custody on the case that led them to probation."),
        tags$p("First of all, were you or a loved one convicted of any felony or first or second degree misdemeanor during this time? Please note that only a conviction counts here, not an arrest.")
      ),
    choices = c("Yes", "No"), 
    question_id = "before_june_11_q1_1",
    next_question = 
      list(
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
      "No" = "no_act_44_relief_result"
    )
  ),
  list(
    question = 
      tags$div(
        tags$p(strong("Now we need to ask you about the severity of the charge you or your loved one was convicted of. This is important because it will determine when you or your loved one are eligible for benefits under Act 44.")),
        tags$p(HTML(paste0("Were any of these charges a felony? If you don't know the answer to this, you can look up it up ",
                           "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank'>here</a>.")))
      ),
    choices = c("Yes", "No"),
    question_id = "before_june_11_q2",
    next_question = list(
      "Yes" = "before_june_11_q3",
      "No" = "before_june_11_q3"
    )
  ),
  list(
    choices = c("Judge Finds Technical Violations", "No Technical Violations"),
    question_id = "before_june_11_q3",
    next_question = list(
      "Judge Finds Technical Violations" = "no_act_44_relief_result",
      "No Technical Violations" = "section_7_act_44_relief_result"
    )
  ),
  list(
    result = "You or your loved one is entitled to a conference under Act 44 where a judge will determine whether their probation should be terminated or modified. That conference must be held by eligibility date",
    question_id = "section_7_act_44_relief_result"
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
  list(result = 
         tags$div(
           tags$p(HTML(paste0("You or your loved one is entitled to apply for early termination or to have their conditions modified under ",
                              "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank'>42 P.A.C.S. § 9771</a>.", 
                              " A judge has discretion to grant or deny this application. Due to the nature of yours or your loved one's convictions they are not entitled to an automatic hearing under Act 44, but they are always eligible to apply for termination or modification of conditions under ",
                              "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank'>42 P.A.C.S. § 9771</a>.")))
         ),
       question_id = "no_act_44_relief_result"
  )
  # ,
  # list(result = 
  #        tags$div(
  #          tags$p("On eligibility date, a court will be required to review the person on probation to determine whether they have their probation terminated or the conditions modified. That judge will have almost unlimited discretion in making this decision, which is why it is important for people on probation to give the court any information that would help it make this decision. In fact, the law requires the court to give both the person on probation and the prosecutor an opportunity to provide written comments on this issue prior to its determination. Do not pass up this opportunity!"),
  #          tags$p("In the meantime, the person on probation is entitled to apply for early termination or to have their conditions modified under 42 P.A.C.S. § 9771 at any time. A judge has discretion to grant or deny this application at any time, for any person, even if that person is not eligible for a mandatory review under Act 44.")
  #        ),
  #      question_id = "no_act_44_relief_yet_result"
  # )
)


####Create App####
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Add a colorful theme
  tags$head(
    # tags$style(HTML(
    #   ".question-text { font-size: 18px; font-weight: bold; margin-bottom: 10px; }
    #    .result-text { font-size: 20px; font-weight: bold; color: #28a745; margin-top: 20px; }
    #    .shiny-input-container { margin-bottom: 10px; }
    #    .btn-container { margin-top: 20px; }
    #    .logo-container { text-align: center; margin-bottom: 20px; margin-top: 10px; } /* Added margin-top */
    #    .logo { max-width: 200px; height: auto; }"
    # ))
    tags$style(HTML(
      "
      .question-text { font-size: 18px; font-weight: bold; margin-bottom: 10px; }
      .result-text { font-size: 20px; font-weight: bold; color: #28a745; margin-top: 20px; }
      .shiny-input-container { margin-bottom: 10px; }
      .btn-container { margin-top: 20px; }
      .logo-container { text-align: center; margin-bottom: 20px; margin-top: 10px; } /* Added margin-top */
      .logo { max-width: 200px; height: auto; }

      /* Custom button styling */
      #violation-buttons .btn {
        margin-right: 10px;
        margin-bottom: 10px; /* Add bottom margin for spacing */
        border: 1px solid #ccc;
        padding: 10px 15px;
        font-size: 16px;
        min-width: 200px; /* Adjust as needed */
        text-align: center;
      }

      #violation-buttons .btn.active {
        border-color: #007bff;
        box-shadow: 0 0 5px rgba(0, 123, 255, 0.5);
      }

      #violation-buttons .btn-danger {
        background-color: #dc3545;
        color: white;
      }

      #violation-buttons .btn-success {
        background-color: #28a745;
        color: white;
      }
      "
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
  sentencing_date <- reactiveVal(NULL)
  selected_answer <- reactiveVal(NULL)
  felony_or_misdemeanor <- reactiveVal(NULL)
  eligibility_date <- reactiveVal(NULL)
  
  violation_answer <- reactiveVal(NULL)
  
  ##Reactive function to calculate eligibility_date
  calculate_eligibility_date <- reactive({
    if(!is.null(sentencing_date()) & !is.null(felony_or_misdemeanor())) {
      base_date <- as.Date(sentencing_date())
      felony_val <- felony_or_misdemeanor()
      if(felony_val == "Felony") {
        potential_date <- base_date + years(4)
      }else if(felony_val == "Misdemeanor"){
        potential_date <- base_date + years(2)
      }
      eligibility_date_val <- max(potential_date, as.Date("2025-06-11"))
      return(eligibility_date_val) # Return the value for use in renderUI
    }else{
      return(NULL) # Return NULL if dependencies are not yet available
    }
  })
  
  observe({
    eligibility_date(calculate_eligibility_date())
  })
  
  ##Eligibility Date Rendering Function
  render_before_june_11_q3 <- reactive({
    eligibility_date_val <- eligibility_date()
    question_content <- tags$div(
      tags$p(HTML(paste0(
        "Now we need to ask you about the probationer's behavior in the following dates: ",
        "from ", "<strong>", format_pretty_date(eligibility_date_val - months(6)), "</strong>",
        " to ", "<strong>", format_pretty_date(eligibility_date_val), "</strong>",
        ". A judge will look at the probationer's record and determine whether they have committed any technical violations during this time period. If they do, it changes what relief the probationer can receive. A technical violation is defined as the violation of any specific term of probation that is not a criminal conviction."
      ))),
      tags$p("A judge will assess whether the probationer has committed a violation in the following categories:"),
      tags$ol(type = "I",
              tags$li("A violation that was sexual in nature"),
              tags$li("A violation that involved assaultive behavior or included a credible threat to cause bodily injury to another, including incidents involving domestic violence"),
              tags$li("A violation that involved possession or control of a firearm or dangerous weapon"),
              tags$li("A violation involved the manufacture, sale, delivery or possession with the intent to manufacture, sell or deliver, a controlled substance or other drug regulated under the act of April 14, 1972 (P.L.233, No.64), known as The Controlled Substance, Drug, Device and Cosmetic Act"),
              tags$li("A violation in which the probationer absconded from probation"),
              tags$li("A violation which involved an intentional and unexcused failure to adhere to recommended programming or conditions on three or more separate occasions. Multiple technical violations stemming from the same episode of events do not constitute separate technical violations."),
              tags$li("A violation that involved an identifiable threat to public safety")
      ),
      tags$p("If the judge finds that a violation has occurred, the next steps will be as follows:"),
      tags$p("If the judge finds no technical violations, the next steps are here:")
    )
    if(!is.null(eligibility_date_val)) {
      tagList(
        question_content,
        fluidRow(
          column(width = 12, # Use all 12 columns to span the full width
                 div(
                   id = "violation-buttons", # Container for buttons
                   actionButton("violation_yes", decision_tree[[which(map_chr(decision_tree, "question_id") == "before_june_11_q3")]]$choices[1], class = "btn btn-danger"),
                   actionButton("violation_no", decision_tree[[which(map_chr(decision_tree, "question_id") == "before_june_11_q3")]]$choices[2], class = "btn btn-success")
                 ),
                 tags$script(HTML(
                   "
                 $(document).on('click', '#violation-buttons .btn', function() {
                   $('#violation-buttons .btn').removeClass('active');
                   $(this).addClass('active');
                 });
                 "
                 ))
          )
        )
      )
    }else {
      tags$p("Calculating eligibility date...")
    }
  })
  
  render_section_7_act_44_relief_result <- reactive({
    eligibility_date_val <- eligibility_date()
    eligibility_date_val_clean <- format_pretty_date(eligibility_date_val)
    # question_content <-
    #   tags$div(
    #     tags$p(HTML(paste0(
    #       "You or your loved one is entitled to a conference under Act 44 where a judge will determine whether their probation should be terminated or modified. That conference must be held by ",
    #       "<strong>", eligibility_date_val_clean, "</strong>", "."
    #     )))
    #   )

    question_content <-
      tags$div(
        tags$p(HTML(paste0("On ", "<strong>", eligibility_date_val_clean, "</strong>", ", a court will be required to review the person on probation to determine whether they have their probation terminated or the conditions modified. That judge will have almost unlimited discretion in making this decision, which is why it is important for people on probation to give the court any information that would help it make this decision. In fact, the law requires the court to give both the person on probation and the prosecutor an opportunity to provide written comments on this issue prior to its determination. Do not pass up this opportunity!"))),
        tags$p("In the meantime, the person on probation is entitled to apply for early termination or to have their conditions modified under 42 P.A.C.S. § 9771 at any time. A judge has discretion to grant or deny this application at any time, for any person, even if that person is not eligible for a mandatory review under Act 44.")
      )
    
    if (!is.null(eligibility_date_val)) {
      tagList(
        question_content, # Display the result message first
        fluidRow(
          column(
            width = 12,
            div(
              class = "btn-container", # Use your existing button styling
            )
          )
        )
      )
    } else {
      tags$p("Calculating eligibility date...")
    }
  })
  
  # render_no_act_44_relief_yet_result <- reactive({
  #   eligibility_date_val <- eligibility_date()
  #   eligibility_date_val_clean <- format_pretty_date(eligibility_date_val)
  #   question_content <-
  #     tags$div(
  #       tags$p(HTML(paste0("On ", "<strong>", eligibility_date_val_clean, "</strong>", ", a court will be required to review the person on probation to determine whether they have their probation terminated or the conditions modified. That judge will have almost unlimited discretion in making this decision, which is why it is important for people on probation to give the court any information that would help it make this decision. In fact, the law requires the court to give both the person on probation and the prosecutor an opportunity to provide written comments on this issue prior to its determination. Do not pass up this opportunity!"))),
  #       tags$p("In the meantime, the person on probation is entitled to apply for early termination or to have their conditions modified under 42 P.A.C.S. § 9771 at any time. A judge has discretion to grant or deny this application at any time, for any person, even if that person is not eligible for a mandatory review under Act 44.")
  #     )
  #   
  #   if (!is.null(eligibility_date_val)) {
  #     tagList(
  #       question_content, # Display the result message first
  #       fluidRow(
  #         column(
  #           width = 12,
  #           div(
  #             class = "btn-container", # Use your existing button styling
  #           )
  #         )
  #       )
  #     )
  #   } else {
  #     tags$p("Calculating eligibility date...")
  #   }
  # })
  
  output$quiz_ui <- renderUI({
    current_index <- tail(history(), 1)
    current_question <- decision_tree[[current_index]]
    
    if(length(current_question$intro) > 0){
      tagList(
        current_question$content_above_buttons,
        uiOutput("intro_buttons"), # Placeholder for buttons
        current_question$content_below_buttons
      )
    }else if("question" %in% names(current_question) & length(current_question$question_list) == 0) {
      tagList(
        div(class = "question-text", current_question$question),
        if("choices" %in% names(current_question) & length(current_question$choices) > 0 & length(current_question$date_question) == 0) {
          radioButtons("answer", NULL, choices = current_question$choices)
        }else if("choices" %in% names(current_question) & length(current_question$choices) > 0 & length(current_question$date_question) > 0){
          dateInput(
            inputId = "answer",
            label = "Select a date:",
            value = Sys.Date(),      
            min = "1980-01-01",      
            max = "2030-12-31",      
            format = "yyyy-mm-dd",   
            startview = "month",     
            weekstart = 0,           
            language = "en"          
          )
        }
      )
    }else if(current_question$question_id == "before_june_11_q3") {
      # Reactive rendering for the specific question
      render_before_june_11_q3()
    }else if("question" %in% names(current_question) & length(current_question$question_list) > 0 & length(current_question$date_question) == 0){
      tagList(
        div(class = "question-text", current_question$question),
        current_question %>% 
          pluck("question_list") %>% 
          enframe() %>% 
          unnest(value) %>% 
          pmap(
            ~tagList(
              div(class = "question-text", tags$div(tags$p(HTML(.y)))),  # The question text
              radioButtons(
                inputId = paste0("answer_", .x),  # Dynamic input ID based on question_id (e.g., "answer_q1")
                label = NULL,  # Label the radio buttons if needed
                choices = current_question$choices  # Choices for the radio buttons
              )
            )
          )
      )
    }
    else if("result" %in% names(current_question)) {
      if(current_question$question_id == "section_7_act_44_relief_result"){
        render_section_7_act_44_relief_result()
      }
      # else if(current_question$question_id == "no_act_44_relief_yet_result"){
      #   render_no_act_44_relief_yet_result()
      # }
      else{
        div(class = "result-text", current_question$result)
      }
      
    }
  })
  
  output$intro_buttons <- renderUI({
    div(
      actionButton("start_button", "Start", class = "btn btn-success")
    )
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
    }else if(length(current_question$intro) == 0){
      buttons <- append(buttons, list(actionButton("next_button", "Next", class = "btn btn-primary")))
    }
    
    do.call(fluidRow, buttons)
  })
  
  observeEvent(input$answer, {
    current_index <- tail(history(), 1)
    current_question <- decision_tree[[current_index]]
    
    if(length(current_question$question_list) == 0 & length(current_question$date_question) > 0){
      if(current_question$question_id == "defendant_sentencing_date"){
        sentencing_date(as.Date(input$answer))
        ifelse(as.Date(input$answer) >= as.Date("2024-06-11"), selected_answer("Yes"), selected_answer("No"))
      }
    }else if(length(current_question$question_list) == 0 & length(current_question$date_question) == 0){
      if(current_question$question_id == "before_june_11_q2"){
        ifelse(input$answer == "Yes", felony_or_misdemeanor("Felony"), felony_or_misdemeanor("Misdemeanor"))
      }
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
  })
  
  observeEvent(input$start_button, {
    current_question <- decision_tree[[1]]
    next_id <- current_question$next_question
    next_index <- which(map_chr(decision_tree, "question_id") == next_id)
    history(c(history(), next_index))
  })
  
  observeEvent(input$next_button, {
    current_index <- tail(history(), 1)
    current_question <- decision_tree[[current_index]]
    
    if("choices" %in% names(current_question)) {
      if(length(current_question$question_list) == 0) {
        if (current_question$question_id == "before_june_11_q3") {
          next_id <- current_question$next_question[[violation_answer()]]
        } else {
          next_id <- current_question$next_question[[selected_answer()]]
        }
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
  
  observeEvent(input$violation_yes, {
    violation_answer(decision_tree[[which(map_chr(decision_tree, "question_id") == "before_june_11_q3")]]$choices[1])
    shinyjs::click("next_button") # Trigger the Next button click
  })
  
  observeEvent(input$violation_no, {
    violation_answer(decision_tree[[which(map_chr(decision_tree, "question_id") == "before_june_11_q3")]]$choices[2])
    shinyjs::click("next_button") # Trigger the Next button click
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