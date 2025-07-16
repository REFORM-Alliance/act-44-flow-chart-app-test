rm(list = ls())


# Load Libraries
library(jsonlite)
library(tidyverse) # For map_chr, enframe, unnest, pmap, filter
library(htmltools) # For tags, html_text
library(lubridate) # For day(), years(), months(), days()
library(scales)    # For ordinal()
library(here)

# --- Helper Functions (from your original Shiny app code) ---
## Pretty Dates
format_pretty_date <- function(date, use_suffix = TRUE){
  date <- as.Date(date)
  
  day_num <- lubridate::day(date)
  day_str <- if(use_suffix){
    scales::ordinal(day_num)
  }else{
    as.character(day_num)
  }
  
  formatted <- paste0(
    format(date, "%B "),
    day_str,
    ", ",
    format(date, "%Y")
  )
  
  return(formatted)
}

# --- 1. Load translation.json ---
translation_raw_data <- jsonlite::fromJSON(here("json", "translation.json"))
translation_data <- translation_raw_data$translation

# --- 2. Mock i18n object for translation lookup ---
# This mimics the core behavior of shiny.i18n::Translator for string lookup
mock_i18n <- function(lang = "en") {
  current_lang <- lang
  
  set_language <- function(new_lang) {
    current_lang <<- new_lang
  }
  
  # The translation function 't'
  t <- function(key) {
    trans_entry <- translation_data %>%
      filter(en == key)
    
    if (nrow(trans_entry) > 0) {
      return(trans_entry[[current_lang]][1])
    } else {
      # If not found, return the key itself (or some indicator for debugging)
      return(paste0("[MISSING_TRANS:", key, "]"))
    }
  }
  
  return(list(
    set_translation_language = set_language,
    t = t
  ))
}

# --- 3. Define a recursive UI element translator for text extraction ---
# This function traverses the 'tags' object and applies translations.
# It's based on your app's `translate_ui_recursive` logic, adapted for text extraction.
# Corrected translate_and_extract_html_text function (third attempt)
translate_and_extract_html_text <- function(element, i11n_translator_func) {
  if (is.character(element)) {
    return(i11n_translator_func(element))
  } else if (inherits(element, "html_node") || inherits(element, "xml_node")) {
    return(element) # Keep raw HTML nodes as is for html_text to process later
  } else if (inherits(element, "shiny.tag") || inherits(element, "htmltools.tag")) {
    translated_children <- list()
    if (!is.null(element$children)) {
      translated_children <- lapply(element$children, translate_and_extract_html_text, i11n_translator_func)
    }
    
    # Initialize a list for arguments to pass to `tag()`
    # The first element is always the tag name
    tag_args <- list(element$name)
    
    # Add all attributes from element$attribs.
    # It's crucial that these are named elements within the list.
    if (!is.null(element$attribs) && length(element$attribs) > 0) {
      tag_args <- c(tag_args, element$attribs)
    }
    
    # Append translated children directly.
    # These will be treated as unnamed arguments (the content).
    tag_args <- c(tag_args, translated_children)
    
    # Call the `tag` function with the carefully constructed arguments
    return(do.call(tag, tag_args))
    
  } else if (is.list(element)) {
    # Recursively apply to elements within a list (e.g., from a tagList or multiple items)
    return(lapply(element, translate_and_extract_html_text, i11n_translator_func))
  }
  return(element)
}

# Rest of your script remains the same:
# 1. Load translation.json
# 2. Mock i18n object
# 3. Decision Tree Definition
# 4. get_ui_text_pairs function (which calls translate_and_extract_html_text)
# 5. Iteration through decision_tree to collect translations
# 6. Post-processing and Write to CSV
# --- Decision Tree Definition (copied directly from your prompt) ---
# This is the R list structure that defines your app's content.
decision_tree <- list(
  list(
    intro = TRUE,
    content_above_buttons =
      tags$div(
        tags$p(strong("Welcome to REFORM Alliance's Act 44 Early Termination tool.")),
        tags$p(
          "This tool is designed to help users navigate the legal requirements of ",
          tags$a(href='https://www.palegis.us/statutes/unconsolidated/law-information/view-statute?txtType=HTM&SessYr=2023&ActNum=0044.&SessInd=0',
                 target='_blank', class='external-link', "Pennsylvania's Act 44"),
          ", which requires courts to assess people for early termination of probation or the modification of probation conditions."
        ),
        tags$p("This tool can be used by anyone, but is specifically designed to be used by people on probation. To begin using the tool, please click on the START button below.")
      ),
    content_below_buttons =
      tags$div(
        tags$p(
          "This tool is specifically designed to be used by people on probation or others who do not professionally work in this system. For those who work in this field, we have created other tools to help navigate every step in the Act 44 process regarding early termination and modification of probation conditions, those tools are available ",
          tags$a(href='https://reformalliance.com/pennsylvania-act-44/', target='_blank', class='external-link', "here"),
          "."
        )
      ),
    next_question = "defendant_sentencing_date",
    question_id = "intro_page"
  ),
  list(
    question =
      tags$div(
        tags$p(strong("First we need to ask when you were sentenced. We ask this question first because Act 44 provides very different options for you depending on when you were sentenced.")),
        tags$p(
          "What date were you sentenced? If you do not know, you can look it up ",
          tags$a(href='https://ujsportal.pacourts.us/casesearch', target='_blank', class='external-link', "here"),
          "."
        )
      ),
    choices = c("Yes", "No"),
    date_question = "Yes",
    question_id = "defendant_sentencing_date",
    next_question = list(
      "Yes" = "prc_flow_q1",
      "No" = "section_7_q1"
    )
  ),
  list(
    question =
      tags$div(
        tags$p(strong("Next we need to ask you some questions about the charge or charges for which you were sentenced to probation. This is important because people sentenced for certain types of crimes are eligible for different kinds of legal options.")),
        tags$p(
          "Do you know the crimes for which you were convicted? If not, you can look them up ",
          tags$a(href='https://ujsportal.pacourts.us/casesearch', target='_blank', class='external-link', "here"),
          "."
        ),
        tags$p("Were you sentenced to probation for one of the following types of crimes?"),
        tags$ul(
          tags$li(
            "A ",
            tags$a(href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=14&subsctn=0', target='_blank', class='external-link', "Crime of Violence")
          ),
          tags$li(
            "A crime related to sex offender registration (defined either ",
            tags$a(href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=91&subsctn=0', target='_blank', class='external-link', "here"),
            " or ",
            tags$a(href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=99&subsctn=51', target='_blank', class='external-link', "here"),
            ")"
          ),
          tags$li(
            "A crime of ",
            tags$a(href='https://www.legis.state.pa.us/WU01/LI/LI/CT/HTM/18/00.025..HTM', target='_blank', class='external-link', "homicide"),
            ", including manslaughter, causing or aiding suicide, or drug delivery resulting in death"
          ),
          tags$li(
            tags$a(href='https://www.legis.state.pa.us/WU01/LI/LI/CT/HTM/18/00.027..HTM', target='_blank', class='external-link', "Simple assault"),
            " committed against a family or household member"
          ),
          tags$li(
            tags$a(href='https://www.legis.state.pa.us/WU01/LI/LI/CT/HTM/18/00.027.009.001..HTM', target='_blank', class='external-link', "Stalking")
          )
        )
      ),
    choices = c("Yes", "No"),
    question_id = "prc_flow_q1",
    next_question = list(
      "Yes" = "prc_result_1",
      "No" = "prc_flow_split_sentence"
    )
  ),
  list(
    result =
      tags$div(
        tags$p(
          "You are eligible to apply for early termination of probation, or for modifications of probation conditions, under ",
          tags$a(href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0', target='_blank', class='external-link', "42 P.A.C.S. § 9771"),
          ". A judge has discretion to grant or deny this application at any time. However, you are not eligible for early termination or the modification of probation conditions under Act 44. Please consult with a lawyer with any questions about your eligibility."
        )
      ),
    question_id = "prc_result_1"
  ),
  list(
    question =
      tags$div(
        tags$p(strong("Next we need to ask you whether any of the charges that landed you on probation were felonies. This is important because it helps determine when you might be eligible for early termination.")),
        tags$p(
          "Were any of the charges for which you were sentenced to probation a felony? If not, you can look that up ",
          tags$a(href='https://ujsportal.pacourts.us/casesearch', target='_blank', class='external-link', "here"),
          "."
        )
      ),
    choices = c("Yes", "No"),
    question_id = "prc_flow_q2",
    next_question = list(
      "Yes" = "prc_flow_q3_1",
      "No" = "prc_flow_q3_2"
    )
  ),
  list(
    question = tags$div(
      tags$p("Did you start serving probation immediately after being on parole?")
    ),
    choices = c("Yes", "No"),
    question_id = "prc_flow_q3_1",
    next_question = list(
      "Yes" = "prc_flow_q3_1_final_year_parole",
      "No" = "prc_flow_q4"
    )
  ),
  list(
    question = tags$div(
      tags$p("Did you serve the final year of parole without any violations?")
    ),
    choices = c("Yes", "No"),
    question_id = "prc_flow_q3_1_final_year_parole",
    next_question = list(
      "Yes" = "prc_flow_q4",
      "No" = "prc_flow_q4"
    )
  ),
  list(
    question =
      tags$div(
        tags$p(strong("Next we need to ask more about your case, it will help us determine when you might be eligible for early termination. Were you convicted of multiple misdemeanors that resulted in consecutive sentences? Again, if you don’t know this, you can look it up ",
                      tags$a(href='https://ujsportal.pacourts.us/casesearch', target='_blank', class='external-link', "here"),
                      ".")),
        tags$p("")
      ),
    choices = c("Yes", "No"),
    question_id = "prc_flow_q3_2",
    next_question = list(
      "Yes" = "prc_flow_q3_3",
      "No" = "prc_flow_q4"
    )
  ),
  list(
    question =
      tags$div(
        tags$p("We need to ask one final question about these convictions, to help determine when you might be eligible for early termination. We need to know whether your misdemeanor convictions were for the same acts or different acts? In other words, were the convictions from one incident at one time, or unrelated incidents at different times?")
      ),
    choices = c("Same Conduct", "Different Conduct"),
    question_id = "prc_flow_q3_3",
    next_question = list(
      "Same Conduct" = "prc_flow_q4",
      "Different Conduct" = "prc_flow_q4"
    )
  ),
  list(
    question =
      tags$div(
        tags$p(strong("Next we need to ask about the length of the probation sentence you have. We need this information to figure out when you are eligible for a probation review conference as part of Act 44. Do you know how long your probation sentence was? If not, you can look it up ",
                      tags$a(href='https://ujsportal.pacourts.us/casesearch', target='_blank', class='external-link', "here"),
                      ".")),
        tags$p("Please enter the probation sentence length here. If you are serving multiple probation sentences consecutively, please add the sentences together. If you served probation after serving time in jail or prison, do not include any jail or prison time here.")
      ),
    drop_down = TRUE,
    drop_down_options = c("Month", "Year"),
    question_id = "prc_flow_q4",
    next_question = "prc_flow_q5"
  ),
  list(
    question =
      tags$div(
        tags$p(strong("Next we need to ask you about what you have been doing while on probation. Act 44 allows those who have finished certain programs or graduated from high school or college while on probation to be eligible for early termination earlier than others.")),
        tags$p("Did you do any of the following things while on probation?"),
        tags$ul(
          tags$li("Earn a high school diploma or GED"),
          tags$li("Earn an associate degree from an accredited university, college, seminary college, community college, or two-year college"),
          tags$li("Earn a bachelor's degree from an accredited university, college, or seminary college"),
          tags$li("Earn a master's or other graduate degree from an accredited university, college, or seminary college"),
          tags$li("Obtain a vocational or occupational license, certificate, registration, or permit that was approved by your probation officer"),
          tags$li("Complete a certified vocational, certified technical, or certified career education or training program that was approved by your probation officer"),
          tags$li("Finish a program or condition designed to improve your life that was ordered by the court at sentencing and approved by your probation officer")
        )
      ),
    choices = c("Yes", "No"),
    question_id = "prc_flow_q5",
    next_question = list(
      "Yes" = "prc_flow_q6",
      "No" = "prc_flow_q7"
    )
  ),
  list(
    question =
      tags$div(
        tags$p("Did you complete two or more of these achievements while on probation?")
      ),
    choices = c("Yes", "No"),
    question_id = "prc_flow_q6",
    next_question = list(
      "Yes" = "prc_flow_q7",
      "No" = "prc_flow_q7"
    )
  ),
  list(question = "",
       question_id = "prc_flow_q7",
       next_question = "prc_flow_q8"),
  list(question =
         tags$div(
           tags$p("After receiving the Probation Status Report, you and the prosecutor have 30 days to object to the findings and recommendations of your probation office. This is ",
                  tags$b("extremely important "), "because if neither you nor your prosecutor object, these recommendations ",
                  tags$b("must"), " be enforced. This means that if your probation office recommends early termination and your prosecutor does not object, the court must terminate your probation! But it also means that if you do not agree with your probation office’s recommendations, you should strongly consider objecting to this report, and should contact your lawyer about your options."),
           tags$p("If you or your prosecutor object to this report, the next step is a mandatory court procedure called a Probation Review Conference. What happens at a Probation Review Conference is extremely important because it may help you decide whether to object to this report or not. In some cases, a court will be required to grant you early termination at a Probation Review Conference. In other cases, a court will not be allowed to do so. Knowing how these conferences work will help you make the right decision about objecting to the Probation Status Report or not."),
           tags$p("Click NEXT to find out what happens at a Probation Review Conference.")
         ),
       question_id = "prc_flow_q8",
       next_question = "prc_flow_q9"),
  list(
    question = "", # This will be generated by render_prc_flow_q9, which needs the fix
    choices = c("Yes", "No"),
    question_id = "prc_flow_q9",
    next_question = list(
      "Yes" = "prc_flow_q9_result",
      "No" = "prc_flow_q10"
    )
  ),
  list(
    result =
      tags$div(
        tags$p("At your Probation Review Conference, a court will consider whether to change the conditions of your probation, and can reduce or increase the severity of those conditions. The judge is permitted to consider any information you provide, so if you are seeking to have your conditions changed it is important that you give the judge all the information you can to support your application."),
        tags$p("Due to your conviction, a judge cannot grant you early termination at your Probation Review Conference. However, you are always eligible to apply for early termination under ",
               tags$a(href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0', target='_blank', class='external-link', "42 P.A.C.S. § 9771"),
               ". A judge has discretion to grant such an application at any time regardless of any convictions or other issues."),
        tags$p("After your conference, the court must provide you with “written notice of the court's order detailing the court's findings.” You are eligible for another probation review conference within one year of the Conference, where the judge can again consider changing the conditions of your probation but cannot terminate probation early.")
      ),
    question_id = "prc_flow_q9_result"
  ),
  list(
    question =
      tags$div(
        tags$p(strong("Next, a judge will look at any alleged bad acts you have committed while on probation. This is important because if the judge finds you have committed one of these acts, they are not allowed to grant you early termination.")),
        tags$p("First, the judge will decide whether there is clear and convincing evidence that you caused an “identifiable threat to public safety.” The terms “clear and convincing evidence” and “identifiable threat to public safety” have important legal meanings, please consult an attorney if you have any question about their meaning."),
        tags$p("The options available at a Probation Review Conference change depending on whether a judge finds this “identifiable threat to public safety.” Click below to see what happens next.")
      ),
    choices = c("Found Threat to Public Safety", "Did Not Find Threat to Public Safety"),
    question_id = "prc_flow_q10",
    next_question = list(
      "Found Threat to Public Safety" = "prc_flow_q10_result",
      "Did Not Find Threat to Public Safety" = "prc_flow_q11"
    )
  ),
  list(
    result =
      tags$div(
        tags$p("At your Probation Review Conference, a court will consider whether to change the conditions of your probation, and can reduce or increase the severity of those conditions. The judge is permitted to consider any information you provide, so if you are seeking to have your conditions changed it is important that you give the judge all the information you can to support your application."),
        tags$p("Due to the judge’s findings of a threat to public safety in the past 6 months, a judge cannot grant you early termination at your Probation Review Conference ",
               tags$b("at this time"),
               ". However, you are eligible for another Probation Review Conference 6 months after the date on which this threat to public safety occurred. This could be just a few days or weeks from the date of your Conference, depending on exactly when this threat occurred."),
        tags$p("In the meantime, you are always eligible to apply for early termination under ",
               tags$a(href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0', target='_blank', class='external-link', "42 P.A.C.S. § 9771"),
               ". A judge has discretion to grant such an application at any time regardless of any other issues."),
        tags$p("After your conference, the court must provide you with “written notice of the court's order detailing the court's findings.” This should contain information about the date of the threat to public safety the judge found. Remember, you are eligible for another Probation Review Conference within 6 months of the date of this threat, at which time the court can again consider changing the conditions of your probation or terminating probation early.")
      ),
    question_id = "prc_flow_q10_result"
  ),
  list(
    question =
      tags$div(
        tags$p(strong("Next, a judge will look at another type of alleged bad act you may have committed while on probation. Again, this is important because if the judge finds you have committed one of these acts, they are not allowed to grant you early termination.")),
        tags$p("At this point, the judge will look at technical violations that occurred in the past 6 months. These acts must have occurred in the 6 months before the Conference, anything older than 6 months does not count. The judge will determine if there is a preponderance of the evidence that you committed a violation of the terms of your probation in a number of different categories listed below. A “preponderance of the evidence” means that the evidence makes it more likely than not that you committed one of these acts. The categories are as follows:"),
        tags$ul(
          tags$li('"Sexual in nature"'),
          tags$li('"Assaultive behavior" or "credible threats to cause bodily injury to another"'),
          tags$li("Possession or control of firearm or dangerous weapon"),
          tags$li("Manufacture, sale, delivery, or possession with intent to sell drugs"),
          tags$li("Absconded"),
          tags$li("Unexcused and intentional failure to adhere to programming or conditions on 3 or more separate occasions")
        ),
        tags$p("The options available at a Probation Review Conference change depending on whether a judge finds that you committed one of these technical violations. Click below to see what happens next.")
      ),
    choices = c("Technical Violation Found", "No Technical Violation Found"),
    question_id = "prc_flow_q11",
    next_question = list(
      "Technical Violation Found" = "prc_flow_q11_result",
      "No Technical Violation Found" = "prc_flow_q12"
    )
  ),
  list(
    result =
      tags$div(
        tags$p("At your Probation Review Conference, a court will consider whether to change the conditions of your probation, and can reduce or increase the severity of those conditions. The judge is permitted to consider any information you provide, so if you are seeking to have your conditions changed it is important that you give the judge all the information you can to support your application."),
        tags$p("Due to the judge’s findings of a technical violation in the past 6 months, a judge cannot grant you early termination at your Probation Review Conference at this time. However, you are eligible for another Probation Review Conference 6 months after the date on which this threat to public safety occurred. This could be just a few days or weeks from the date of your Conference, depending on exactly when this threat occurred."),
        tags$p("In the meantime, you are always eligible to apply for early termination under ",
               tags$a(href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0', target='_blank', class='external-link', "42 P.A.C.S. § 9771"),
               ". A judge has discretion to grant such an application at any time regardless of any other issues."),
        tags$p("After your conference, the court must provide you with “written notice of the court's order detailing the court's findings.” This should contain information about the technical violation or violations the judge found, so you are informed of the judge’s decision-making.")
      ),
    question_id = "prc_flow_q11_result"
  ),
  list(
    question =
      tags$div(
        tags$p(strong("Next, a judge will look at other behavior during your time on probation. For this step, the judge will look at your behavior the entire time you were on probation, not just the past 6 months.")),
        tags$p("For this step, the judge will look at your behavior in two categories, for the entire time you have been on probation:"),
        tags$ol(
          type = "1",
          tags$li("Clear and convincing evidence that you “created an identifiable threat to public safety.”"),
          tags$li("A preponderance of the evidence that you have not “successfully completed all treatment or other programs required as a condition of probation,” and also that terminating probation would prevent you from continuing in the programming that the court finds is necessary for your rehabilitation, or that it would create a “substantial likelihood” that you would stop the treatment or program.")
        ),
        tags$p("As a reminder, the difference between “clear and convincing evidence” and a “preponderance of the evidence” is significant, and you should consult a lawyer with any questions about these or other legal issues."),
        tags$p("After the judge makes a finding on these two questions, they may be required to grant early termination, so click below to see what happens next."),
        tags$p("Did the judge answer ", tags$b("YES"), " to either of these two questions?")
      ),
    choices = c("Yes", "No"),
    question_id = "prc_flow_q12",
    next_question = list(
      "Yes" = "prc_flow_q12_result",
      "No" = "prc_flow_q13"
    )
  ),
  list(
    result =
      tags$div(
        tags$p("At your Probation Review Conference, a court is not required to grant you early termination. However, the judge can still grant early termination in their discretion! The judge is permitted to consider any information you provide, so if you are seeking early termination or to have your conditions changed it is important that you give the judge all the information you can to support your application. Please consult with a lawyer to discuss your options for best advocating for early termination."),
        tags$p("Also please be aware that even if the court does not grant you early termination, they can also change the conditions of your probation, and can reduce or increase the severity of those conditions. Again, it is important to present the judge with whatever information you can to best make your arguments!"),
        tags$p('If the judge does not grant you early termination at this time, you are eligible for another Probation Review Conference 1 year after this Conference. Also, the court must provide you with “written notice of the court\'s order detailing the court\'s findings.”')
      ),
    question_id = "prc_flow_q12_result"
  ),
  list(
    question =
      tags$div(
        tags$p(strong("We have only one more question to answer before determining whether the judge will be required to grant you early termination.")),
        tags$p("Have you paid all of the restitution you owe, if restitution was part of your sentence?")
      ),
    choices = c("Yes", "No"),
    question_id = "prc_flow_q13",
    next_question = list(
      "Yes" = "prc_flow_q13_result",
      "No" = "prc_flow_q14"
    )
  ),
  list(
    result =
      tags$div(
        tags$p(strong("Congratulations, the judge ", tags$b("must"), " grant you early termination of probation!"))
      ),
    question_id = "prc_flow_q13_result"
  ),
  list(
    question =
      tags$div(
        tags$p(strong("You are still eligible for early termination!")),
        tags$p("The judge has the power to grant or deny early termination, or to modify the conditions of your probation. However, because you have not paid all of your restitution, the judge is not required to grant you early termination. If you have paid at least 50% of your restitution, or if you have made a “good faith” effort to pay the restitution you owe, then it impacts what a judge is required to do."),
        tags$p("Have you paid at least 50% of your restitution, or made a “good faith” effort to pay?")
      ),
    choices = c("Yes", "No"),
    question_id = "prc_flow_q14",
    next_question = list(
      "Yes" = "prc_flow_q14_result",
      "No" = "prc_flow_q12_result"
    )
  ),
  list(
    result =
      tags$div(
        tags$p("You are still eligible for early termination! However, because you have not paid all of your restitution, the judge is not required to grant you early termination. The judge is permitted to consider any information you provide, so it is important that you give the judge all the information you can to support your application. Please consult with a lawyer to discuss your options for best advocating for early termination."),
        tags$p("Because you have paid at least 50% of your restitution, or made a “good faith” effort to pay it, if the judge does not grant you early termination they must place you on administrative probation. This means that the only terms of your probation must be as follows:"),
        tags$ol(
          type = "1",
          tags$li("Required contact with probation between 1-4 times per year"),
          tags$li('Required to notify the court of any change in address or employment “within a reasonable period of time”'),
          tags$li("Required to pay restitution “as ordered by the court on a schedule or payment plan that” you can afford to pay"),
          tags$li("Does not require fees or costs")
        )
      ),
    question_id = "prc_flow_q14_result"
  ),
  list(
    question =
      tags$div(
        tags$p("Next we need to ask you some questions about the crime or crimes for which you were sentenced. Do you know the crimes for which you were convicted? If not, you can look them up ",
               tags$a(href='https://ujsportal.pacourts.us/casesearch', target='_blank', class='external-link', "here"),
               "."),
        tags$p("Were you sentenced to probation for one of the following types of crimes?")
      ),
    question_list = list(
      "q1" = "A crime related to sex offender registration",
      "q2" = "A crime of violence", # Raw string here, no paste0(A , link)
      "q3" = "Assault or stalking against a family or household member? This only counts if they were convicted under 18 Pa.C.S. § 2701 (relating to simple assault) or 2709.1 (relating to stalking)"
    ),
    choices = c("Yes", "No"),
    question_id = "section_7_q1",
    next_question =
      list(
        "Yes" = "no_act_44_relief_result",
        "No" = "section_7_q1_1"
      )
  ),
  list(
    question =
      tags$div(
        tags$p("Now we need to ask you about your behavior while on probation. Eligibility for certain kinds of benefits under Act 44 depends on whether you had any arrests, convictions, or violations while on probation or while in custody."),
        tags$p("Were you convicted of a felony or first or second degree misdemeanor during this time? Please note that only a conviction counts here, not an arrest.")
      ),
    choices = c("Yes", "No"),
    question_id = "section_7_q1_1",
    next_question =
      list(
        "Yes" = "no_act_44_relief_result",
        "No" = "section_7_q2"
      )
  ),
  list(
    question =
      tags$div(
        tags$p(strong("Now we need to ask you about the severity of the charge that resulted in your being on probation. This is important because whether you were convicted of a felony or misdemeanor determines when you are eligible for benefits under Act 44.")),
        tags$p(
          "Were any of the charges for which you were sentenced to probation a felony? If you don't know the answer to this, you can look up it up ",
          tags$a(href='https://ujsportal.pacourts.us/casesearch', target='_blank', class='external-link', "here"),
          "."
        )
      ),
    choices = c("Yes", "No"),
    question_id = "section_7_q2",
    next_question = list(
      "Yes" = "section_7_split_sentence",
      "No" = "section_7_split_sentence"
    )
  ),
  list(
    question =
      tags$div(
        tags$p(strong("Next we need to ask you whether you were sentenced to jail or prison in addition to your probation sentence. This is important because it helps determine when you might be eligible for early termination.")),
        tags$p("Were you sentenced to jail or prison in addition to your probation sentence? For this question, only sentences to jail or prison count, not time served in jail before your sentence.")
      ),
    question_id = "section_7_split_sentence",
    choices = c("Yes", "No"),
    next_question = list(
      "Yes" = "section_7_probation_start_date",
      "No" = "section_7_q3"
    )
  ),
  list(
    question =
      tags$div(
        tags$p(strong("Next we need to ask you when you started serving your probation sentence, after you were released from jail or prison. This is important because it helps determine when you might be eligible for early termination.")),
        tags$p("On what date did you start serving your probation sentence?")
      ),
    choices = c("Yes", "No"),
    date_question = "Yes",
    question_id = "section_7_probation_start_date",
    next_question = list(
      "Yes" = "section_7_q3",
      "No" = "section_7_q3"
    )
  ),
  list(
    choices = c("Judge Finds Technical Violations", "No Technical Violations"),
    question_id = "section_7_q3",
    next_question = list(
      "Judge Finds Technical Violations" = "no_act_44_relief_result",
      "No Technical Violations" = "section_7_act_44_relief_result"
    )
  ),
  list(
    result = "You are entitled to a conference under Act 44 where a judge will determine whether their probation should be terminated or modified. That conference must be held by eligibility date",
    question_id = "section_7_act_44_relief_result"
  ),
  list(result =
         tags$div(
           tags$p(
             "You are eligible to apply for early termination of probation or for modifications of probation conditions, under ",
             tags$a(href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0', target='_blank', class='external-link', "42 P.A.C.S. § 9771"),
             ". A judge has discretion to grant or deny this application at any time. However, you are not eligible for early termination or the modification of probation conditions under Act 44. Please consult with a lawyer with any questions about your eligibility."
           )
         ),
       question_id = "no_act_44_relief_result"
  ),
  list(
    result =
      tags$div(
        tags$h2(strong("Thank you for using the Act 44 Early Termination Tool!")),
        tags$p("We hope this tool has been helpful in understanding your potential eligibility for early termination or modification of probation conditions under Act 44.")
      ),
    question_id = "finished_page"
  )
)

# --- 4. Function to process a UI element and get English & Spanish text ---
get_ui_text_pairs <- function(ui_element, mock_i18n_en, mock_i18n_es) {
  
  if (is.null(ui_element)) {
    return(NULL)
  }
  
  # Set language for English translation
  mock_i18n_en$set_translation_language("en")
  translated_en_tags <- translate_and_extract_html_text(ui_element, mock_i18n_en$t)
  english_text <- html_text(htmltools::tagList(translated_en_tags)) # Combine tags into a list if needed and get text
  
  # Set language for Spanish translation
  mock_i18n_es$set_translation_language("es")
  translated_es_tags <- translate_and_extract_html_text(ui_element, mock_i18n_es$t)
  spanish_text <- html_text(htmltools::tagList(translated_es_tags)) # Combine tags and get text
  
  return(data.frame(
    English_Sentence = english_text,
    Spanish_Translation = spanish_text,
    stringsAsFactors = FALSE
  ))
}

# --- 5. Iterate through decision_tree and collect translations ---
all_sentences_df <- data.frame(English_Sentence = character(), Spanish_Translation = character(), stringsAsFactors = FALSE)

mock_en_translator <- mock_i18n("en")
mock_es_translator <- mock_i18n("es")

for (item in decision_tree) {
  current_id <- item$question_id
  
  # Handle 'intro' elements
  if (!is.null(item$intro) && item$intro) {
    if (!is.null(item$content_above_buttons)) {
      df <- get_ui_text_pairs(item$content_above_buttons, mock_en_translator, mock_es_translator)
      all_sentences_df <- rbind(all_sentences_df, df)
    }
    if (!is.null(item$content_below_buttons)) {
      df <- get_ui_text_pairs(item$content_below_buttons, mock_en_translator, mock_es_translator)
      all_sentences_df <- rbind(all_sentences_df, df)
    }
    # Add 'Start' button text
    all_sentences_df <- rbind(all_sentences_df, data.frame(
      English_Sentence = mock_en_translator$t("Start"),
      Spanish_Translation = mock_es_translator$t("Start"),
      stringsAsFactors = FALSE
    ))
  }
  
  # Handle 'question' elements
  if (!is.null(item$question)) {
    # Special handling for question_list (e.g., section_7_q1)
    if (!is.null(item$question_list)) {
      # First, process the main question text if it exists
      df_main_q <- get_ui_text_pairs(item$question, mock_en_translator, mock_es_translator)
      all_sentences_df <- rbind(all_sentences_df, df_main_q)
      
      # Then, process each item in question_list separately
      for (q_key in names(item$question_list)) {
        en_q_text <- mock_en_translator$t(item$question_list[[q_key]])
        es_q_text <- mock_es_translator$t(item$question_list[[q_key]])
        all_sentences_df <- rbind(all_sentences_df, data.frame(
          English_Sentence = en_q_text,
          Spanish_Translation = es_q_text,
          stringsAsFactors = FALSE
        ))
      }
    } else if (is.character(item$question) && nchar(item$question) == 0) {
      # Handle cases like prc_flow_q7, prc_flow_q9 where question is empty string
      # These are dynamically generated by render_ functions in the server.
      # We need to manually reconstruct their content based on the server logic.
      
      # Using a fixed date for demonstration of dynamic content.
      # In a live app, this would depend on user input.
      sample_eligibility_date <- as.Date("2025-01-15") 
      
      if (current_id == "prc_flow_q7") {
        # Recreate the tags structure from render_prc_flow_q7 in English and Spanish contexts
        q7_tags_en <- tags$div(
          tags$p(
            paste0(mock_en_translator$t("On or before "), format_pretty_date(as.Date(sample_eligibility_date) - days(30)),
                   mock_en_translator$t(", your probation officer must serve you with a document known as a Probation Status Report. They must also serve it on your prosecutor, court, your lawyer, and any registered victim. This document is "),
                   mock_en_translator$t("critically important"), mock_en_translator$t(" for you to review carefully."))
          ),
          tags$p(mock_en_translator$t("This document must contain the following information")),
          tags$ul(
            tags$li(mock_en_translator$t("The date your PO believes you are eligible for a conference on early termination or changing probation conditions")),
            tags$li(mock_en_translator$t("Any technical violations your PO thinks you have committed within the past 6 months")),
            tags$li(mock_en_translator$t("Any criminal convictions you have had while on probation or in custody on uderlying case")),
            tags$li(mock_en_translator$t("Any programs you have completed while on probation")),
            tags$li(mock_en_translator$t("Your restitution payments, if you have any")),
            tags$li(mock_en_translator$t("A general description of your progress on probation"))
          ),
          tags$p(mock_en_translator$t("Most importantly, your probation office must make a recommendation that your probation be terminated, your conditions modified, or that you continue under your current conditions. This is critically important for reasons explained on the next screen.")),
          tags$p(
            paste0(mock_en_translator$t("If you did not receive this report by "),
                   format_pretty_date(as.Date(sample_eligibility_date) - days(30)),
                   mock_en_translator$t(", you should contact your attorney as soon as possible. It is very important to review this document, for reasons we will explain next."))
          )
        )
        
        q7_tags_es <- tags$div(
          tags$p(
            paste0(mock_es_translator$t("On or before "), format_pretty_date(as.Date(sample_eligibility_date) - days(30)),
                   mock_es_translator$t(", su oficial de libertad condicional debe entregarle un documento conocido como Informe de Estado de Libertad Condicional. También debe entregárselo a su fiscal, al tribunal, a su abogado y a cualquier víctima registrada. Esto es "),
                   mock_es_translator$t("críticamente importante"), mock_es_translator$t(" para que lo revise cuidadosamente."))
          ),
          tags$p(mock_es_translator$t("Este documento debe contener la siguiente información")),
          tags$ul(
            tags$li(mock_es_translator$t("La fecha en que su oficial de libertad condicional cree que usted es elegible para una conferencia sobre terminación anticipada o cambio de condiciones de libertad condicional")),
            tags$li(mock_es_translator$t("Cualquier infracción técnica que su oficial de libertad condicional crea que ha cometido en los últimos 6 meses")),
            tags$li(mock_es_translator$t("Cualquier condena penal que haya tenido mientras estuvo en libertad condicional o bajo custodia en el caso subyacente")),
            tags$li(mock_es_translator$t("Cualquier programa que haya completado mientras estuvo en libertad condicional")),
            tags$li(mock_es_translator$t("Sus pagos de restitución, si los tiene")),
            tags$li(mock_es_translator$t("Una descripción general de su progreso en libertad condicional"))
          ),
          tags$p(mock_es_translator$t("Lo más importante es que su oficina de libertad condicional debe hacer una recomendación para que su libertad condicional sea terminada, sus condiciones modificadas o que continúe bajo sus condiciones actuales. Esto es de vital importancia por las razones que se explican en la siguiente pantalla.")),
          tags$p(
            paste0(mock_es_translator$t("Si no recibió este informe para el "),
                   format_pretty_date(as.Date(sample_eligibility_date) - days(30)),
                   mock_es_translator$t(", debe comunicarse con su abogado lo antes posible. Es muy importante revisar este documento, por las razones que explicaremos a continuación."))
          )
        )
        
        all_sentences_df <- rbind(all_sentences_df, data.frame(
          English_Sentence = html_text(q7_tags_en),
          Spanish_Translation = html_text(q7_tags_es),
          stringsAsFactors = FALSE
        ))
        
      } else if (current_id == "prc_flow_q9") {
        # Recreate the tags structure from render_prc_flow_q9
        q9_tags_en <- tags$div(
          tags$p(
            paste0(mock_en_translator$t("Your Probation Review Conference must be held by "),
                   format_pretty_date(as.Date(sample_eligibility_date) + days(60)),
                   mock_en_translator$t(". If you do not receive your hearing by this date, you have the right to petition the court to hold this hearing within 5 days."))
          ),
          tags$p(mock_en_translator$t("During the Conference, the judge will review your behavior and record while on probation. In some cases, good behavior may lead to mandatory termination of probation. In other cases, certain types of poor behavior such as a new conviction while on probation makes early termination impossible. This tool is designed to help you understand exactly what will happen at your Probation Review Conference, and exactly what a judge is required to consider and decide. This tool will pose a series of questions that a judge will need to answer during a Probation Review Conference, and then this tool will tell you the potential outcome of your case based on the information provided. Of course, this tool cannot predict exactly what a judge will decide, but it will tell you what the outcome must be if certain factors are met or not met.")),
          tags$p(mock_en_translator$t("First, a judge will look at your criminal record while you were on probation or while you were in custody for that case. Were you convicted of a felony or a first or second degree misdemeanor during this time?"))
        )
        
        q9_tags_es <- tags$div(
          tags$p(
            paste0(mock_es_translator$t("Su Conferencia de Revisión de Libertad Condicional debe celebrarse antes del "),
                   format_pretty_date(as.Date(sample_eligibility_date) + days(60)),
                   mock_es_translator$t(". Si no recibe su audiencia para esta fecha, tiene derecho a solicitar al tribunal que celebre esta audiencia dentro de los 5 días."))
          ),
          tags$p(mock_es_translator$t("Durante la Conferencia, el juez revisará su comportamiento y registro mientras estuvo en libertad condicional. En algunos casos, el buen comportamiento puede llevar a la terminación obligatoria de la libertad condicional. En otros casos, ciertos tipos de mal comportamiento, como una nueva condena mientras está en libertad condicional, imposibilitan la terminación anticipada. Esta herramienta está diseñada para ayudarle a comprender exactamente lo que sucederá en su Conferencia de Revisión de Libertad Condicional, y exactamente lo que un juez debe considerar y decidir. Esta herramienta le presentará una serie de preguntas que un juez deberá responder durante una Conferencia de Revisión de Libertad Condicional, y luego esta herramienta le dirá el resultado potencial de su caso basándose en la información proporcionada. Por supuesto, esta herramienta no puede predecir exactamente lo que un juez decidirá, pero le dirá cuál debe ser el resultado si se cumplen o no ciertos factores.")),
          tags$p(mock_es_translator$t("Primero, un juez examinará sus antecedentes penales mientras estuvo en libertad condicional o bajo custodia por ese caso. ¿Fue condenado por un delito grave o un delito menor de primer o segundo grado durante este tiempo?"))
        )
        
        all_sentences_df <- rbind(all_sentences_df, data.frame(
          English_Sentence = html_text(q9_tags_en),
          Spanish_Translation = html_text(q9_tags_es),
          stringsAsFactors = FALSE
        ))
      } else {
        # For other cases where question is empty but not handled by specific render logic above,
        # this means there's no actual question text to extract, just transition or dynamically generated
        # within the renderUI output later.
      }
      
    } else {
      # For standard questions directly defined in 'question' field
      df <- get_ui_text_pairs(item$question, mock_en_translator, mock_es_translator)
      all_sentences_df <- rbind(all_sentences_df, df)
    }
  }
  
  # Handle 'result' elements
  if (!is.null(item$result)) {
    # Special handling for section_7_act_44_relief_result which uses a direct string and dynamic date
    if (item$question_id == "section_7_act_44_relief_result") {
      sample_eligibility_date_section_7 <- as.Date("2025-06-11") # A fixed date for demonstration
      
      # Reconstruct the text from render_section_7_act_44_relief_result
      result_tags_en <- tags$div(
        tags$p(paste0(mock_en_translator$t("On "), "<strong>", format_pretty_date(sample_eligibility_date_section_7), "</strong>", mock_en_translator$t(", a court will be required to review your case to determine whether to have your probation terminated or your conditions modified. That conference must be held by eligibility date"), mock_en_translator$t("."))),
        tags$p(mock_en_translator$t("The law also requires the judge to consider whether you have had any educational achievements or graduated from any programs. It is critically important you tell the judge if you have done so!")),
        tags$p(mock_en_translator$t("In the meantime, you are entitled to apply for early termination or to have your conditions modified under 42 P.A.C.S. § 9771 at any time. A judge has discretion to grant or deny this application at any time, for any person, even if that person is not eligible for a mandatory review under Act 44."))
      )
      
      result_tags_es <- tags$div(
        tags$p(paste0(mock_es_translator$t("El "), "<strong>", format_pretty_date(sample_eligibility_date_section_7), "</strong>", mock_es_translator$t(", un tribunal deberá revisar su caso para determinar si su libertad condicional debe ser terminada o modificada. Esa conferencia debe realizarse antes de la fecha de elegibilidad"), mock_es_translator$t("."))),
        tags$p(mock_es_translator$t("La ley también requiere que el juez considere si ha tenido algún logro educativo o se ha graduado de algún programa. ¡Es de vital importancia que le informe al juez si lo ha hecho!")),
        tags$p(mock_es_translator$t("Mientras tanto, usted tiene derecho a solicitar la terminación anticipada o a que se modifiquen sus condiciones bajo 42 P.A.C.S. § 9771 en cualquier momento. Un juez tiene discreción para conceder o denegar esta solicitud en cualquier momento, para cualquier persona, incluso si esa persona no es elegible para una revisión obligatoria bajo la Ley 44."))
      )
      
      all_sentences_df <- rbind(all_sentences_df, data.frame(
        English_Sentence = html_text(result_tags_en),
        Spanish_Translation = html_text(result_tags_es),
        stringsAsFactors = FALSE
      ))
      
    } else {
      # For other result elements (which are already tags structures)
      df <- get_ui_text_pairs(item$result, mock_en_translator, mock_es_translator)
      all_sentences_df <- rbind(all_sentences_df, df)
    }
  }
  
  # Add common button texts (Back, Next, Finish, Start Over)
  # These are generally translated directly from translation.json in the UI.
  # We add them here if they're not part of a 'result' or 'intro' text block already captured.
  if (!is.null(item$question_id) && item$question_id != "intro_page") {
    # Back button
    all_sentences_df <- rbind(all_sentences_df, data.frame(
      English_Sentence = mock_en_translator$t("Back"),
      Spanish_Translation = mock_es_translator$t("Back"),
      stringsAsFactors = FALSE
    ))
    # Next button
    all_sentences_df <- rbind(all_sentences_df, data.frame(
      English_Sentence = mock_en_translator$t("Next"),
      Spanish_Translation = mock_es_translator$t("Next"),
      stringsAsFactors = FALSE
    ))
    # Finish button (appears on result pages except 'finished_page')
    if ("result" %in% names(item) && item$question_id != "finished_page") {
      all_sentences_df <- rbind(all_sentences_df, data.frame(
        English_Sentence = mock_en_translator$t("Finish"),
        Spanish_Translation = mock_es_translator$t("Finish"),
        stringsAsFactors = FALSE
      ))
    }
  }
  if (!is.null(item$question_id) && item$question_id == "finished_page") {
    all_sentences_df <- rbind(all_sentences_df, data.frame(
      English_Sentence = mock_en_translator$t("Start Over"),
      Spanish_Translation = mock_es_translator$t("Empezar de Nuevo"),
      stringsAsFactors = FALSE
    ))
  }
}

# --- Add global UI strings not explicitly in decision_tree structures above ---
# These are the strings used directly in ui.R with i18n$t() that are not
# part of the main question/result content already processed.
global_ui_strings <- c(
  "Act 44 Early Termination Tool",
  "Change Language",
  "English",
  "Español",
  "Select a date:",
  "Select Duration",
  "Years:",
  "Months:",
  "Missing answer",
  "Please select an answer before clicking Next",
  "Leaving Application",
  "This will take you to a new website.",
  "Continue",
  "Felony", # From logic, but acts like a choice label
  "Misdemeanor", # From logic, but acts like a choice label
  "Judge Finds Technical Violations", # Button label
  "No Technical Violations",         # Button label
  "Found Threat to Public Safety",   # Button label
  "Did Not Find Threat to Public Safety", # Button label
  "Same Conduct",                    # Button label
  "Different Conduct",               # Button label
  "Technical Violation Found",       # Button label
  "No Technical Violation Found",    # Button label
  "must",                            # Used within strong/b tags, should be caught by recursive translation
  "at this time",                    # Used within strong/b tags, should be caught by recursive translation
  "Required contact with probation between 1-4 times per year", # List item text
  'Required to notify the court of any change in address or employment “within a reasonable period of time”', # List item text
  "Required to pay restitution “as ordered by the court on a schedule or payment plan that” you can afford to pay", # List item text
  "Does not require fees or costs", # List item text
  "A crime of violence", # Specific list item from section_7_q1 question_list "q2"
  "YES" # Specific text used with tags$b in prc_flow_q12
)

for (s in global_ui_strings) {
  # Only add if it's not already present to avoid duplicates
  if (!(s %in% all_sentences_df$English_Sentence)) {
    all_sentences_df <- rbind(all_sentences_df, data.frame(
      English_Sentence = mock_en_translator$t(s),
      Spanish_Translation = mock_es_translator$t(s),
      stringsAsFactors = FALSE
    ))
  }
}


# --- Post-processing: Deduplicate and clean up ---
# Remove any duplicate rows
all_sentences_df <- unique(all_sentences_df)

# Remove leading/trailing whitespace
all_sentences_df$English_Sentence <- trimws(all_sentences_df$English_Sentence)
all_sentences_df$Spanish_Translation <- trimws(all_sentences_df$Spanish_Translation)

# Filter out any rows where translation failed (e.g., "[MISSING_TRANS:...")
all_sentences_df <- all_sentences_df %>%
  filter(!grepl("^\\[MISSING_TRANS:", English_Sentence))

# --- 6. Write to CSV ---
write.csv(all_sentences_df, "translations_full_sentences.csv", row.names = FALSE, fileEncoding = "UTF-8")

message("CSV file 'translations_full_sentences.csv' has been created.")