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
library(shinyalert)
library(shiny.i18n)
library(googleLanguageR)
library(here)

####Helper Functions####
##Pretty Dates
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

####Google Cloud Authentication####
# googleLanguageR::gl_auth(here("json", "jovial-totality-455414-v4-ca642b3ae4f9.json"))

####Initialize Translator for Automatic Translation####
# i18n <- Translator$new(automatic = TRUE)
i18n <- Translator$new(translation_json_path = here("json", "translation.json"))

##Set default language
i18n$set_translation_language("en")


####Write Decision Tree####
decision_tree <- list(
  list(
    intro = TRUE,
    content_above_buttons =
      tags$div(
        tags$p(strong("Welcome to REFORM Alliance's Act 44 Early Termination tool.")),
        # Revised P tag: pass components directly, no HTML(paste0) for text-link-text combo
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
  # ... You MUST continue this pattern for every list item in your decision_tree
  #     where `HTML(paste0(...))` combines text and `<a>` tags.
  #     Also check result sections.
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
                      ".")), # Note: Strong tag now wraps the whole paragraph
        tags$p("") # This was an empty p tag, keep if intentional
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


# ####Write Decision Tree####
# decision_tree <- list(
#   list(
#     intro = TRUE,
#     content_above_buttons = 
#       tags$div(
#         tags$p(strong("Welcome to REFORM Alliance's Act 44 Early Termination tool.")),
#         tags$p(HTML(paste0("This tool is designed to help users navigate the legal requirements of ",
#                            "<a href='https://www.palegis.us/statutes/unconsolidated/law-information/view-statute?txtType=HTM&SessYr=2023&ActNum=0044.&SessInd=0' target='_blank' class='external-link'>Pennsylvania's Act 44</a>, ",
#                            "which requires courts to assess people for early termination of probation or the modification of probation conditions."))),
#         tags$p("This tool can be used by anyone, but is specifically designed to be used by people on probation. To begin using the tool, please click on the START button below.")
#       ),
#     content_below_buttons = 
#       tags$div(
#         tags$p(HTML(paste0("This tool is specifically designed to be used by people on probation or others who do not professionally work in this system. For those who work in this field, we have created other tools to help navigate every step in the Act 44 process regarding early termination and modification of probation conditions, those tools are available ",
#                            "<a href='https://reformalliance.com/pennsylvania-act-44/' target='_blank' class='external-link'>here</a>.")))
#       ),
#     next_question = "defendant_sentencing_date",
#     question_id = "intro_page"
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("First we need to ask when you were sentenced. We ask this question first because Act 44 provides very different options for you depending on when you were sentenced.")),
#         tags$p(HTML(paste0("What date were you sentenced? If you do not know, you can look it up ", 
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>here</a>.")))
#       ),
#     choices = c("Yes", "No"),
#     date_question = "Yes",
#     question_id = "defendant_sentencing_date",
#     next_question = list(
#       "Yes" = "prc_flow_q1",
#       "No" = "section_7_q1"
#     )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("Next we need to ask you some questions about the charge or charges for which you were sentenced to probation. This is important because people sentenced for certain types of crimes are eligible for different kinds of legal options.")),
#         tags$p(HTML(paste0("Do you know the crimes for which you were convicted? If not, you can look them up ",
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>here</a>."))),
#         tags$p("Were you sentenced to probation for one of the following types of crimes?"),
#         tags$ul(
#           tags$li(HTML(paste0("A ", 
#                               "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=14&subsctn=0' target='_blank' class='external-link'>Crime of Violence</a>"))),
#           tags$li(HTML(paste0("A crime related to sex offender registration (defined either ", 
#                               "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=91&subsctn=0' target='_blank' class='external-link'>here</a>", 
#                               " or ", 
#                               "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=99&subsctn=51' target='_blank' class='external-link'>here</a>)"))),
#           tags$li(HTML(paste0("A crime of ", 
#                               "<a href='https://www.legis.state.pa.us/WU01/LI/LI/CT/HTM/18/00.025..HTM' target='_blank' class='external-link'>homicide</a>", 
#                               ", including manslaughter, causing or aiding suicide, or drug delivery resulting in death"))),
#           tags$li(HTML(paste0("<a href='https://www.legis.state.pa.us/WU01/LI/LI/CT/HTM/18/00.027..HTM' target='_blank' class='external-link'>Simple assault</a>",  
#                               " committed against a family or household member"))),
#           tags$li(HTML("<a href='https://www.legis.state.pa.us/WU01/LI/LI/CT/HTM/18/00.027.009.001..HTM' target='_blank' class='external-link'>Stalking</a>"))
#         )
#       ),
#     choices = c("Yes", "No"),
#     question_id = "prc_flow_q1",
#     next_question = list(
#       "Yes" = "prc_result_1",
#       "No" = "prc_flow_split_sentence"
#     )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("Next we need to ask you whether you were sentenced to jail or prison in addition to your probation sentence. This is important because it helps determine when you might be eligible for early termination.")),
#         tags$p("Were you sentenced to jail or prison in addition to your probation sentence? For this question, only sentences to jail or prison count, not time served in jail before your sentence.")
#       ),
#     question_id = "prc_flow_split_sentence",
#     choices = c("Yes", "No"),
#     next_question = list(
#       "Yes" = "prc_flow_probation_start_date",
#       "No" = "prc_flow_q2"
#     )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("Next we need to ask you when you started serving your probation sentence, after you were released from jail or prison. This is important because it helps determine when you might be eligible for early termination.")),
#         tags$p("On what date did you start serving your probation sentence?")
#       ),
#     choices = c("Yes", "No"),
#     date_question = "Yes",
#     question_id = "prc_flow_probation_start_date",
#     next_question = list(
#       "Yes" = "prc_flow_q2",
#       "No" = "prc_flow_q2"
#     )
#   ),
#   list(
#     result = 
#       tags$div(
#         tags$p(HTML(paste0("You are eligible to apply for early termination of probation, or for modifications of probation conditions, under ",
#                            "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0' target='_blank' class='external-link'>42 P.A.C.S. § 9771</a>.",
#                            " A judge has discretion to grant or deny this application at any time. However, you are not eligible for early termination or the modification of probation conditions under Act 44. Please consult with a lawyer with any questions about your eligibility.")))
#       ),
#     question_id = "prc_result_1"
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(HTML(paste0("<strong>", "Next we need to ask you whether any of the charges that landed you on probation were felonies. This is important because it helps determine when you might be eligible for early termination.", "</strong>"))),
#         tags$p(HTML(paste("Were any of the charges for which you were sentenced to probation a felony? If not, you can look that up ",
#                           "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>here</a>.")))
#       ),
#     choices = c("Yes", "No"),
#     question_id = "prc_flow_q2",
#     next_question = list(
#       "Yes" = "prc_flow_q3_1",
#       "No" = "prc_flow_q3_2"
#     )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p("Did you start serving probation immediately after being on parole?")
#       ),
#     choices = c("Yes", "No"),
#     question_id = "prc_flow_q3_1",
#     next_question = list(
#       "Yes" = "prc_flow_q3_1_final_year_parole",
#       "No" = "prc_flow_q4"
#     )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p("Did you serve the final year of parole without any violations?")
#       ),
#     choices = c("Yes", "No"),
#     question_id = "prc_flow_q3_1_final_year_parole",
#     next_question = list(
#       "Yes" = "prc_flow_q4", 
#       "No" = "prc_flow_q4"
#     )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(HTML(paste0("<strong>", "Next we need to ask more about your case, it will help us determine when you might be eligible for early termination. Were you convicted of multiple misdemeanors that resulted in consecutive sentences? Again, if you don’t know this, you can look it up ",
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>here</a>.", "</strong>")))
#       ),
#     choices = c("Yes", "No"),
#     question_id = "prc_flow_q3_2",
#     next_question = list(
#       "Yes" = "prc_flow_q3_3",
#       "No" = "prc_flow_q4"
#     )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p("We need to ask one final question about these convictions, to help determine when you might be eligible for early termination. We need to know whether your misdemeanor convictions were for the same acts or different acts? In other words, were the convictions from one incident at one time, or unrelated incidents at different times?")
#       ),
#     choices = c("Same Conduct", "Different Conduct"),
#     question_id = "prc_flow_q3_3",
#     next_question = list(
#       "Same Conduct" = "prc_flow_q4",
#       "Different Conduct" = "prc_flow_q4"
#     )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(HTML(paste0("<strong>", "Next we need to ask about the length of the probation sentence you have. We need this information to figure out when you are eligible for a probation review conference as part of Act 44. Do you know how long your probation sentence was? If not, you can look it up ",
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>here</a>.", "</strong>"))),
#         tags$p("Please enter the probation sentence length here. If you are serving multiple probation sentences consecutively, please add the sentences together. If you served probation after serving time in jail or prison, do not include any jail or prison time here.")
#       ),
#     drop_down = TRUE,
#     drop_down_options = c("Month", "Year"),
#     question_id = "prc_flow_q4",
#     next_question = "prc_flow_q5"
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("Next we need to ask you about what you have been doing while on probation. Act 44 allows those who have finished certain programs or graduated from high school or college while on probation to be eligible for early termination earlier than others.")),
#         tags$p("Did you do any of the following things while on probation?"),
#         tags$ul(
#           tags$li("Earn a high school diploma or GED"),
#           tags$li("Earn an associate degree from an accredited university, college, seminary college, community college, or two-year college"),
#           tags$li("Earn a bachelor's degree from an accredited university, college, or seminary college"),
#           tags$li("Earn a master's or other graduate degree from an accredited university, college, or seminary college"),
#           tags$li("Obtain a vocational or occupational license, certificate, registration, or permit that was approved by your probation officer"),
#           tags$li("Complete a certified vocational, certified technical, or certified career education or training program that was approved by your probation officer"),
#           tags$li("Finish a program or condition designed to improve your life that was ordered by the court at sentencing and approved by your probation officer")
#         )
#       ),
#     choices = c("Yes", "No"),
#     question_id = "prc_flow_q5",
#     next_question = list(
#       "Yes" = "prc_flow_q6",
#       "No" = "prc_flow_q7"
#     )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p("Did you complete two or more of these achievements while on probation?")
#       ),
#     choices = c("Yes", "No"),
#     question_id = "prc_flow_q6",
#     next_question = list(
#       "Yes" = "prc_flow_q7",
#       "No" = "prc_flow_q7"
#     )
#   ),
#   list(question = "",
#        question_id = "prc_flow_q7",
#        next_question = "prc_flow_q8"),
#   list(question = 
#          tags$div(
#            tags$p(HTML(paste0("After receiving the Probation Status Report, you and the prosecutor have 30 days to object to the findings and recommendations of your probation office. This is ", 
#                               "<b>", "extremely important ", "</b>", "because if neither you nor your prosecutor object, these recommendations ", 
#                               "<b>", "must", "</b>", " be enforced. This means that if your probation office recommends early termination and your prosecutor does not object, the court must terminate your probation! But it also means that if you do not agree with your probation office’s recommendations, you should strongly consider objecting to this report, and should contact your lawyer about your options."))),
#            tags$p("If you or your prosecutor object to this report, the next step is a mandatory court procedure called a Probation Review Conference. What happens at a Probation Review Conference is extremely important because it may help you decide whether to object to this report or not. In some cases, a court will be required to grant you early termination at a Probation Review Conference. In other cases, a court will not be allowed to do so. Knowing how these conferences work will help you make the right decision about objecting to the Probation Status Report or not."),
#            tags$p("Click NEXT to find out what happens at a Probation Review Conference.")
#          ),
#        question_id = "prc_flow_q8", 
#        next_question = "prc_flow_q9"),
#   list(
#     question = "",
#     choices = c("Yes", "No"),
#     question_id = "prc_flow_q9",
#     next_question = list(
#       "Yes" = "prc_flow_q9_result",
#       "No" = "prc_flow_q10"
#     )
#   ),
#   list(
#     result = 
#       tags$div(
#         tags$p("At your Probation Review Conference, a court will consider whether to change the conditions of your probation, and can reduce or increase the severity of those conditions. The judge is permitted to consider any information you provide, so if you are seeking to have your conditions changed it is important that you give the judge all the information you can to support your application."),
#         tags$p("Due to your conviction, a judge cannot grant you early termination at your Probation Review Conference. However, you are always eligible to apply for early termination under 42 P.A.C.S. § 9771. A judge has discretion to grant such an application at any time regardless of any convictions or other issues."), 
#         tags$p("After your conference, the court must provide you with “written notice of the court's order detailing the court's findings.” You are eligible for another probation review conference within one year of the Conference, where the judge can again consider changing the conditions of your probation but cannot terminate probation early.")
#       ),
#     question_id = "prc_flow_q9_result"
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("Next, a judge will look at any alleged bad acts you have committed while on probation. This is important because if the judge finds you have committed one of these acts, they are not allowed to grant you early termination.")),
#         tags$p("First, the judge will decide whether there is clear and convincing evidence that you caused an “identifiable threat to public safety.” The terms “clear and convincing evidence” and “identifiable threat to public safety” have important legal meanings, please consult an attorney if you have any question about their meaning."),
#         tags$p("The options available at a Probation Review Conference change depending on whether a judge finds this “identifiable threat to public safety.” Click below to see what happens next.")
#       ),
#     choices = c("Found Threat to Public Safety", "Did Not Find Threat to Public Safety"),
#     question_id = "prc_flow_q10",
#     next_question = list(
#       "Found Threat to Public Safety" = "prc_flow_q10_result",
#       "Did Not Find Threat to Public Safety" = "prc_flow_q11"
#     )
#   ),
#   list(
#     result = 
#       tags$div(
#         tags$p("At your Probation Review Conference, a court will consider whether to change the conditions of your probation, and can reduce or increase the severity of those conditions. The judge is permitted to consider any information you provide, so if you are seeking to have your conditions changed it is important that you give the judge all the information you can to support your application."),
#         tags$p(HTML(paste0("Due to the judge’s findings of a threat to public safety in the past 6 months, a judge cannot grant you early termination at your Probation Review Conference ", 
#                            "<b>", "at this time", "</b>", 
#                            ". However, you are eligible for another Probation Review Conference 6 months after the date on which this threat to public safety occurred. This could be just a few days or weeks from the date of your Conference, depending on exactly when this threat occurred."))),
#         tags$p(HTML(paste0("In the meantime, you are always eligible to apply for early termination under ",
#                            "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0' target='_blank' class='external-link'>42 P.A.C.S. § 9771</a>. ", 
#                            "A judge has discretion to grant such an application at any time regardless of any other issues."))),
#         tags$p("After your conference, the court must provide you with “written notice of the court's order detailing the court's findings.” This should contain information about the date of the threat to public safety the judge found. Remember, you are eligible for another Probation Review Conference within 6 months of the date of this threat, at which time the court can again consider changing the conditions of your probation or terminating probation early.")
#       ),
#     question_id = "prc_flow_q10_result"
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("Next, a judge will look at another type of alleged bad act you may have committed while on probation. Again, this is important because if the judge finds you have committed one of these acts, they are not allowed to grant you early termination.")),
#         tags$p("At this point, the judge will look at technical violations that occurred in the past 6 months. These acts must have occurred in the 6 months before the Conference, anything older than 6 months does not count. The judge will determine if there is a preponderance of the evidence that you committed a violation of the terms of your probation in a number of different categories listed below. A “preponderance of the evidence” means that the evidence makes it more likely than not that you committed one of these acts. The categories are as follows:"),
#         tags$ul(
#           tags$li('"Sexual in nature"'), 
#           tags$li('"Assaultive behavior" or "credible threats to cause bodily injury to another"'),
#           tags$li("Possession or control of firearm or dangerous weapon"),
#           tags$li("Manufacture, sale, delivery, or possession with intent to sell drugs"),
#           tags$li("Absconded"),
#           tags$li("Unexcused and intentional failure to adhere to programming or conditions on 3 or more separate occasions")
#         ),
#         tags$p("The options available at a Probation Review Conference change depending on whether a judge finds that you committed one of these technical violations. Click below to see what happens next.")
#       ),
#     choices = c("Technical Violation Found", "No Technical Violation Found"),
#     question_id = "prc_flow_q11",
#     next_question = list(
#       "Technical Violation Found" = "prc_flow_q11_result",
#       "No Technical Violation Found" = "prc_flow_q12"
#     )
#   ),
#   list(
#     result = 
#       tags$div(
#         tags$p("At your Probation Review Conference, a court will consider whether to change the conditions of your probation, and can reduce or increase the severity of those conditions. The judge is permitted to consider any information you provide, so if you are seeking to have your conditions changed it is important that you give the judge all the information you can to support your application."),
#         tags$p(HTML(paste0("Due to the judge’s findings of a technical violation in the past 6 months, a judge cannot grant you early termination at your Probation Review Conference at this time. However, you are eligible for another Probation Review Conference 6 months after the date on which this threat to public safety occurred. This could be just a few days or weeks from the date of your Conference, depending on exactly when this threat occurred."))),
#         tags$p(HTML(paste0("In the meantime, you are always eligible to apply for early termination under ", 
#                            "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0' target='_blank' class='external-link'>42 P.A.C.S. § 9771</a>", 
#                            ". A judge has discretion to grant such an application at any time regardless of any other issues."))),
#         tags$p("After your conference, the court must provide you with “written notice of the court's order detailing the court's findings.” This should contain information about the technical violation or violations the judge found, so you are informed of the judge’s decision-making.")
#       ),
#     question_id = "prc_flow_q11_result"
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("Next, a judge will look at other behavior during your time on probation. For this step, the judge will look at your behavior the entire time you were on probation, not just the past 6 months.")),
#         tags$p("For this step, the judge will look at your behavior in two categories, for the entire time you have been on probation:"),
#         tags$ol(
#           type = "1",
#           tags$li("Clear and convincing evidence that you “created an identifiable threat to public safety.”"),
#           tags$li("A preponderance of the evidence that you have not “successfully completed all treatment or other programs required as a condition of probation,” and also that terminating probation would prevent you from continuing in the programming that the court finds is necessary for your rehabilitation, or that it would create a “substantial likelihood” that you would stop the treatment or program.")
#         ),
#         tags$p("As a reminder, the difference between “clear and convincing evidence” and a “preponderance of the evidence” is significant, and you should consult a lawyer with any questions about these or other legal issues."),
#         tags$p("After the judge makes a finding on these two questions, they may be required to grant early termination, so click below to see what happens next."),
#         tags$p(HTML("Did the judge answer <b>YES</b> to either of these two questions?"))
#       ), 
#     choices = c("Yes", "No"),
#     question_id = "prc_flow_q12",
#     next_question = list(
#       "Yes" = "prc_flow_q12_result", 
#       "No" = "prc_flow_q13"
#     )
#   ),
#   list(
#     result = 
#       tags$div(
#         tags$p("At your Probation Review Conference, a court is not required to grant you early termination. However, the judge can still grant early termination in their discretion! The judge is permitted to consider any information you provide, so if you are seeking early termination or to have your conditions changed it is important that you give the judge all the information you can to support your application. Please consult with a lawyer to discuss your options for best advocating for early termination."),
#         tags$p("Also please be aware that even if the court does not grant you early termination, they can also change the conditions of your probation, and can reduce or increase the severity of those conditions. Again, it is important to present the judge with whatever information you can to best make your arguments!"),
#         tags$p('If the judge does not grant you early termination at this time, you are eligible for another Probation Review Conference 1 year after this Conference. Also, the court must provide you with “written notice of the court\'s order detailing the court\'s findings.”')
#       ),
#     question_id = "prc_flow_q12_result"
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("We have only one more question to answer before determining whether the judge will be required to grant you early termination.")),
#         tags$p("Have you paid all of the restitution you owe, if restitution was part of your sentence?")
#       ),
#     choices = c("Yes", "No"),
#     question_id = "prc_flow_q13",
#     next_question = list(
#       "Yes" = "prc_flow_q13_result",
#       "No" = "prc_flow_q14"
#     )
#   ),
#   list(
#     result = 
#       tags$div(
#         tags$p(HTML(paste0("<strong>", "Congratulations, the judge ", "<b>", "must", "</b>", 
#                            " grant you early termination of probation!", "</strong>")))
#       ),
#     question_id = "prc_flow_q13_result"
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("You are still eligible for early termination!")),
#         tags$p("The judge has the power to grant or deny early termination, or to modify the conditions of your probation. However, because you have not paid all of your restitution, the judge is not required to grant you early termination. If you have paid at least 50% of your restitution, or if you have made a “good faith” effort to pay the restitution you owe, then it impacts what a judge is required to do."),
#         tags$p("Have you paid at least 50% of your restitution, or made a “good faith” effort to pay?")
#       ),
#     choices = c("Yes", "No"),
#     question_id = "prc_flow_q14", 
#     next_question = list(
#       "Yes" = "prc_flow_q14_result",
#       "No" = "prc_flow_q12_result"
#     )
#   ),
#   list(
#     result = 
#       tags$div(
#         tags$p("You are still eligible for early termination! However, because you have not paid all of your restitution, the judge is not required to grant you early termination. The judge is permitted to consider any information you provide, so it is important that you give the judge all the information you can to support your application. Please consult with a lawyer to discuss your options for best advocating for early termination."),
#         tags$p("Because you have paid at least 50% of your restitution, or made a “good faith” effort to pay it, if the judge does not grant you early termination they must place you on administrative probation. This means that the only terms of your probation must be as follows:"),
#         tags$ol(
#           type = "1",
#           tags$li("Required contact with probation between 1-4 times per year"),
#           tags$li('Required to notify the court of any change in address or employment “within a reasonable period of time”'),
#           tags$li("Required to pay restitution “as ordered by the court on a schedule or payment plan that” you can afford to pay"),
#           tags$li("Does not require fees or costs")
#         )
#       ),
#     question_id = "prc_flow_q14_result"
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(HTML(paste0("Next we need to ask you some questions about the crime or crimes for which you were sentenced. Do you know the crimes for which you were convicted? If not, you can look them up ",
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>here</a>."))),
#         tags$p("Were you sentenced to probation for one of the following types of crimes?")
#       ),
#     question_list = list("q1" = "A crime related to sex offender registration",
#                          "q2" = paste0("A ", "<a href='https://www.legis.state.pa.us/cfdocs/legis/LI/consCheck.cfm?txtType=HTM&ttl=42&div=0&chpt=97&sctn=14&subsctn=0' target='_blank' class='external-link'>crime of violence</a>"),
#                          "q3" = "Assault or stalking against a family or household member? This only counts if they were convicted under 18 Pa.C.S. § 2701 (relating to simple assault) or 2709.1 (relating to stalking)"),
#     choices = c("Yes", "No"),
#     question_id = "section_7_q1",
#     next_question = 
#       list(
#         "Yes" = "no_act_44_relief_result",
#         "No" = "section_7_q1_1"
#       )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p("Now we need to ask you about your behavior while on probation. Eligibility for certain kinds of benefits under Act 44 depends on whether you had any arrests, convictions, or violations while on probation or while in custody."),
#         tags$p("Were you convicted of a felony or first or second degree misdemeanor during this time? Please note that only a conviction counts here, not an arrest.")
#       ),
#     choices = c("Yes", "No"), 
#     question_id = "section_7_q1_1",
#     next_question = 
#       list(
#         "Yes" = "no_act_44_relief_result",
#         "No" = "section_7_q2"
#       )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("Now we need to ask you about the severity of the charge that resulted in your being on probation. This is important because whether you were convicted of a felony or misdemeanor determines when you are eligible for benefits under Act 44.")),
#         tags$p(HTML(paste0("Were any of the charges for which you were sentenced to probation a felony? If you don't know the answer to this, you can look up it up ",
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>here</a>.")))
#       ),
#     choices = c("Yes", "No"),
#     question_id = "section_7_q2",
#     next_question = list(
#       "Yes" = "section_7_split_sentence",
#       "No" = "section_7_split_sentence"
#     )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("Next we need to ask you whether you were sentenced to jail or prison in addition to your probation sentence. This is important because it helps determine when you might be eligible for early termination.")),
#         tags$p("Were you sentenced to jail or prison in addition to your probation sentence? For this question, only sentences to jail or prison count, not time served in jail before your sentence.")
#       ),
#     question_id = "section_7_split_sentence",
#     choices = c("Yes", "No"),
#     next_question = list(
#       "Yes" = "section_7_probation_start_date",
#       "No" = "section_7_q3"
#     )
#   ),
#   list(
#     question = 
#       tags$div(
#         tags$p(strong("Next we need to ask you when you started serving your probation sentence, after you were released from jail or prison. This is important because it helps determine when you might be eligible for early termination.")),
#         tags$p("On what date did you start serving your probation sentence?")
#       ),
#     choices = c("Yes", "No"),
#     date_question = "Yes",
#     question_id = "section_7_probation_start_date",
#     next_question = list(
#       "Yes" = "section_7_q3",
#       "No" = "section_7_q3"
#     )
#   ),
#   list(
#     choices = c("Judge Finds Technical Violations", "No Technical Violations"),
#     question_id = "section_7_q3",
#     next_question = list(
#       "Judge Finds Technical Violations" = "no_act_44_relief_result",
#       "No Technical Violations" = "section_7_act_44_relief_result"
#     )
#   ),
#   list(
#     result = "You are entitled to a conference under Act 44 where a judge will determine whether their probation should be terminated or modified. That conference must be held by eligibility date",
#     question_id = "section_7_act_44_relief_result"
#   ),
#   list(result = 
#          tags$div(
#            tags$p(HTML(paste0("You are eligible to apply for early termination of probation or for modifications of probation conditions, under ",
#                               "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0' target='_blank' class='external-link'>42 P.A.C.S. § 9771</a>.", 
#                               " A judge has discretion to grant or deny this application at any time. However, you are not eligible for early termination or the modification of probation conditions under Act 44. Please consult with a lawyer with any questions about your eligibility.")))
#          ),
#        question_id = "no_act_44_relief_result"
#   ),
#   list(
#     result =
#       tags$div(
#         tags$h2(strong("Thank you for using the Act 44 Early Termination Tool!")),
#         tags$p("We hope this tool has been helpful in understanding your potential eligibility for early termination or modification of probation conditions under Act 44.")
#       ),
#     question_id = "finished_page"
#   )
# )
# 

# ####Write Decision Tree####
# decision_tree <- list(
#   list(
#     intro = TRUE,
#     content_above_buttons =
#       tags$div(
#         tags$p(strong(i18n$t("Welcome to REFORM Alliance's Act 44 Early Termination tool."))),
#         tags$p(HTML(paste0(i18n$t("This tool is designed to help users navigate the legal requirements of "),
#                            "<a href='https://www.palegis.us/statutes/unconsolidated/law-information/view-statute?txtType=HTM&SessYr=2023&ActNum=0044.&SessInd=0' target='_blank' class='external-link'>", i18n$t("Pennsylvania's Act 44"), "</a>",
#                            i18n$t(", which requires courts to assess people for early termination of probation or the modification of probation conditions.")))),
#         tags$p(i18n$t("This tool can be used by anyone, but is specifically designed to be used by people on probation. To begin using the tool, please click on the START button below."))
#       ),
#     content_below_buttons =
#       tags$div(
#         tags$p(HTML(paste0(i18n$t("This tool is specifically designed to be used by people on probation or others who do not professionally work in this system. For those who work in this field, we have created other tools to help navigate every step in the Act 44 process regarding early termination and modification of probation conditions, those tools are available "),
#                            "<a href='https://reformalliance.com/pennsylvania-act-44/' target='_blank' class='external-link'>", i18n$t("here"), "</a>",
#                            i18n$t("."))))
#       ),
#     next_question = "defendant_sentencing_date",
#     question_id = "intro_page"
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("First we need to ask when you were sentenced. We ask this question first because Act 44 provides very different options for you depending on when you were sentenced."))),
#         tags$p(HTML(paste0(i18n$t("What date were you sentenced? If you do not know, you can look it up "),
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>", i18n$t("here"), "</a>",
#                            i18n$t("."))))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     date_question = "Yes",
#     question_id = "defendant_sentencing_date",
#     next_question = list(
#       "Yes" = "prc_flow_q1",
#       "No" = "section_7_q1"
#     )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("Next we need to ask you some questions about the charge or charges for which you were sentenced to probation. This is important because people sentenced for certain types of crimes are eligible for different kinds of legal options."))),
#         tags$p(HTML(paste0(i18n$t("Do you know the crimes for which you were convicted? If not, you can look them up "),
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>", i18n$t("here"), "</a>",
#                            i18n$t(".")))),
#         tags$p(i18n$t("Were you sentenced to probation for one of the following types of crimes?")),
#         tags$ul(
#           tags$li(HTML(paste0(i18n$t("A "),
#                               "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=14&subsctn=0' target='_blank' class='external-link'>", i18n$t("Crime of Violence"), "</a>"))),
#           tags$li(HTML(paste0(i18n$t("A crime related to sex offender registration(defined either "),
#                               "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=91&subsctn=0' target='_blank' class='external-link'>", i18n$t("here"), "</a>",
#                               i18n$t(" or "),
#                               "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=99&subsctn=51' target='_blank' class='external-link'>", i18n$t("here"), "</a>",
#                               ")"))),
#           tags$li(HTML(paste0(i18n$t("A crime of "),
#                               "<a href='https://www.legis.state.pa.us/WU01/LI/LI/CT/HTM/18/00.025..HTM' target='_blank' class='external-link'>", i18n$t("homicide"), "</a>",
#                               i18n$t(", including manslaughter, causing or aiding suicide, or drug delivery resulting in death")))),
#           tags$li(HTML(paste0("<a href='https://www.legis.state.pa.us/WU01/LI/LI/CT/HTM/18/00.027..HTM' target='_blank' class='external-link'>", i18n$t("Simple assault"), "</a>",
#                               i18n$t(" committed against a family or household member")))),
#           tags$li(HTML(paste0("<a href='https://www.legis.state.pa.us/WU01/LI/LI/CT/HTM/18/00.027.009.001..HTM' target='_blank' class='external-link'>", i18n$t("Stalking"), "</a>")))
#         )
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "prc_flow_q1",
#     next_question = list(
#       "Yes" = "prc_result_1",
#       "No" = "prc_flow_split_sentence"
#     )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("Next we need to ask you whether you were sentenced to jail or prison in addition to your probation sentence. This is important because it helps determine when you might be eligible for early termination."))),
#         tags$p(i18n$t("Were you sentenced to jail or prison in addition to your probation sentence? For this question, only sentences to jail or prison count, not time served in jail before your sentence."))
#       ),
#     question_id = "prc_flow_split_sentence",
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     next_question = list(
#       "Yes" = "prc_flow_probation_start_date",
#       "No" = "prc_flow_q2"
#     )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("Next we need to ask you when you started serving your probation sentence, after you were released from jail or prison. This is important because it helps determine when you might be eligible for early termination."))),
#         tags$p(i18n$t("On what date did you start serving your probation sentence?"))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     date_question = "Yes",
#     question_id = "prc_flow_probation_start_date",
#     next_question = list(
#       "Yes" = "prc_flow_q2",
#       "No" = "prc_flow_q2"
#     )
#   ),
#   list(
#     result =
#       tags$div(
#         tags$p(HTML(paste0(i18n$t("You are eligible to apply for early termination of probation, or for modifications of probation conditions, under "),
#                            "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0' target='_blank' class='external-link'>", i18n$t("42 P.A.C.S. § 9771"), "</a>",
#                            i18n$t(". A judge has discretion to grant or deny this application at any time. However, you are not eligible for early termination or the modification of probation conditions under Act 44. Please consult with a lawyer with any questions about your eligibility."))))
#       ),
#     question_id = "prc_result_1"
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(HTML(paste0("<strong>", i18n$t("Next we need to ask you whether any of the charges that landed you on probation were felonies. This is important because it helps determine when you might be eligible for early termination."), "</strong>"))),
#         tags$p(HTML(paste(i18n$t("Were any of the charges for which you were sentenced to probation a felony? If not, you can look that up "),
#                           "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>", i18n$t("here"), "</a>",
#                           i18n$t("."))))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "prc_flow_q2",
#     next_question = list(
#       "Yes" = "prc_flow_q3_1",
#       "No" = "prc_flow_q3_2"
#     )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(i18n$t("Did you start serving probation immediately after being on parole?"))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "prc_flow_q3_1",
#     next_question = list(
#       "Yes" = "prc_flow_q3_1_final_year_parole",
#       "No" = "prc_flow_q4"
#     )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(i18n$t("Did you serve the final year of parole without any violations?"))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "prc_flow_q3_1_final_year_parole",
#     next_question = list(
#       "Yes" = "prc_flow_q4",
#       "No" = "prc_flow_q4"
#     )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(HTML(paste0("<strong>", i18n$t("Next we need to ask more about your case, it will help us determine when you might be eligible for early termination. Were you convicted of multiple misdemeanors that resulted in consecutive sentences? Again, if you don’t know this, you can look it up "),
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>", i18n$t("here"), "</a>",
#                            i18n$t("."), "</strong>"))),
#         tags$p(i18n$t("")) 
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "prc_flow_q3_2",
#     next_question = list(
#       "Yes" = "prc_flow_q3_3",
#       "No" = "prc_flow_q4"
#     )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(i18n$t("We need to ask one final question about these convictions, to help determine when you might be eligible for early termination. We need to know whether your misdemeanor convictions were for the same acts or different acts? In other words, were the convictions from one incident at one time, or unrelated incidents at different times?"))
#       ),
#     choices = c(i18n$t("Same Conduct"), i18n$t("Different Conduct")),
#     question_id = "prc_flow_q3_3",
#     next_question = list(
#       "Same Conduct" = "prc_flow_q4",
#       "Different Conduct" = "prc_flow_q4"
#     )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(HTML(paste0("<strong>", i18n$t("Next we need to ask about the length of the probation sentence you have. We need this information to figure out when you are eligible for a probation review conference as part of Act 44. Do you know how long your probation sentence was? If not, you can look it up "),
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>", i18n$t("here"), "</a>",
#                            i18n$t("."), "</strong>"))),
#         tags$p(i18n$t("Please enter the probation sentence length here. If you are serving multiple probation sentences consecutively, please add the sentences together. If you served probation after serving time in jail or prison, do not include any jail or prison time here."))
#       ),
#     drop_down = TRUE,
#     drop_down_options = c(i18n$t("Month"), i18n$t("Year")), 
#     question_id = "prc_flow_q4",
#     next_question = "prc_flow_q5"
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("Next we need to ask you about what you have been doing while on probation. Act 44 allows those who have finished certain programs or graduated from high school or college while on probation to be eligible for early termination earlier than others."))),
#         tags$p(i18n$t("Did you do any of the following things while on probation?")),
#         tags$ul(
#           tags$li(i18n$t("Earn a high school diploma or GED")),
#           tags$li(i18n$t("Earn an associate degree from an accredited university, college, seminary college, community college, or two-year college")),
#           tags$li(i18n$t("Earn a bachelor's degree from an accredited university, college, or seminary college")),
#           tags$li(i18n$t("Earn a master's or other graduate degree from an accredited university, college, or seminary college")),
#           tags$li(i18n$t("Obtain a vocational or occupational license, certificate, registration, or permit that was approved by your probation officer")),
#           tags$li(i18n$t("Complete a certified vocational, certified technical, or certified career education or training program that was approved by your probation officer")),
#           tags$li(i18n$t("Finish a program or condition designed to improve your life that was ordered by the court at sentencing and approved by your probation officer"))
#         )
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "prc_flow_q5",
#     next_question = list(
#       "Yes" = "prc_flow_q6",
#       "No" = "prc_flow_q7"
#     )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(i18n$t("Did you complete two or more of these achievements while on probation?"))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "prc_flow_q6",
#     next_question = list(
#       "Yes" = "prc_flow_q7",
#       "No" = "prc_flow_q7"
#     )
#   ),
#   list(question = "", 
#        question_id = "prc_flow_q7",
#        next_question = "prc_flow_q8"),
#   list(question =
#          tags$div(
#            tags$p(HTML(paste0(i18n$t("After receiving the Probation Status Report, you and the prosecutor have 30 days to object to the findings and recommendations of your probation office. This is "),
#                               "<b>", i18n$t("extremely important"), "</b>", i18n$t(" because if neither you nor your prosecutor object, these recommendations "),
#                               "<b>", i18n$t("must"), "</b>", i18n$t(" be enforced. This means that if your probation office recommends early termination and your prosecutor does not object, the court must terminate your probation! But it also means that if you do not agree with your probation office’s recommendations, you should strongly consider objecting to this report, and should contact your lawyer about your options.")))),
#            tags$p(i18n$t("If you or your prosecutor object to this report, the next step is a mandatory court procedure called a Probation Review Conference. What happens at a Probation Review Conference is extremely important because it may help you decide whether to object to this report or not. In some cases, a court will be required to grant you early termination at a Probation Review Conference. In other cases, a court will not be allowed to do so. Knowing how these conferences work will help you make the right decision about objecting to the Probation Status Report or not.")),
#            tags$p(i18n$t("Click NEXT to find out what happens at a Probation Review Conference."))
#          ),
#        question_id = "prc_flow_q8",
#        next_question = "prc_flow_q9"),
#   list(
#     question = "", 
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "prc_flow_q9",
#     next_question = list(
#       "Yes" = "prc_flow_q9_result",
#       "No" = "prc_flow_q10"
#     )
#   ),
#   list(
#     result =
#       tags$div(
#         tags$p(i18n$t("At your Probation Review Conference, a court will consider whether to change the conditions of your probation, and can reduce or increase the severity of those conditions. The judge is permitted to consider any information you provide, so if you are seeking to have your conditions changed it is important that you give the judge all the information you can to support your application.")),
#         tags$p(i18n$t("Due to your conviction, a judge cannot grant you early termination at your Probation Review Conference. However, you are always eligible to apply for early termination under 42 P.A.C.S. § 9771. A judge has discretion to grant such an application at any time regardless of any convictions or other issues.")),
#         tags$p(i18n$t("After your conference, the court must provide you with “written notice of the court's order detailing the court's findings.” You are eligible for another probation review conference within one year of the Conference, where the judge can again consider changing the conditions of your probation but cannot terminate probation early."))
#       ),
#     question_id = "prc_flow_q9_result"
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("Next, a judge will look at any alleged bad acts you have committed while on probation. This is important because if the judge finds you have committed one of these acts, they are not allowed to grant you early termination."))),
#         tags$p(i18n$t("First, the judge will decide whether there is clear and convincing evidence that you caused an “identifiable threat to public safety.” The terms “clear and convincing evidence” and “identifiable threat to public safety” have important legal meanings, please consult an attorney if you have any question about their meaning.")),
#         tags$p(i18n$t("The options available at a Probation Review Conference change depending on whether a judge finds this “identifiable threat to public safety.” Click below to see what happens next."))
#       ),
#     choices = c(i18n$t("Found Threat to Public Safety"), i18n$t("Did Not Find Threat to Public Safety")),
#     question_id = "prc_flow_q10",
#     next_question = list(
#       "Found Threat to Public Safety" = "prc_flow_q10_result",
#       "Did Not Find Threat to Public Safety" = "prc_flow_q11"
#     )
#   ),
#   list(
#     result =
#       tags$div(
#         tags$p(i18n$t("At your Probation Review Conference, a court will consider whether to change the conditions of your probation, and can reduce or increase the severity of those conditions. The judge is permitted to consider any information you provide, so if you are seeking to have your conditions changed it is important that you give the judge all the information you can to support your application.")),
#         tags$p(HTML(paste0(i18n$t("Due to the judge’s findings of a threat to public safety in the past 6 months, a judge cannot grant you early termination at your Probation Review Conference "),
#                            "<b>", i18n$t("at this time"), "</b>",
#                            i18n$t(". However, you are eligible for another Probation Review Conference 6 months after the date on which this threat to public safety occurred. This could be just a few days or weeks from the date of your Conference, depending on exactly when this threat occurred.")))),
#         tags$p(HTML(paste0(i18n$t("In the meantime, you are always eligible to apply for early termination under "),
#                            "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0' target='_blank' class='external-link'>", i18n$t("42 P.A.C.S. § 9771"), "</a>",
#                            i18n$t(". A judge has discretion to grant such an application at any time regardless of any other issues.")))),
#         tags$p(i18n$t("After your conference, the court must provide you with “written notice of the court's order detailing the court's findings.” This should contain information about the date of the threat to public safety the judge found. Remember, you are eligible for another Probation Review Conference within 6 months of the date of this threat, at which time the court can again consider changing the conditions of your probation or terminating probation early."))
#       ),
#     question_id = "prc_flow_q10_result"
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("Next, a judge will look at another type of alleged bad act you may have committed while on probation. Again, this is important because if the judge finds you have committed one of these acts, they are not allowed to grant you early termination."))),
#         tags$p(i18n$t("At this point, the judge will look at technical violations that occurred in the past 6 months. These acts must have occurred in the 6 months before the Conference, anything older than 6 months does not count. The judge will determine if there is a preponderance of the evidence that you committed a violation of the terms of your probation in a number of different categories listed below. A “preponderance of the evidence” means that the evidence makes it more likely than not that you committed one of these acts. The categories are as follows:")),
#         tags$ul(
#           tags$li(i18n$t("\"Sexual in nature\"")),
#           tags$li(i18n$t("\"Assaultive behavior\" or \"credible threats to cause bodily injury to another\"")),
#           tags$li(i18n$t("Possession or control of firearm or dangerous weapon")),
#           tags$li(i18n$t("Manufacture, sale, delivery, or possession with intent to sell drugs")),
#           tags$li(i18n$t("Absconded")),
#           tags$li(i18n$t("Unexcused and intentional failure to adhere to programming or conditions on 3 or more separate occasions"))
#         ),
#         tags$p(i18n$t("The options available at a Probation Review Conference change depending on whether a judge finds that you committed one of these technical violations. Click below to see what happens next."))
#       ),
#     choices = c(i18n$t("Technical Violation Found"), i18n$t("No Technical Violation Found")),
#     question_id = "prc_flow_q11",
#     next_question = list(
#       "Technical Violation Found" = "prc_flow_q11_result",
#       "No Technical Violation Found" = "prc_flow_q12"
#     )
#   ),
#   list(
#     result =
#       tags$div(
#         tags$p(i18n$t("At your Probation Review Conference, a court will consider whether to change the conditions of your probation, and can reduce or increase the severity of those conditions. The judge is permitted to consider any information you provide, so if you are seeking to have your conditions changed it is important that you give the judge all the information you can to support your application.")),
#         tags$p(HTML(paste0(i18n$t("Due to the judge’s findings of a technical violation in the past 6 months, a judge cannot grant you early termination at your Probation Review Conference at this time. However, you are eligible for another Probation Review Conference 6 months after the date on which this threat to public safety occurred. This could be just a few days or weeks from the date of your Conference, depending on exactly when this threat occurred.")))),
#         tags$p(HTML(paste0(i18n$t("In the meantime, you are always eligible to apply for early termination under "),
#                            "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0' target='_blank' class='external-link'>", i18n$t("42 P.A.C.S. § 9771"), "</a>",
#                            i18n$t(". A judge has discretion to grant such an application at any time regardless of any other issues.")))),
#         tags$p(i18n$t("After your conference, the court must provide you with “written notice of the court's order detailing the court's findings.” This should contain information about the technical violation or violations the judge found, so you are informed of the judge’s decision-making."))
#       ),
#     question_id = "prc_flow_q11_result"
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("Next, a judge will look at other behavior during your time on probation. For this step, the judge will look at your behavior the entire time you were on probation, not just the past 6 months."))),
#         tags$p(i18n$t("For this step, the judge will look at your behavior in two categories, for the entire time you have been on probation:")),
#         tags$ol(
#           type = "1",
#           tags$li(i18n$t("Clear and convincing evidence that you “created an identifiable threat to public safety.”")),
#           tags$li(i18n$t("A preponderance of the evidence that you have not “successfully completed all treatment or other programs required as a condition of probation,” and also that terminating probation would prevent you from continuing in the programming that the court finds is necessary for your rehabilitation, or that it would create a “substantial likelihood” that you would stop the treatment or program."))
#         ),
#         tags$p(i18n$t("As a reminder, the difference between “clear and convincing evidence” and a “preponderance of the evidence” is significant, and you should consult a lawyer with any questions about these or other legal issues.")),
#         tags$p(i18n$t("After the judge makes a finding on these two questions, they may be required to grant early termination, so click below to see what happens next.")),
#         tags$p(HTML(i18n$t("Did the judge answer <b>YES</b> to either of these two questions?")))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "prc_flow_q12",
#     next_question = list(
#       "Yes" = "prc_flow_q12_result",
#       "No" = "prc_flow_q13"
#     )
#   ),
#   list(
#     result =
#       tags$div(
#         tags$p(i18n$t("At your Probation Review Conference, a court is not required to grant you early termination. However, the judge can still grant early termination in their discretion! The judge is permitted to consider any information you provide, so if you are seeking early termination or to have your conditions changed it is important that you give the judge all the information you can to support your application. Please consult with a lawyer to discuss your options for best advocating for early termination.")),
#         tags$p(i18n$t("Also please be aware that even if the court does not grant you early termination, they can also change the conditions of your probation, and can reduce or increase the severity of those conditions. Again, it is important to present the judge with whatever information you can to best make your arguments!")),
#         tags$p(i18n$t("If the judge does not grant you early termination at this time, you are eligible for another Probation Review Conference 1 year after this Conference. Also, the court must provide you with “written notice of the court's order detailing the court's findings.”"))
#       ),
#     question_id = "prc_flow_q12_result"
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("We have only one more question to answer before determining whether the judge will be required to grant you early termination."))),
#         tags$p(i18n$t("Have you paid all of the restitution you owe, if restitution was part of your sentence?"))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "prc_flow_q13",
#     next_question = list(
#       "Yes" = "prc_flow_q13_result",
#       "No" = "prc_flow_q14"
#     )
#   ),
#   list(
#     result =
#       tags$div(
#         tags$p(HTML(paste0("<strong>", i18n$t("Congratulations, the judge <b>must</b> grant you early termination of probation!"), "</strong>")))
#       ),
#     question_id = "prc_flow_q13_result"
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("You are still eligible for early termination!"))),
#         tags$p(i18n$t("The judge has the power to grant or deny early termination, or to modify the conditions of your probation. However, because you have not paid all of your restitution, the judge is not required to grant you early termination. If you have paid at least 50% of your restitution, or if you have made a “good faith” effort to pay the restitution you owe, then it impacts what a judge is required to do.")),
#         tags$p(i18n$t("Have you paid at least 50% of your restitution, or made a “good faith” effort to pay?"))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "prc_flow_q14",
#     next_question = list(
#       "Yes" = "prc_flow_q14_result",
#       "No" = "prc_flow_q12_result"
#     )
#   ),
#   list(
#     result =
#       tags$div(
#         tags$p(i18n$t("You are still eligible for early termination! However, because you have not paid all of your restitution, the judge is not required to grant you early termination. The judge is permitted to consider any information you provide, so it is important that you give the judge all the information you can to support your application. Please consult with a lawyer to discuss your options for best advocating for early termination.")),
#         tags$p(i18n$t("Because you have paid at least 50% of your restitution, or made a “good faith” effort to pay it, if the judge does not grant you early termination they must place you on administrative probation. This means that the only terms of your probation must be as follows:")),
#         tags$ol(
#           type = "1",
#           tags$li(i18n$t("Required contact with probation between 1-4 times per year")),
#           tags$li(i18n$t('Required to notify the court of any change in address or employment “within a reasonable period of time”')),
#           tags$li(i18n$t("Required to pay restitution “as ordered by the court on a schedule or payment plan that” you can afford to pay")),
#           tags$li(i18n$t("Does not require fees or costs"))
#         )
#       ),
#     question_id = "prc_flow_q14_result"
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(HTML(paste0(i18n$t("Next we need to ask you some questions about the crime or crimes for which you were sentenced. Do you know the crimes for which you were convicted? If not, you can look them up "),
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>", i18n$t("here"), "</a>",
#                            i18n$t("."))))),
#     tags$p(i18n$t("Were you sentenced to probation for one of the following types of crimes?")),
#     question_list = list("q1" = i18n$t("A crime related to sex offender registration"),
#                          "q2" = HTML(paste0(i18n$t("A "), "<a href='https://www.legis.state.pa.us/cfdocs/legis/LI/consCheck.cfm?txtType=HTM&ttl=42&div=0&chpt=97&sctn=14&subsctn=0' target='_blank' class='external-link'>", i18n$t("crime of violence"), "</a>")),
#                          "q3" = i18n$t("Assault or stalking against a family or household member? This only counts if they were convicted under 18 Pa.C.S. § 2701(relating to simple assault) or 2709.1(relating to stalking)")),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "section_7_q1",
#     next_question =
#       list(
#         "Yes" = "no_act_44_relief_result",
#         "No" = "section_7_q1_1"
#       )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(i18n$t("Now we need to ask you about your behavior while on probation. Eligibility for certain kinds of benefits under Act 44 depends on whether you had any arrests, convictions, or violations while on probation or while in custody.")),
#         tags$p(i18n$t("Were you convicted of a felony or first or second degree misdemeanor during this time? Please note that only a conviction counts here, not an arrest."))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "section_7_q1_1",
#     next_question =
#       list(
#         "Yes" = "no_act_44_relief_result",
#         "No" = "section_7_q2"
#       )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("Now we need to ask you about the severity of the charge that resulted in your being on probation. This is important because whether you were convicted of a felony or misdemeanor determines when you are eligible for benefits under Act 44."))),
#         tags$p(HTML(paste0(i18n$t("Were any of the charges for which you were sentenced to probation a felony? If you don't know the answer to this, you can look up it up "),
#                            "<a href='https://ujsportal.pacourts.us/casesearch' target='_blank' class='external-link'>", i18n$t("here"), "</a>",
#                            i18n$t("."))))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     question_id = "section_7_q2",
#     next_question = list(
#       "Yes" = "section_7_split_sentence",
#       "No" = "section_7_split_sentence"
#     )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("Next we need to ask you whether you were sentenced to jail or prison in addition to your probation sentence. This is important because it helps determine when you might be eligible for early termination."))),
#         tags$p(i18n$t("Were you sentenced to jail or prison in addition to your probation sentence? For this question, only sentences to jail or prison count, not time served in jail before your sentence."))
#       ),
#     question_id = "section_7_split_sentence",
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     next_question = list(
#       "Yes" = "section_7_probation_start_date",
#       "No" = "section_7_q3"
#     )
#   ),
#   list(
#     question =
#       tags$div(
#         tags$p(strong(i18n$t("Next we need to ask you when you started serving your probation sentence, after you were released from jail or prison. This is important because it helps determine when you might be eligible for early termination."))),
#         tags$p(i18n$t("On what date did you start serving your probation sentence?"))
#       ),
#     choices = c(i18n$t("Yes"), i18n$t("No")),
#     date_question = "Yes",
#     question_id = "section_7_probation_start_date",
#     next_question = list(
#       "Yes" = "section_7_q3",
#       "No" = "section_7_q3"
#     )
#   ),
#   list(
#     choices = c(i18n$t("Judge Finds Technical Violations"), i18n$t("No Technical Violations")),
#     question_id = "section_7_q3",
#     next_question = list(
#       "Judge Finds Technical Violations" = "no_act_44_relief_result",
#       "No Technical Violations" = "section_7_act_44_relief_result"
#     )
#   ),
#   list(
#     result = i18n$t("You are entitled to a conference under Act 44 where a judge will determine whether their probation should be terminated or modified. That conference must be held by eligibility date"),
#     question_id = "section_7_act_44_relief_result"
#   ),
#   list(result =
#          tags$div(
#            tags$p(HTML(paste0(i18n$t("You are eligible to apply for early termination of probation or for modifications of probation conditions, under "),
#                               "<a href='https://www.palegis.us/statutes/consolidated/view-statute?txtType=HTM&ttl=42&div=0&chapter=97&section=71&subsctn=0' target='_blank' class='external-link'>", i18n$t("42 P.A.C.S. § 9771"), "</a>",
#                               i18n$t(". A judge has discretion to grant or deny this application at any time. However, you are not eligible for early termination or the modification of probation conditions under Act 44. Please consult with a lawyer with any questions about your eligibility."))))
#          ),
#        question_id = "no_act_44_relief_result"
#   ),
#   list(
#     result =
#       tags$div(
#         tags$h2(strong(i18n$t("Thank you for using the Act 44 Early Termination Tool!"))),
#         tags$p(i18n$t("We hope this tool has been helpful in understanding your potential eligibility for early termination or modification of probation conditions under Act 44."))
#       ),
#     question_id = "finished_page"
#   )
# )
# 

####Create App####
ui <- fluidPage(
  usei18n(i18n),
  title = i18n$t("Act 44 Early Termination Tool"),
  useShinyjs(),
  theme = shinytheme("flatly"),  
  tags$head(
    tags$style(HTML(
      "
      .question-text{ font-size: 20px; margin-bottom: 30px; margin-top: 30px}
      .result-text{ font-size: 20px; font-weight: bold; color: #28a745; margin-top: 20px; }
      .shiny-input-container{ margin-bottom: 10px; }
      .btn-container{ margin-top: 20px; }
      .logo-container{ text-align: center; margin-bottom: 40px; margin-top: 10px;}/* Added margin-top */
      .logo{ max-width: 200px; height: auto; }

      /* Custom button styling */
      #violation-buttons .btn{
        margin-right: 10px;
        margin-bottom: 10px; /* Add bottom margin for spacing */
        border: 1px solid #ccc;
        padding: 10px 15px;
        font-size: 16px;
        min-width: 200px; /* Adjust as needed */
        text-align: center;
      }

      #violation-buttons .btn.active{
        border-color: #007bff;
        box-shadow: 0 0 5px rgba(0, 123, 255, 0.5);
      }

      #violation-buttons .btn-danger{
        background-color: #dc3545;
        color: white;
      }

      #violation-buttons .btn-success {
        background-color: #28a745;
        color: white;
        min-width: 250px;
        height: 60px;
        padding: 0 20px;     /* Horizontal padding only */
        font-size: 18px;
        line-height: 60px;
        text-align: center;
      }
      
      #start_button {
        font-size: 24px; /* Increase font size */
        padding: 15px 30px; /* Increase padding */
        min-width: 280px; /* Adjust width as needed */
        height: 70px;    /* Adjust height as needed */
        display: flex;        /* Enable flexbox */
        justify-content: center; /* Center horizontally */
        align-items: center;    /* Center vertically */
      }
      "
    )),
    
    tags$script(HTML("
    $(document).on('click', 'a.external-link', function(e) {
      e.preventDefault();
      var $this = $(this);
      var href = $this.attr('href');
      $this.data('href-backup', href); // Backup the href
      Shiny.setInputValue('js_href', href);
    });

    Shiny.addCustomMessageHandler('open_url', function(message) {
      window.open(message, '_blank');
      // No need to restore here, navigation happens
    });

    Shiny.addCustomMessageHandler('remove_href', function(href) {
      $('a.external-link[data-href-backup=\"' + href + '\"]').each(function() {
        $(this).removeAttr('href'); // Temporarily remove href
      });
    });

    Shiny.addCustomMessageHandler('restore_href', function(href) {
      $('a.external-link[data-href-backup=\"' + href + '\"]').each(function() {
        $(this).attr('href', $(this).data('href-backup'));
        $(this).removeData('href-backup');
      });
    });
    "))
  ),
  
  div(class = "logo-container",
      img(src = "reform-logo-charcoal.png", class = "logo")
  ),
  
  div(strong(i18n$t("Act 44 Early Termination Tool")),
      style = "font-size: 40px; margin-top: 60px; margin-bottom: 40px; text-align: center;"
  ),
  
  div(
    class = "language-picker-container", 
    pickerInput(
      inputId = "selected_language",
      label = i18n$t("Change Language"),
      choices = c("en", "es"), 
      selected = i18n$get_translation_language(), 
      choicesOpt = list(
        content = c(
          paste0("<i class='flag-icon flag-icon-us'></i> ", i18n$t("English")), 
          paste0("<i class='flag-icon flag-icon-es'></i> ", i18n$t("Español"))
          # Alternatively for emojis(less robust cross-platform for consistent appearance):
          # paste0("🇺🇸 ", i18n$t("English")),
          # paste0("🇪🇸 ", i18n$t("Español"))
        )
      ),
      options = list(
        style = "btn-primary", # Apply a Bootstrap button style
        `live-search` = FALSE, # No search box needed for two options
        `selected-text-format` = "count > 2", # Or "static" if you prefer only "English" or "Español"
        # Optional: `width` can be set here too, or via CSS
        width = "180px"
      )
    )
  ),
  
  uiOutput("quiz_ui"),
  div(class = "btn-container",
      uiOutput("button_ui")
  )
)


####Server####
server <- function(input, output, session){
  i18n_r <- reactive({
    selected <- input$selected_language
    if(length(selected) > 0 && selected %in% i18n$get_languages()) {
      i18n$set_translation_language(selected)
    }
    i18n
  })
  
  history <- reactiveVal(c(1))
  sentencing_date <- reactiveVal(NULL)
  probation_start_date <- reactiveVal(NULL)
  sentencing_length_month <- reactiveVal(NULL)
  sentencing_length_year <- reactiveVal(NULL)
  selected_answer <- reactiveVal(NULL)
  felony_or_misdemeanor <- reactiveVal(NULL)
  multiple_misdemeanors <- reactiveVal("N/A")
  education_credits_answer <- reactiveVal(NULL)
  education_credits_more_than_2_answer <- reactiveVal("N/A")
  probation_after_parole_answer <- reactiveVal("N/A")
  final_year_of_parole_answer <- reactiveVal("N/A")
  eligibility_date_section_7 <- reactiveVal(NULL)
  eligibility_date_prc <- reactiveVal(NULL)
  
  violation_answer <- reactiveVal(NULL)
  threat_to_safety_answer <- reactiveVal(NULL)
  same_conduct_answer <- reactiveVal("N/A")
  technical_violation_found_answer <- reactiveVal(NULL)
  answer_selected <- reactiveVal(FALSE)
  answer_years_selected <- reactiveVal(FALSE)
  answer_months_selected <- reactiveVal(FALSE)
  
  observeEvent(input$js_href, {
    href <- input$js_href
    
    shinyalert(
      title = i18n_r()$t("Leaving Application"), 
      text = i18n_r()$t("This will take you to a new website."), 
      type = "info",
      showCancelButton = TRUE,
      confirmButtonText = i18n_r()$t("Continue"), 
      callbackR = function(x){
        if(isTRUE(x)){
          session$sendCustomMessage("open_url", href)
          session$sendCustomMessage("remove_href", href)
        }else{
          session$sendCustomMessage("restore_href", href)
        }
      }
    )
  }, ignoreInit = TRUE)
  
  ##Reactive function to calculate eligibility_date 
  calculate_eligibility_date_section_7 <- reactive({
    if(!is.null(sentencing_date()) & !is.null(felony_or_misdemeanor())){
      if(!is.null(probation_start_date())){
        base_date <- as.Date(probation_start_date())
      }else{
        base_date <- as.Date(sentencing_date())
      }
      felony_val <- felony_or_misdemeanor()
      if(felony_val == "Felony"){ 
        potential_date <- base_date + years(4)
      }else if(felony_val == "Misdemeanor"){
        potential_date <- base_date + years(2)
      }
      eligibility_date_val <- max(potential_date, as.Date("2025-06-11"))
      return(eligibility_date_val)
    }else{
      return(NULL)
    }
  })
  
  calculate_eligibility_date_prc <- reactive({
    req(sentencing_date(), felony_or_misdemeanor(), sentencing_length_year(),
        sentencing_length_month(), education_credits_answer())
    
    sentence_date <- as.Date(sentencing_date())
    if(!is.null(probation_start_date())){
      base_date <- as.Date(probation_start_date())
    }else{
      base_date <- as.Date(sentencing_date())
    }
    felony_val <- felony_or_misdemeanor()
    sentencing_length_year_val <- as.numeric(sentencing_length_year())
    sentencing_length_month_val <- as.numeric(sentencing_length_month())
    education_credits_val <- education_credits_answer()
    
    misdemeanor_potential_date <- base_date + years(2)
    felony_potential_date <- base_date + years(4)
    one_year_after_sentence_date <- sentence_date + years(1)
    sentencing_length_val <- as.numeric(sentencing_length_month_val +(12 * sentencing_length_year_val))
    
    multiple_misdemeanors_val <- multiple_misdemeanors()
    same_conduct_val <- same_conduct_answer()
    education_credits_more_than_2_val <- education_credits_more_than_2_answer()
    probation_after_parole_val <- probation_after_parole_answer()
    final_year_of_parole_val <- final_year_of_parole_answer()
    
    if(sentencing_length_val %% 2 == 1){
      half_sentence <- base_date + months((sentencing_length_val - 1) / 2) + days(15)
    }else{
      half_sentence <- base_date + months(sentencing_length_val / 2)
    }
  
    if(felony_val == i18n_r()$t("Misdemeanor") &(multiple_misdemeanors_val == i18n_r()$t("No") |(same_conduct_val == i18n_r()$t("Same Conduct") & multiple_misdemeanors_val == i18n_r()$t("Yes")))){
      eligibility_date_val <- as.Date(min(c(misdemeanor_potential_date, half_sentence)))
      if(education_credits_val == i18n_r()$t("Yes")){
        eligibility_date_val = eligibility_date_val - months(6)
      }
    }else if(felony_val == i18n_r()$t("Felony")){
      eligibility_date_val <- as.Date(min(c(felony_potential_date, half_sentence)))
      if(education_credits_val == i18n_r()$t("Yes")){
        eligibility_date_val = eligibility_date_val - months(6)
        if(education_credits_more_than_2_val == i18n_r()$t("Yes")){
          eligibility_date_val = eligibility_date_val - months(6)
        }
      }
      if(probation_after_parole_val == i18n_r()$t("Yes") & final_year_of_parole_val == i18n_r()$t("Yes")){
        eligibility_date_val = eligibility_date_val - months(12)
      }
    }else if(felony_val == i18n_r()$t("Misdemeanor") & multiple_misdemeanors_val == i18n_r()$t("Yes") & same_conduct_val == i18n_r()$t("Different Conduct")){
      eligibility_date_val <- as.Date(min(c(felony_potential_date, half_sentence)))
      if(education_credits_val == i18n_r()$t("Yes")){
        eligibility_date_val = eligibility_date_val - months(6)
      }
    }
    
    eligibility_date_final = as.Date(max(c(eligibility_date_val, one_year_after_sentence_date)))
    return(eligibility_date_final)
  })
  
  ##Eligibility Date Rendering Function
  render_section_7_q3 <- reactive({
    eligibility_date_val <- eligibility_date_section_7()
    question_content <- tags$div(
      tags$p(HTML(paste0(
        i18n_r()$t("Now we need to ask you about your behavior in the following dates: from "),
        "<strong>", format_pretty_date(eligibility_date_val - months(6)), "</strong>",
        i18n_r()$t(" to "), "<strong>", format_pretty_date(eligibility_date_val), "</strong>",
        i18n_r()$t(". Act 44 requires a judge to look at any alleged bad acts you have committed during this time. This is important because if the judge finds you have committed one of these acts, they are not required to consider you for early termination. A judge will look at your record and determine whether you have committed any technical violations during this time period.")
      ))),
      tags$p(i18n_r()$t("They will decide whether you have committed a violation in the following categories:")),
      tags$ol(type = "I",
              tags$li(i18n_r()$t("A violation that was sexual in nature")),
              tags$li(i18n_r()$t("A violation that involved assaultive behavior or included a credible threat to cause bodily injury to another, including incidents involving domestic violence")),
              tags$li(i18n_r()$t("A violation that involved possession or control of a firearm or dangerous weapon")),
              tags$li(i18n_r()$t("A violation involved the manufacture, sale, delivery or possession with the intent to manufacture, sell or deliver, a controlled substance or other drug regulated under the act of April 14, 1972(P.L.233, No.64), known as The Controlled Substance, Drug, Device and Cosmetic Act")),
              tags$li(i18n_r()$t("A violation in which the probationer absconded from probation")),
              tags$li(i18n_r()$t("A violation that involved an intentional and unexcused failure to adhere to recommended programming or conditions on three or more separate occasions. Multiple technical violations stemming from the same episode of events do not constitute separate technical violations.")),
              tags$li(i18n_r()$t("A violation that involved an identifiable threat to public safety"))
      ),
      tags$p(i18n_r()$t("If the judge finds that you have committed one of these violations the next steps will be as follows:"))
    )
    if(!is.null(eligibility_date_val)){
      tagList(
        div(question_content, class = "question-text"),
        fluidRow(
          column(width = 12,
                 div(
                   id = "violation-buttons",
                   actionButton("violation_yes", i18n_r()$t(decision_tree[[which(map_chr(decision_tree, "question_id") == "section_7_q3")]]$choices[1]), class = "btn btn-danger"),
                   actionButton("violation_no", i18n_r()$t(decision_tree[[which(map_chr(decision_tree, "question_id") == "section_7_q3")]]$choices[2]), class = "btn btn-success")
                 ),
                 tags$script(HTML(
                   "
                   $(document).on('click', '#violation-buttons .btn', function(){
                     $('#violation-buttons .btn').removeClass('active');
                     $(this).addClass('active');
                   });
                   "
                 ))
          )
        )
      )
    }else{
      tags$p(i18n_r()$t("Calculating eligibility date...")) 
    }
  })
  
  render_section_7_act_44_relief_result <- reactive({
    eligibility_date_val <- eligibility_date_section_7()
    eligibility_date_val_clean <- format_pretty_date(eligibility_date_val)
    
    question_content <-
      tags$div(
        tags$p(HTML(paste0(i18n_r()$t("On "), "<strong>", eligibility_date_val_clean, "</strong>", i18n_r()$t(", a court will be required to review your case to determine whether to have your probation terminated or your conditions modified. That judge will have almost unlimited discretion in making this decision, which is why it is important for you to give the court any information that could help them make this decision. In fact, the law requires the court to give both you and the prosecutor an opportunity to provide input on this issue prior to its determination. Do not pass up this opportunity!")))),
        tags$p(i18n_r()$t("The law also requires the judge to consider whether you have had any educational achievements or graduated from any programs. It is critically important you tell the judge if you have done so!")),
        tags$p(i18n_r()$t("In the meantime, you are entitled to apply for early termination or to have your conditions modified under 42 P.A.C.S. § 9771 at any time. A judge has discretion to grant or deny this application at any time, for any person, even if that person is not eligible for a mandatory review under Act 44."))
      )
    
    if(!is.null(eligibility_date_val)){
      tagList(
        div(question_content, class = "question-text"),
        fluidRow(
          column(
            width = 12,
            div(
              class = "btn-container",
            )
          )
        )
      )
    }else{
      tags$p(i18n_r()$t("Calculating eligibility date..."))
    }
  })
  
  render_prc_flow_q7 <- reactive({
    eligibility_date_val <- eligibility_date_prc()
    
    question_content <-
      tags$div(
        tags$p(HTML(paste0(i18n_r()$t("On or before "), "<strong>", format_pretty_date(as.Date(eligibility_date_val) - days(30)), "</strong>",
                           i18n_r()$t(", your probation officer must serve you with a document known as a Probation Status Report. They must also serve it on your prosecutor, court, your lawyer, and any registered victim. This document is "),
                           "<b>", i18n_r()$t("critically important"), "</b>", i18n_r()$t(" for you to review carefully.")))),
        tags$p(i18n_r()$t("This document must contain the following information")),
        tags$ul(
          tags$li(i18n_r()$t("The date your PO believes you are eligible for a conference on early termination or changing probation conditions")),
          tags$li(i18n_r()$t("Any technical violations your PO thinks you have committed within the past 6 months")),
          tags$li(i18n_r()$t("Any criminal convictions you have had while on probation or in custody on uderlying case")),
          tags$li(i18n_r()$t("Any programs you have completed while on probation")),
          tags$li(i18n_r()$t("Your restitution payments, if you have any")),
          tags$li(i18n_r()$t("A general description of your progress on probation"))
        ),
        tags$p(i18n_r()$t("Most importantly, your probation office must make a recommendation that your probation be terminated, your conditions modified, or that you continue under your current conditions. This is critically important for reasons explained on the next screen.")),
        tags$p(HTML(paste0(i18n_r()$t("If you did not receive this report by "),
                           "<strong>", format_pretty_date(as.Date(eligibility_date_val) - days(30)), "</strong>",
                           i18n_r()$t(", you should contact your attorney as soon as possible. It is very important to review this document, for reasons we will explain next."))))
      )
    
    if(!is.null(eligibility_date_val)){
      tagList(
        div(question_content, class = "question-text"),
        fluidRow(
          column(
            width = 12,
            div(
              class = "btn-container",
            )
          )
        )
      )
    }else{
      tags$p(i18n_r()$t("Calculating eligibility date..."))
    }
  })
  
  render_prc_flow_q9 <- reactive({
    eligibility_date_val <- eligibility_date_prc()
    
    question_content <-
      tags$div(
        tags$p(HTML(paste0(i18n_r()$t("Your Probation Review Conference must be held by "),
                           "<strong>", format_pretty_date(as.Date(eligibility_date_val) + days(60)), "</strong>",
                           i18n_r()$t(". If you do not receive your hearing by this date, you have the right to petition the court to hold this hearing within 5 days.")))),
        tags$p(i18n_r()$t("During the Conference, the judge will review your behavior and record while on probation. In some cases, good behavior may lead to mandatory termination of probation. In other cases, certain types of poor behavior such as a new conviction while on probation makes early termination impossible. This tool is designed to help you understand exactly what will happen at your Probation Review Conference, and exactly what a judge is required to consider and decide. This tool will pose a series of questions that a judge will need to answer during a Probation Review Conference, and then this tool will tell you the potential outcome of your case based on the information provided. Of course, this tool cannot predict exactly what a judge will decide, but it will tell you what the outcome must be if certain factors are met or not met.")),
        tags$p(i18n_r()$t("First, a judge will look at your criminal record while you were on probation or while you were in custody for that case. Were you convicted of a felony or a first or second degree misdemeanor during this time?"))
      )
    
    if(!is.null(eligibility_date_val)){
      tagList(
        div(question_content, class = "question-text"),
        fluidRow(
          column(
            width = 12,
            div(
              class = "btn-container",
            )
          )
        ),
        radioButtons("answer", NULL, selected = character(0), choices = c(i18n_r()$t("Yes"), i18n_r()$t("No"))) 
      )
    }else{
      tags$p(i18n_r()$t("Calculating eligibility date..."))
    }
  })
  
  render_prc_flow_q10 <- reactive({
    question_content <- decision_tree[[which(map_chr(decision_tree, "question_id") == "prc_flow_q10")]]$question
    
    tagList(
      div(question_content, class = "question-text"),
      fluidRow(
        column(width = 12,
               div(
                 id = "threat_to_safety_answer-buttons",
                 actionButton("threat_to_safety_answer_yes", i18n_r()$t(decision_tree[[which(map_chr(decision_tree, "question_id") == "prc_flow_q10")]]$choices[1]), class = "btn btn-danger"),
                 actionButton("threat_to_safety_answer_no", i18n_r()$t(decision_tree[[which(map_chr(decision_tree, "question_id") == "prc_flow_q10")]]$choices[2]), class = "btn btn-success")
               ),
               tags$script(HTML(
                 "
                 $(document).on('click', '#threat_to_safety_answer-buttons .btn', function(){
                   $('#threat_to_safety_answer-buttons .btn').removeClass('active');
                   $(this).addClass('active');
                 });
                 "
               ))
        )
      )
    )
  })
  
  render_prc_flow_q3_3 <- reactive({
    question_content <- decision_tree[[which(map_chr(decision_tree, "question_id") == "prc_flow_q3_3")]]$question
    
    tagList(
      div(question_content, class = "question-text"),
      fluidRow(
        column(width = 12,
               div(
                 id = "same_conduct_answer-buttons",
                 actionButton("same_conduct_answer_yes", i18n_r()$t(decision_tree[[which(map_chr(decision_tree, "question_id") == "prc_flow_q3_3")]]$choices[1]), class = "btn btn-danger"),
                 actionButton("same_conduct_answer_no", i18n_r()$t(decision_tree[[which(map_chr(decision_tree, "question_id") == "prc_flow_q3_3")]]$choices[2]), class = "btn btn-success")
               ),
               tags$script(HTML(
                 "
                 $(document).on('click', '#same_conduct_answer-buttons .btn', function(){
                   $('#same_conduct_answer-buttons .btn').removeClass('active');
                   $(this).addClass('active');
                 });
                 "
               ))
        )
      )
    )
  })
  
  render_prc_flow_q11 <- reactive({
    question_content <- decision_tree[[which(map_chr(decision_tree, "question_id") == "prc_flow_q11")]]$question
    
    tagList(
      div(question_content, class = "question-text"),
      fluidRow(
        column(width = 12,
               div(
                 id = "technical_violation_found-buttons",
                 actionButton("technical_violation_found_yes", i18n_r()$t(decision_tree[[which(map_chr(decision_tree, "question_id") == "prc_flow_q11")]]$choices[1]), class = "btn btn-danger"),
                 actionButton("technical_violation_found_no", i18n_r()$t(decision_tree[[which(map_chr(decision_tree, "question_id") == "prc_flow_q11")]]$choices[2]), class = "btn btn-success")
               ),
               tags$script(HTML(
                 "
                 $(document).on('click', '#technical_violation_found-buttons .btn', function(){
                   $('#technical_violation_found-buttons .btn').removeClass('active');
                   $(this).addClass('active');
                 });
                 "
               ))
        )
      )
    )
  })
  
  translate_ui_recursive <- function(element, translator_reactive){
    if(is.character(element)){
      return(translator_reactive()$t(element)) 
   }else if(inherits(element, "shiny.tag") || inherits(element, "htmltools.tag")){
      if(!is.null(element$children)){
        element$children <- lapply(element$children, translate_ui_recursive, translator_reactive)
      }
      if(!is.null(element$attribs)){
        if(!is.null(element$attribs$placeholder)){
          element$attribs$placeholder <- translator_reactive()$t(element$attribs$placeholder)
        }
      }
      return(element)
   }else if(is.list(element)){
      return(lapply(element, translate_ui_recursive, translator_reactive))
    }
    return(element) 
  }
  
  output$quiz_ui <- renderUI({
    current_index <- tail(history(), 1)
    current_question <- decision_tree[[current_index]]
    
    if(length(current_question$intro) > 0){
      tagList(
        translate_ui_recursive(current_question$content_above_buttons, i18n_r),
        uiOutput("intro_buttons"), 
        translate_ui_recursive(current_question$content_below_buttons, i18n_r)
      )
    }else if("question" %in% names(current_question) & length(current_question$question_list) == 0){
      tagList(
        
        translate_ui_recursive(current_question$question, i18n_r),
        if("choices" %in% names(current_question) & length(current_question$choices) > 0 & length(current_question$date_question) == 0){
          radioButtons("answer", NULL, selected = character(0),
                       choices = lapply(current_question$choices, function(x) i18n_r()$t(x)))
        }else if("choices" %in% names(current_question) & length(current_question$choices) > 0 & length(current_question$date_question) > 0){
          dateInput(
            inputId = "answer",
            label = i18n_r()$t("Select a date:"), 
            value = NULL,
            min = "1980-01-01",
            max = "2030-12-31",
            format = "yyyy-mm-dd",
            startview = "month",
            weekstart = 0,
            language = "en"
          )
        }else if(length(current_question$drop_down) > 0){
          dropdownButton(
            label = i18n_r()$t("Select Duration"), 
            status = "primary",
            icon = icon("sliders-h"),
            circle = FALSE,
            fluidRow(
              column(6,
                     selectInput("answer_years", i18n_r()$t("Years:"), choices = 0:100, selected = character(0))
              ),
              column(6,
                     selectInput("answer_months", i18n_r()$t("Months:"), choices = 0:11, selected = character(0))
              )
            )
          )
        }
      )
    }else if("question" %in% names(current_question) & length(current_question$question_list) > 0 & length(current_question$date_question) == 0){
      tagList(
        translate_ui_recursive(current_question$question, i18n_r),
        current_question %>%
          pluck("question_list") %>%
          enframe() %>%
          unnest(value) %>%
          pmap(
            ~tagList(
              div(tags$div(tags$p(HTML(i18n_r()$t(.y))))), 
              radioButtons(
                inputId = paste0("answer_", .x),
                selected = character(0),
                label = NULL,
                choices = lapply(c("Yes", "No"), function(x) i18n_r()$t(x)) 
              )
            )
          )
      )
    }else if("result" %in% names(current_question)){
      translate_ui_recursive(current_question$result, i18n_r)
    }
  })
  
  # output$quiz_ui <- renderUI({
  #   current_index <- tail(history(), 1)
  #   current_question <- decision_tree[[current_index]]
  #   
  #   if(length(current_question$intro) > 0){
  #     tagList(
  #       div(current_question$content_above_buttons, class = "question-text"),
  #       uiOutput("intro_buttons"),
  #       div(current_question$content_below_buttons, class = "question-text")
  #     )
  #   }else if(current_question$question_id == "section_7_q3"){
  #     render_section_7_q3()
  #   }else if(current_question$question_id == "prc_flow_q7"){
  #     render_prc_flow_q7()
  #   }else if(current_question$question_id == "prc_flow_q9"){
  #     render_prc_flow_q9()
  #   }else if(current_question$question_id == "prc_flow_q10"){
  #     render_prc_flow_q10()
  #   }else if(current_question$question_id == "prc_flow_q3_3"){
  #     render_prc_flow_q3_3()
  #   }else if(current_question$question_id == "prc_flow_q11"){
  #     render_prc_flow_q11()
  #   }else if("question" %in% names(current_question) & length(current_question$question_list) == 0){
  #     tagList(
  #       div(current_question$question, class = "question-text"),
  #       if("choices" %in% names(current_question) & length(current_question$choices) > 0 & length(current_question$date_question) == 0){
  #         radioButtons("answer", NULL, selected = character(0), choices = current_question$choices)
  #       }else if("choices" %in% names(current_question) & length(current_question$choices) > 0 & length(current_question$date_question) > 0){
  #         dateInput(
  #           inputId = "answer",
  #           label = i18n_r()$t("Select a date:"), 
  #           value = NULL,
  #           min = "1980-01-01",
  #           max = "2030-12-31",
  #           format = "yyyy-mm-dd",
  #           startview = "month",
  #           weekstart = 0,
  #           language = "en" 
  #         )
  #       }else if(length(current_question$drop_down) > 0){
  #         dropdownButton(
  #           label = i18n_r()$t("Select Duration"), 
  #           status = "primary",
  #           icon = icon("sliders-h"),
  #           circle = FALSE,
  #           
  #           fluidRow(
  #             column(6,
  #                    selectInput("answer_years", i18n_r()$t("Years:"), choices = 0:100, selected = character(0))
  #             ),
  #             column(6,
  #                    selectInput("answer_months", i18n_r()$t("Months:"), choices = 0:11, selected = character(0)) 
  #             )
  #           )
  #         )
  #       }
  #     )
  #   }else if("question" %in% names(current_question) & length(current_question$question_list) > 0 & length(current_question$date_question) == 0){
  #     tagList(
  #       div(current_question$question, class = "question-text"),
  #       current_question %>%
  #         pluck("question_list") %>%
  #         enframe() %>%
  #         unnest(value) %>%
  #         pmap(
  #           ~tagList(
  #             div(tags$div(tags$p(HTML(.y)))),
  #             radioButtons(
  #               inputId = paste0("answer_", .x),
  #               selected = character(0),
  #               label = NULL,
  #               choices = c(i18n_r()$t("Yes"), i18n_r()$t("No")) 
  #             )
  #           )
  #         )
  #     )
  #   }else if("result" %in% names(current_question)){
  #     if(current_question$question_id == "section_7_act_44_relief_result"){
  #       render_section_7_act_44_relief_result()
  #     }else{
  #       div(current_question$result, class = "question-text")
  #     }
  #   }
  # })
  
  output$intro_buttons <- renderUI({
    div(
      actionButton("start_button", i18n_r()$t("Start"), class = "btn btn-success") 
    )
  })
  
  output$button_ui <- renderUI({
    current_index <- tail(history(), 1)
    current_question <- decision_tree[[current_index]]
    buttons <- list()
    
    if(length(history()) > 1 & current_question$question_id != "intro_page"){
      buttons <- append(buttons, list(actionButton("back_button", i18n_r()$t("Back"), class = "btn btn-warning"))) 
    }
    
    if(current_question$question_id == "finished_page"){
      buttons <- append(buttons, list(actionButton("start_over_button", i18n_r()$t("Start Over"), class = "btn btn-success"))) 
    }else if("result" %in% names(current_question) & current_question$question_id != "finished_page"){
      buttons <- append(buttons, list(actionButton("finish_button", i18n_r()$t("Finish"), class = "btn btn-danger"))) 
    }else if(length(current_question$intro) == 0 & current_question$question_id != "finished_page"){
      buttons <- append(buttons, list(actionButton("next_button", i18n_r()$t("Next"), class = "btn btn-primary"))) 
    }
    
    do.call(fluidRow, buttons)
  })
  
  observeEvent(input$answer, {
    current_index <- tail(history(), 1)
    current_question <- decision_tree[[current_index]]
    
    if(length(current_question$question_list) == 0 & length(current_question$date_question) > 0){
      if(current_question$question_id == "defendant_sentencing_date"){
        sentencing_date(as.Date(input$answer))
        ifelse(as.Date(input$answer) >= as.Date("2024-06-11"), selected_answer(i18n_r()$t("Yes")), selected_answer(i18n_r()$t("No")))
        answer_selected(TRUE)
      }else if(current_question$question_id %in% c("prc_flow_probation_start_date", "section_7_probation_start_date")){
        probation_start_date(as.Date(input$answer))
        answer_selected(TRUE)
        selected_answer(i18n_r()$t("Yes")) 
      }
    }else if(length(current_question$question_list) == 0 & length(current_question$date_question) == 0 & length(current_question$drop_down) > 0){
      answer_ids <-
        input %>%
        names() %>%
        str_subset("answer_years|answer_months")
      responses <- sapply(answer_ids, function(id) input[[id]])
      if(any(is.na(responses)) == TRUE){
        answer_selected(FALSE)
      }else{
        answer_selected(TRUE)
      }
    }else if(length(current_question$question_list) == 0 & length(current_question$date_question) == 0 & length(current_question$drop_down) == 0 & "choices" %in% names(current_question)){
      if(current_question$question_id %in% c("section_7_q2", "prc_flow_q2")){
        ifelse(input$answer == i18n_r()$t("Yes"), felony_or_misdemeanor(i18n_r()$t("Felony")), felony_or_misdemeanor(i18n_r()$t("Misdemeanor")))
      }else if(current_question$question_id == "prc_flow_q3_2"){
        ifelse(input$answer == i18n_r()$t("Yes"), multiple_misdemeanors(i18n_r()$t("Yes")), multiple_misdemeanors(i18n_r()$t("No")))
      }else if(current_question$question_id == "prc_flow_q5"){
        ifelse(input$answer == i18n_r()$t("Yes"), education_credits_answer(i18n_r()$t("Yes")), education_credits_answer(i18n_r()$t("No")))
      }else if(current_question$question_id == "prc_flow_q6"){
        ifelse(input$answer == i18n_r()$t("Yes"), education_credits_more_than_2_answer(i18n_r()$t("Yes")), education_credits_more_than_2_answer(i18n_r()$t("No")))
      }else if(current_question$question_id == "prc_flow_q3_1"){
        ifelse(input$answer == i18n_r()$t("Yes"), probation_after_parole_answer(i18n_r()$t("Yes")), probation_after_parole_answer(i18n_r()$t("No")))
      }else if(current_question$question_id == "prc_flow_q3_1_final_year_parole"){
        ifelse(input$answer == i18n_r()$t("Yes"), final_year_of_parole_answer(i18n_r()$t("Yes")), final_year_of_parole_answer(i18n_r()$t("No")))
      }else if(current_question$question_id %in% c("prc_flow_split_sentence", "section_7_split_sentence")){
        if(input$answer == i18n_r()$t("No")){ 
          probation_start_date(NULL)
        }
      }
      ifelse(is.null(input$answer) == FALSE, answer_selected(TRUE), answer_selected(FALSE))
      selected_answer(input$answer) 
    }else if(length(current_question$question_list) > 0 & length(current_question$date_question) == 0){
      answer_ids <- names(input)[grepl("^answer_q", names(input))]
      
      responses <- sapply(answer_ids, function(id) input[[id]])
      null_responses <-
        responses %>%
        map(~is.null(.x)) %>%
        unlist() %>%
        any()
      if(null_responses == FALSE){
        answer_selected(TRUE)
      }else if(null_responses == TRUE){
        answer_selected(FALSE)
      }
      
      answer_selected_val = answer_selected()
      
      if(answer_selected_val == TRUE){
        if(i18n_r()$t("Yes") %in% responses){ 
          selected_answer(i18n_r()$t("Yes"))
        }else{
          selected_answer(i18n_r()$t("No"))
        }
      }
    }
  })
  
  observe({
    eligibility_date_prc(calculate_eligibility_date_prc())
  })
  
  observe({
    eligibility_date_section_7(calculate_eligibility_date_section_7())
  })
  
  observe({
    answer_selected_val = answer_selected()
    if(!is.null(answer_selected_val) & answer_selected_val == TRUE) {
      shinyjs::enable("next_button")
    }else{
      shinyjs::disable("next_button")
    }
  })
  
  observeEvent(input$answer_years, {
    answer_val <- as.numeric(input$answer_years)
    if(!is.null(answer_val)){
      sentencing_length_year(answer_val)
      answer_years_selected(TRUE)
    }
    
  })
  
  observeEvent(input$answer_months, {
    answer_val <- as.numeric(input$answer_months)
    if(!is.null(answer_val)){
      sentencing_length_month(answer_val)
      answer_months_selected(TRUE)
    }
  })
  
  observe({
    current_index <- tail(history(), 1)
    current_question <- decision_tree[[current_index]]
    answer_years_selected_val = answer_years_selected()
    answer_months_selected_val = answer_months_selected()
    
    if(current_question$question_id == "prc_flow_q4"){
      if(answer_years_selected_val == TRUE & answer_months_selected_val == TRUE){
        answer_selected(TRUE)
      }else{
        answer_selected(FALSE)
      }
    }else if(length(current_question$question_list) > 0){
      answer_ids <- names(input)[grepl("^answer_q", names(input))]
      
      responses <- sapply(answer_ids, function(id) input[[id]])
      null_responses <-
        responses %>%
        map(~is.null(.x)) %>%
        unlist() %>%
        any()
      if(null_responses == FALSE){
        answer_selected(TRUE)
      }else if(null_responses == TRUE){
        answer_selected(FALSE)
      }
      
      answer_selected_val = answer_selected()
      
      if(answer_selected_val == TRUE){
        if(i18n_r()$t("Yes") %in% responses){ 
          selected_answer(i18n_r()$t("Yes"))
        }else{
          selected_answer(i18n_r()$t("No"))
        }
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
    answer_selected_val <- answer_selected()
    
    ##Accounts for if no answer selected or if the current question is just an info slide that doesn't need to be answered
    if(answer_selected_val == FALSE &(length(current_question$next_question) == 1 & !("result" %in% names(current_question)) &  !("choices" %in% names(current_question))) == FALSE){
      shinyalert(
        title = i18n_r()$t("Missing answer"), 
        text = i18n_r()$t("Please select an answer before clicking Next"), 
        type = "warning"
      )
      return()
    }
    
    if("choices" %in% names(current_question)){
      if(length(current_question$question_list) == 0){
        if(current_question$question_id == "section_7_q3"){
          next_id <- switch(violation_answer(),
                            "Judge Finds Technical Violations" = current_question$next_question[["Judge Finds Technical Violations"]],
                            "No Technical Violations" = current_question$next_question[["No Technical Violations"]]
                            # ,
                            # i18n_r()$t("Judge Finds Technical Violations") = current_question$next_question[["Judge Finds Technical Violations"]],
                            # i18n_r()$t("No Technical Violations") = current_question$next_question[["No Technical Violations"]]
          )
        }else if(current_question$question_id == "prc_flow_q10"){
          next_id <- switch(threat_to_safety_answer(),
                            "Found Threat to Public Safety" = current_question$next_question[["Found Threat to Public Safety"]],
                            "Did Not Find Threat to Public Safety" = current_question$next_question[["Did Not Find Threat to Public Safety"]]
                            # ,
                            # i18n_r()$t("Found Threat to Public Safety") = current_question$next_question[["Found Threat to Public Safety"]],
                            # i18n_r()$t("Did Not Find Threat to Public Safety") = current_question$next_question[["Did Not Find Threat to Public Safety"]]
          )
        }else if(current_question$question_id == "prc_flow_q3_3"){
          next_id <- switch(same_conduct_answer(),
                            "Same Conduct" = current_question$next_question[["Same Conduct"]],
                            "Different Conduct" = current_question$next_question[["Different Conduct"]]
                            # ,
                            # i18n_r()$t("Same Conduct") = current_question$next_question[["Same Conduct"]],
                            # i18n_r()$t("Different Conduct") = current_question$next_question[["Different Conduct"]]
          )
        }else if(current_question$question_id == "prc_flow_q11"){
          next_id <- switch(technical_violation_found_answer(),
                            "Technical Violation Found" = current_question$next_question[["Technical Violation Found"]],
                            "No Technical Violation Found" = current_question$next_question[["No Technical Violation Found"]]
                            # ,
                            # i18n_r()$t("Technical Violation Found") = current_question$next_question[["Technical Violation Found"]],
                            # i18n_r()$t("No Technical Violation Found") = current_question$next_question[["No Technical Violation Found"]]
          )
        }else if(current_question$question_id == "prc_flow_q5"){
          education_credits_val <- education_credits_answer()
          felony_or_misdemeanor_val <- felony_or_misdemeanor()
          if((felony_or_misdemeanor_val == i18n_r()$t("Felony") & education_credits_val == i18n_r()$t("Yes"))){
            next_id <- current_question$next_question[[i18n_r()$t("Yes")]]
          }else{
            next_id <- current_question$next_question[[i18n_r()$t("No")]]
          }
        }else{
          next_id <- switch(selected_answer(),
                            "Yes" = current_question$next_question[["Yes"]],
                            "No" = current_question$next_question[["No"]]
                            # ,
                            # i18n_r()$t("Yes") = current_question$next_question[["Yes"]], 
                            # i18n_r()$t("No") = current_question$next_question[["No"]]  
          )
        }
      }else{ 
        answer_ids <- names(input)[grepl("^answer_q", names(input))]
        responses <- sapply(answer_ids, function(id) input[[id]])
        if(any(responses == i18n_r()$t("Yes"))){ 
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
    answer_selected(FALSE)
    selected_answer(NULL)
  })
  
  observeEvent(input$violation_yes, {
    violation_answer("Judge Finds Technical Violations") 
    answer_selected(TRUE)
  })
  
  observeEvent(input$violation_no, {
    violation_answer("No Technical Violations") 
    answer_selected(TRUE)
  })
  
  observeEvent(input$threat_to_safety_answer_yes, {
    threat_to_safety_answer("Found Threat to Public Safety") 
    answer_selected(TRUE)
  })
  
  observeEvent(input$threat_to_safety_answer_no, {
    threat_to_safety_answer("Did Not Find Threat to Public Safety") 
    answer_selected(TRUE)
  })
  
  observeEvent(input$same_conduct_answer_yes, {
    same_conduct_answer("Same Conduct") 
    answer_selected(TRUE)
  })
  
  observeEvent(input$same_conduct_answer_no, {
    same_conduct_answer("Different Conduct") 
    answer_selected(TRUE)
  })
  
  observeEvent(input$technical_violation_found_yes, {
    technical_violation_found_answer("Technical Violation Found") 
    answer_selected(TRUE)
  })
  
  observeEvent(input$technical_violation_found_no, {
    technical_violation_found_answer("No Technical Violation Found") 
    answer_selected(TRUE)
  })
  
  observeEvent(input$back_button, {
    answer_selected(FALSE)
    selected_answer(NULL)
    if(length(history()) > 1){
      history(history()[1:(length(history()) - 1)])
      answer_selected(FALSE)
      selected_answer(NULL)
    }
  })
  
  observeEvent(input$start_over_button, {
    history(c(1))
    sentencing_date(NULL)
    probation_start_date(NULL)
    sentencing_length_month(NULL)
    sentencing_length_year(NULL)
    selected_answer(NULL)
    felony_or_misdemeanor(NULL)
    multiple_misdemeanors("N/A")
    education_credits_answer(NULL)
    education_credits_more_than_2_answer("N/A")
    probation_after_parole_answer("N/A")
    final_year_of_parole_answer("N/A")
    eligibility_date_section_7(NULL)
    eligibility_date_prc(NULL)
    violation_answer(NULL)
    threat_to_safety_answer(NULL)
    same_conduct_answer("N/A")
    technical_violation_found_answer(NULL)
    answer_selected(FALSE)
    answer_years_selected(FALSE)
    answer_months_selected(FALSE)
  })
  
  observeEvent(input$finish_button, {
    finished_page_index <- which(map_chr(decision_tree, "question_id") == "finished_page")
    history(c(history(), finished_page_index))
  })
  
  ##Observe language selection to update the translator
  observeEvent(input$selected_language, {
    i18n$set_translation_language(input$selected_language)
  })
  
}

##Deploy App
shinyApp(ui = ui, server = server)