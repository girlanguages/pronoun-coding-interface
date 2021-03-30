#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)

data_to_code <- data.frame()
max_sent <- 200

first_person_agreement_options <- c("not yet coded",
                                    "verb is omitted",
                                    "third singular (nós/a gente vai, pra nós ir)",
                                    "first plural (nós vamos, pra nós irmos)",
                                    "impersonal (a gente chegando)")

second_person_agreement_options <- c("not yet coded",
                                     "verb is omitted",
                                     "third singular (você/tu vai, pra você ir)",
                                     "second singular (tu vais, pra tu ires)",
                                     "impersonal (você chegando)")

tma_options <- c("not yet coded",
                 "infinitivo (pra gente caminhar)",
                 "gerúndio (a gente caminhando)",
                 "presente (a gente caminha)",
                 "presente contínuo (a gente está caminhando)",
                 "pretérito imperfeito (a gente caminhava)",
                 "pretérito contínuo (a gente estava caminhando)",
                 "pretérito perfeito (a gente caminhou)",
                 "pretérito perfeito composto (a gente tem caminhado)",
                 "pretérito mais-que-perfeito (a gente tinha caminhado)",
                 "futuro (a gente caminhará)",
                 "futuro perifrástico (a gente vai caminhar)",
                 "futuro contínuo (a gente estará caminhando)",
                 "futuro perifrástico contínuo (a gente vai estar caminhando)",
                 "futuro do pretérito/condicional (a gente caminharia)",
                 "futuro perifrástico do pretérito (a gente iria caminhar)",
                 "futuro do pretérito composto (a gente teria caminhado)",
                 "subjuntivo do presente (que a gente caminhe)",
                 "subjuntivo do pretérito (que a gente caminhasse)",
                 "subjuntivo do futuro (quando a gente caminhar)",
                 "subjuntivo do futuro perifrástico (quando a gente for caminhar)",
                 "subjuntivo do futuro contínuo (quando a gente estiver caminhando)",
                 "subjuntivo do presente contínuo (que a gente esteja caminhando)",
                 "subjuntivo do presente perfeito (que a gente tenha caminhado)",
                 "subjuntivo do pretérito perfeito (que a gente tivesse caminhado)",
                 "subjuntivo do pretérito mais-que-perfeito (que a gente tivesse tido caminhado)")

display_tweet_to_code <- function(my_data, my_index) {
    selected_tweet <- my_data %>%
        slice(as.numeric(my_index)) %>%
        select(text)

    tweet_to_display <- my_data %>%
        mutate(tweet_number = rownames(.)) %>%
        filter(text == selected_tweet$text) %>%
        mutate(pronouns = ifelse(tweet_number == my_index,
                                 paste('<font color="blue">',
                                       pronouns, '</font>'),
                                 pronouns)) %>%
        mutate(tweet_to_display = paste("<p><strong>Pronoun for tweet #",
                                        tweet_number,
                                        ":</strong> ", pronouns,
                                        "</p>")) %>%
        pull(tweet_to_display)
    
    return(paste("<h3>Tweet Text</h3><p>", selected_tweet$text, "</p>",
                 "<h3>Coding</h3>",
                 paste(tweet_to_display, collapse = " ")))
}


update_code_display <- function(session, input, selection) {
    keep_value <- data_to_code$keep[selection]
    
    updateCheckboxInput(session, "include_tweet", 
                        label = paste0("Include tweet #", selection,
                                       " in the analysis"),
                        value = keep_value)
    
    updateTextAreaInput(session, "comments",
                        value = data_to_code$comments[selection])
    
    updateTextInput(session, "verb",
                    value = data_to_code$verb[selection])
    
    updateTextInput(session, "clause",
                    value = data_to_code$clause[selection])
    
    updateTextInput(session, "internetes",
                    value = data_to_code$internetes[selection])
    
    updateTextInput(session, "phonemic",
                    value = data_to_code$phonemic[selection])
    
    
    req(input$user_file)
    
    if (data_to_code$pronouns[selection] %in% c("tu", "você", "voce", "cê",
                                          "ce", "c", "vc", "ocê", "oce",
                                          "o senhor", "a senhora", 
                                          "o snr", "a snra", "o sr",
                                          "a sra", "a senhorita")) {
        
        updateSelectInput(session, "verb_agreement",
                          choices = second_person_agreement_options)
        
    } else {
        updateSelectInput(session, "verb_agreement",
                          choices = first_person_agreement_options)
    }
    
    
    if (keep_value) {
        shinyjs::show("verb")
        shinyjs::show("clause")
        shinyjs::show("internetes")
        shinyjs::show("phonemic")
        shinyjs::show("verb_agreement")
        shinyjs::show("tma")
        shinyjs::show("intervening_material")
        shinyjs::show("negation")
        shinyjs::show("order")
        shinyjs::show("reference")
    } else {
        shinyjs::hide("verb")
        shinyjs::hide("clause")
        shinyjs::hide("internetes")
        shinyjs::hide("phonemic")
        shinyjs::hide("verb_agreement")
        shinyjs::hide("tma")
        shinyjs::hide("intervening_material")
        shinyjs::hide("negation")
        shinyjs::hide("order")
        shinyjs::hide("reference")
    }
    
    updateSelectInput(session, "verb_agreement",
                      selected = data_to_code$verb_agreement[selection])
    
    updateSelectInput(session, "tma",
                      selected = data_to_code$tma[selection])
    
    updateSelectInput(session, "intervening_material",
                      selected = data_to_code$intervening_material[selection])
    
    updateSelectInput(session, "negation",
                      selected = data_to_code$negation[selection])
    
    updateSelectInput(session, "order",
                      selected = data_to_code$order[selection])
    
    updateSelectInput(session, "reference",
                      selected = data_to_code$reference[selection])
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Code Subject Pronoun in Brazilian Portuguese Tweets"),

    # other elements 
    useShinyjs(),
    fluidRow(
        column(3,
            fileInput('user_file', 
                      'Select the .tsv file',
                      accept=c('text/tsv',
                               'text/tab-separated-values,text/plain',
                               '.tsv')),
            selectInput("tweet_number", "Select tweet to code:",
                        c(1:max_sent)),
            actionButton('previous_sent',"<< Previous"),
            actionButton('next_sent',"Next >>"),
            downloadButton("downloadData", "Download")
        ),

        # Show a plot of the generated distribution
        column(8,
               tabsetPanel(type = "tabs",
                           tabPanel("Tweets", 
                                    htmlOutput("display_tweet"),
                                    checkboxInput("include_tweet", 
                                                  "Include tweet in the analysis",
                                                  value = TRUE),
                                    textInput("verb", "Verb Phrase:",
                                              width = "70%"),
                                    textInput("clause", "Context (clause):",
                                              width = "70%"),
                                    textInput("internetes", "Internet Language:",
                                              width = "70%"),
                                    textInput("phonemic", "Phonemic Orthography:",
                                              width = "70%"),
                                    selectInput("verb_agreement", "Verb Agreement:",
                                                choices = c("NA"),
                                                width = "70%"),
                                    selectInput("tma", "Tense-Aspect-Mood:",
                                                choices = tma_options,
                                                width = "70%"),
                                    selectInput("intervening_material",
                                                "Intervening Material between subject and verb?",
                                                choices = c("no", "yes",
                                                            "verb is omitted"),
                                                width = "70%"),
                                    selectInput("negation",
                                                "Negation of verb?",
                                                choices = c("no", "yes",
                                                            "verb is omitted"),
                                                width = "70%"),
                                    selectInput("order", "Subject Verb Order:",
                                                choices = c("subject-verb",
                                                            "verb-subject",
                                                            "verb is omitted"),
                                                width = "70%"),
                                    selectInput("reference", "Type of subject reference:",
                                                choices = c("not yet coded",
                                                            "generic",
                                                            "specific",
                                                            "generic or specific",
                                                            "I don't know"),
                                                width = "70%"),
                                    textAreaInput("comments", "Comments:",
                                                  width = "80%")),
                           tabPanel("Data",
                                    tableOutput("display_data")),
                           tabPanel("Coding Schema",
                                    includeMarkdown("coding-schema.Rmd"))
                           )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        tsv_file = input$user_file
        
        if (is.null(tsv_file)) {
            return(NULL)
        }
        
        data_to_code <<- read_tsv(tsv_file$datapath)
        data_to_code <<- data_to_code %>%
            mutate(tweet_number = row.names(.))
        
        updateSelectInput(session, "tweet_number",
                          choices = c(1:max_sent),
                          selected = 1)
        
        updateCheckboxInput(session, "include_tweet", 
                            label = paste0("Include tweet #1 in the analysis"),
                            value = TRUE)
        
        isolate({
            if (!("keep" %in% colnames(data_to_code))) {
                data_to_code$keep <<- TRUE
                data_to_code$verb <<- ""
                data_to_code$clause <<- ""
                data_to_code$internetes <<- ""
                data_to_code$phonemic <<- ""
                data_to_code$verb_agreement <<- "not yet coded"
                data_to_code$tma <<- "not yet coded"
                data_to_code$intervening_material <<- "no"
                data_to_code$negation <<- "no"
                data_to_code$order <<- "subject-verb"
                data_to_code$reference <<- "not yet coded"
                data_to_code$comments <<- ""
            } else {
                update_code_display(session, input, 1)
                data_to_code$phonemic <<- as.character(data_to_code$phonemic)
                data_to_code$verb <<- as.character(data_to_code$verb)
                data_to_code$internetes <<- as.character(data_to_code$internetes)
                data_to_code$clause <<- as.character(data_to_code$clause)
                data_to_code$comments <<- as.character(data_to_code$comments)
            }
            
            max_sent <<- data_to_code %>%
                nrow()

        })
        
    })
    

    output$display_tweet <- renderUI({
        req(input$user_file)
        HTML(display_tweet_to_code(data_to_code, input$tweet_number))
    })
    
    output$display_data <- renderTable({
        if (nrow(data_to_code) > 0) {
            data_to_code[input$tweet_number, ]
        }
        
    })
    
    observe({
        input$comments
        
        isolate({
            comments <- input$comments
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "comments"] <<- comments
        })
        
    })
    
    observe({
        input$verb
        
        isolate({
            verb <- input$verb
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "verb"] <<- verb
        })
        
    })
    
    
    observe({
        input$clause
        
        isolate({
            clause <- input$clause
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "clause"] <<- clause
        })
        
    })
    
    observe({
        input$internetes
        
        isolate({
            internetes <- input$internetes
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "internetes"] <<- internetes
        })
        
    })
    
    observe({
        input$phonemic
        
        isolate({
            phonemic <- input$phonemic
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "phonemic"] <<- phonemic
        })
        
    })
    
    observe({
        input$verb_agreement
        
        isolate({
            verb_agreement <- input$verb_agreement
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "verb_agreement"] <<- verb_agreement
        })
        
        if (verb_agreement == "impersonal (você chegando)") {
            updateSelectInput(session, "tma",
                              selected = "gerúndio (a gente caminhando)")
            
            data_to_code[sel, "tma"] <<- "gerúndio (a gente caminhando)"
        }
        
    })
    
    observe({
        input$tma
        
        isolate({
            tma <- input$tma
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "tma"] <<- tma
        })
        
        if (tma == "infinitivo (pra gente caminhar)") {
            updateSelectInput(session, "verb_agreement",
                              selected = "third singular (você/tu vai, pra você ir)")
            
            data_to_code[sel, "verb_agreement"] <<- "third singular (você/tu vai, pra você ir)"
        }
        
    })
    
    observe({
        input$intervening_material
        
        isolate({
            intervening_material <- input$intervening_material
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "intervening_material"] <<- intervening_material
            
            
        })
        
    })
    
    observe({
        input$negation
        
        isolate({
            negation <- input$negation
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "negation"] <<- negation
            
            if (negation == "yes") {
                updateSelectInput(session, "intervening_material",
                                  selected = "yes")
                
                data_to_code[sel, "intervening_material"] <<- "yes"
            }
        })
        
    })
    
    observe({
        input$order
        
        isolate({
            order <- input$order
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "order"] <<- order
        })
        
    })
    
    observe({
        input$reference
        
        isolate({
            reference <- input$reference
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "reference"] <<- reference
        })
        
    })
    
    observe({
        input$include_tweet
        
        isolate({
            keep_tweet <- input$include_tweet
            sel <- as.numeric(input$tweet_number)
            data_to_code[sel, "keep"] <<- keep_tweet
            
            
            if (keep_tweet) {
                shinyjs::show("verb")
                shinyjs::show("clause")
                shinyjs::show("internetes")
                shinyjs::show("phonemic")
                shinyjs::show("verb_agreement")
                shinyjs::show("tma")
                shinyjs::show("intervening_material")
                shinyjs::show("negation")
                shinyjs::show("order")
                shinyjs::show("reference")
            } else {
                shinyjs::hide("verb")
                shinyjs::hide("clause")
                shinyjs::hide("internetes")
                shinyjs::hide("phonemic")
                shinyjs::hide("verb_agreement")
                shinyjs::hide("tma")
                shinyjs::hide("intervening_material")
                shinyjs::hide("negation")
                shinyjs::hide("order")
                shinyjs::hide("reference")
            }
        })
        
    })
    
    observe({
        input$previous_sent
        isolate({
            sel <- input$tweet_number
            sel <- as.numeric(sel) - 1
            if (sel < 1){
                sel <- 1
            }
            
            updateSelectInput(session, "tweet_number",
                              selected = sel)
        })
    })
    
    observe({
        input$next_sent
        isolate({
            sel <- input$tweet_number
            sel <- as.numeric(sel) + 1
            if (sel > max_sent){
                sel <- max_sent
            }
            
            updateSelectInput(session, "tweet_number",
                              selected = sel)
            
        })
    })
    
    observe({
        input$tweet_number
        
        isolate({
            
            sel <- as.numeric(input$tweet_number)
            
            update_code_display(session, input, sel)
            
            
        })

    })

    
    output$downloadData <- downloadHandler(
        filename = function() {
            this_file_name <- input$user_file
            
            paste0("pronoun-coded-", this_file_name, 
                   '-', gsub('\\s|:','_',Sys.time()), ".tsv", sep="")
        },
        content = function(file) {
            write_tsv(data_to_code, file)
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
