library(shiny)
library(shinydashboard)
library(googledrive)
library(googlesheets4)
library(DT)
library(pdftools)
library(tidyverse)
library(tidytext)
library(topicmodels)

#email_address <- "dicook@monash.edu"
email_address <- "stephanie.kobakian@monash.edu"
#gsheet <- "1LLEX7uxrjmrpCJh_5eTS4Ugham8OgbSndj-vBUlwY4U"
gsheet <- "1xm-yqbHY07ELYNWiirA6y4VKaufJdGQdKvL3STq3vcI" # for testing
# sheet_num <- 2 # Worksheet used for application summary
sheet_num <- 2 # Worksheet

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    source("helpers.R")
    
    #drive_auth(email = "stephanie.kobakian@monash.edu")
    #gargle::gargle_oauth_email("stephanie.kobakian@monash.edu"))
    #gs4_auth(email = "stephanie.kobakian@monash.edu", token = drive_token())
    drive_auth(email = email_address)
    #gargle::gargle_oauth_email("stephanie.kobakian@monash.edu"))
    gs4_auth(email = email_address, token = drive_token())
    
    v <- reactiveValues(
        data = list(),
        decisions = list(),
        statements = list(),
        statements_names = list(),
        online = FALSE,
        lastSync = "Never",
        ID = 1,
        email = "none",
        firstRun = TRUE
    )
    
    #Access the google sheet
    isolate({
        
        cat(file=stderr(), "Get sheet\n")
        notif_data <- showNotification("Constructing dataset", duration = NULL)
        # Applications google sheet
        #gsapps <- gs4_get("1xm-yqbHY07ELYNWiirA6y4VKaufJdGQdKvL3STq3vcI")
        gsapps <- gs4_get(gsheet)
        
        ## Download data
        cat(file=stderr(), "Get data from sheet\n")
        v$data <- gsapps %>% 
            # demog submission info on sheet 2
            read_sheet(sheet = sheet_num) %>% tail(-1) %>% 
            mutate(id = seq_len(NROW(.)))
        
        v$email <- gs4_user()
        v$firstRun <- FALSE
        v$lastSync <- Sys.time()
        
        # Pull Statements
        
        # analyse text from Statements
        # Read in all statements
        cat(file=stderr(), "Get text from statements\n")
        
        # create names of all statements
        pdfnames <- v$data %>% 
            mutate(name = 
                       paste0(Surname, ", ", `Given Name`, " ",
                              `Monash ID`, " - ", "SoP.pdf")) %>% 
            pull(name)
        
        files_to_get <- drive_find(pattern = "SoP.pdf", type = "pdf")
        
        # names of current statements
        current_statements <- list.files("SoP/")
        
        # get the list of missing SoPs
        to_get <- files_to_get$name[!(files_to_get$name %in% current_statements)]
        #    filter(`Monash ID` %in% to_get) %>% 
        
        cat(file=stderr(), "Files to get: \n", paste(to_get, collapse = "\n"), "\n")
        
        # Download statements
        dl_sop <- showNotification("Downloading Statements of Purpose, this may take a while.", duration = NULL)
    
        if (length(to_get)>0){    
        downloads <- lapply(to_get, FUN = filedownload)
        }
    
        # Use pdf tools to extract text from statement
        v$statements <- statements <- purrr::map(pdfnames, function(a){
            file.path("SoP", a) %>% pdf_text() %>% str_trim() %>% toString()})
        
        # make sure amount of statements match data for student submissions
        v$statement_names <- pdfnames
        
        removeNotification(dl_sop)
        
        removeNotification(notif_data)
    })
    
    tbl_filtered_data <- reactive({
        notif_tbl <- showNotification("Filtering table")
        
        cat(file=stderr(), "Filter data for table\n")
        out <- v$data %>%
            replace_na(list(`Clearly In (offer)` = 0,
                            `Pending (may select on close of applications)` = 0, 
                            `No offer (reject)` = 0)) %>%
            mutate(similarity = fuzzyMatching(input$text_match, .)) %>%
            arrange(desc(similarity)) %>%  
            # check this
            mutate(`WAM/GPA` = {  unlist(`WAM/GPA`) %>% 
                    parse_number(., na = c("", "NA", "TBC")) %>% 
                    replace_na(50)}) %>% 
            mutate(`WAM/GPA` = ifelse(`WAM/GPA` < 5,`WAM/GPA`*25, `WAM/GPA`)) %>% 
            # remove previously considered applications
            filter(!(`Clearly In (offer)`) == 1,
                   !(`No offer (reject)`) == 1) %>% 
            filter(between(`WAM/GPA`, input$slider_wam[1], input$slider_wam[2])) %>%
            rowwise %>%
            filter(all(`Compl Stats Unit? Y/N` %in% input$stats_background)) %>%
            ungroup
        
        if (all(input$decided == "N")) {
            out <- out %>% filter(`No offer (reject)` == 0,
                                  `Pending (may select on close of applications)` == 0,
                                  `Clearly In (offer)` == 0)
        }
        
        
        removeNotification(notif_tbl)
        out
    })
    
    output$tblapplicants <- DT::renderDT({
        
        cat(file=stderr(), "Construct Datatable\n")
        notif_table <- showNotification("Constructing datatable")
        
        out <- tbl_filtered_data() %>% 
            select(`Monash ID`, `M/F`,  WAM = `WAM/GPA`, 
                   Location = `Domestic/   International`,
                   Credit = `Applicant applied for credit? Y/N`,
                   Qualification,
                   Statistics = `Compl Stats Unit? Y/N`, 
                   `No offer (reject)`, 
                   `Pending (may select on close of applications)`, 
                   `Clearly In (offer)`) %>% 
            mutate(decision = case_when(`No offer (reject)` == 1 ~ "No",
                                        `Pending (may select on close of applications)` == 1 ~ "Maybe",
                                        `Clearly In (offer)` == 1 ~ "Yes")) %>% 
            select(-`No offer (reject)`, 
                   -`Pending (may select on close of applications)`, 
                   -`Clearly In (offer)`) %>% 
            datatable(rownames = FALSE, 
                      selection = list(mode = "single"),
                      style = "bootstrap", 
                      class = "hover",
                      options = list(sDom  = '<"top">irt<"bottom">p',
                                     scrollX = TRUE))
        
        
        # colour the rows by decision
        if (any(input$decided == "Y")) {
            out <- out %>%
                # if decision is no
                formatStyle("decision",
                            target = 'row',
                            backgroundColor = styleEqual(c("No", "Maybe", "Yes", "Unknown"), 
                                                         c("red", "orange", "green", "grey"))) 
        }
        
        removeNotification(notif_table)
        
        out
        
        
    })

    
    
    # UI for abstract
    output$abstract <- renderUI({
        if(is.null(input$tblapplicants_rows_selected)){
            return(
                box(title = "Select an applicant from the sidebar to review their application",
                    width = 12)
            )}
        
    })
    
    # UI for abstract updated when selected
    observeEvent(input$tblapplicants_rows_selected,{
        
        
        updateTabItems(session, "tabs", "abstract")
        
        v$ID <- tbl_filtered_data() %>% 
            filter(row_number() == input$tblapplicants_rows_selected) %>%
            pull(`Monash ID`)
        
        
        output$abstract <- renderUI({
            if(is.null(input$tblapplicants_rows_selected)){
                return(
                    box(title = "Select an applicant from the sidebar to review their application",
                        width = 12)
                )
            }
            
            applicant_data <- tbl_filtered_data() %>%
                filter(`Monash ID` == v$ID)
            
            # Get statement of purpose
            pdfname <- applicant_data %>% 
                mutate(name = paste0(Surname, ", ", `Given Name`, " ", v$ID, " ", "SoP.pdf")) %>% pull(name)
            
            SoPtext <- pdf_text(file.path("SoP", pdfname)) %>% paste()
            
            # Show the Statement of Purpose
            box(
                width = "500px",
                title = "Statement of Purpose",
                formText(SoPtext),
                hr())
            
        })  
    })
    
    
    
    output$review <- renderUI({
        if(is.null(input$tblapplicants_rows_selected)){
            return()
        }
        
        box(width = 12,
            title = "Decision",
            solidHeader = TRUE,
            status = "info",
            column(3,
                   radioButtons("accept", 
                                label = "Select One:", 
                                choices = c("Offer", "Maybe", "Reject"),
                                selected = "Maybe"
                   ),
                   uiOutput("ui_save")
            ),
            column(9,
                   textAreaInput("comment",
                                 label = "Comments", 
                                 rows = 6
                   )
            )
        )
    })
    
    
    
    observeEvent(input$save, {
        
        # Get row for review
        new_decision <- v$data %>% 
            filter(`Monash ID` == v$ID) %>% 
            mutate(`Owner & Date` = paste(format(Sys.time(), tz="GMT"), v$email),
                   `Clearly In (offer)` = 
                       ifelse(input$accept == "Offer", 1, 0),
                   `Pending (may select on close of applications)`= 
                       ifelse(input$accept == "Maybe", 1, 0),
                   `No offer (reject)` = 
                       ifelse(input$accept == "Reject", 1, 0),
                   Query = input$comment)
        
        # Add to list of changes
        v$decisions <- new_decision
        
        # upload decision to spreadsheet
        v$changes <- uploadChanges(changes = v$decisions)
        
    })
    
    
    
    # Upload changes to the google sheet
    uploadChanges <- function(changes){
        
        notif_save <- showNotification("Uploading review.")
        if(NROW(changes) > 0){
            # find row of data to replace based on ID, allow for headers in A and B
            replacing_row <- as.character(which(v$data$`Monash ID` == v$ID) + 2)
            
            # Remove row without a decision
            range_delete(ss = gsheet, range = replacing_row)
            
            # Remove the id column
            changes <- changes %>% select(-id)
            
            # Add the decision to the data set
            sheet_append(ss = gsheet, data = changes)
            
            # Get the new updated data
            gsapps <- gs4_get(gsheet)
            
            ## Download data
            v$data <- gsapps %>% 
                # demog submission info on sheet 1
                read_sheet(sheet = sheet_num) %>% tail(-1) %>% mutate(id = seq_len(NROW(.)))
            
            
            # Clear changes 
            v$changes <- list()
            
        }
        removeNotification(notif_save)
        return(changes)
    }
    
    
    output$uniquewords <- renderPlot({
        
        statements_tf_idf <- getUniqueWords(v$statement_names, v$statements) %>% 
            mutate(ID = parse_number(ID))
        
        statements_tf_idf <- statements_tf_idf %>%
            group_by(ID) %>%
            slice_max(tf_idf, n = 12, with_ties = FALSE) %>%
            ungroup() 
        
        # if (!is.null(input$commonwords_rows_selected)){
        #   statements_tf_idf %>% select(ID, word) %>% 
        #     filter(word == input$commonwords_rows_selected$word)
        #   
        # }
        
        statements_tf_idf %>%
            ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = word)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~ID, ncol = 4, scales = "free") +
            labs(x = "tf-idf", y = NULL) +
            scale_fill_viridis_d(option = "inferno")
        
    })
    
    output$lda <- renderPlot({
        
        # Change topics to allow more or less groups
        top_terms <- tibble(ID = v$statement_names, statement = unlist(v$statements)) %>%
            LDAwords(., topics = 5)
        
        top_terms %>%
            mutate(term = reorder_within(term, beta, topic)) %>%
            slice_max(beta, n = 15) %>% 
            ungroup() %>% 
            ggplot(aes(beta, term)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~ topic, scales = "free") +
            scale_y_reordered() +
            scale_fill_viridis_d(option = "inferno")
        
    })
    
    output$commonwords <- DT::renderDT({
        
        notif_statements <- showNotification("Updating Statements")
            
        # Attempt to remove names from statements for privacy
        name_words <- tibble(word = c(v$data$`Given Name`, v$data$Surname))
            
        # statements as a data set
        commonwords <- tibble(ID = v$statement_names, statement = unlist(v$statements)) %>%
            unnest_tokens(word, statement) %>%
            anti_join(stop_words) %>%
            anti_join(name_words) %>%
            group_by(ID, word) %>% 
            count(sort = TRUE) %>%
            ungroup() %>% 
            group_by(word) %>% 
            summarise(Statements = n(), Mentions = sum(n)) %>% 
            ungroup() %>% 
            mutate(word = reorder(word, Mentions)) %>% 
            slice_max(Mentions, n = 15) %>% 
            datatable(rownames = FALSE, 
                      selection = list(mode = "single"),
                      style = "bootstrap", 
                      class = "hover",
                      options = list(sDom  = '<"top">irt<"bottom">p',
                                     scrollX = TRUE), extensions = 'Responsive')
        
        removeNotification(notif_statements)
        
        commonwords 
    })
    
    
    
    observeEvent(input$commonwords_rows_selected,{
        # Update
        
        message("Output selected applicant")
        
        v$ID <- tbl_filtered_data() %>% 
            filter(row_number() == input$tblapplicants_rows_selected) %>%
            pull(`Monash ID`)
    })
    
    selectedRows <- reactive({
        unique(
            c(input[["commonwords_rows_selected"]])
        )
    })
    
    output[["selectedRows"]] <- renderText({
        selectedRows()
    })
    
    
})
