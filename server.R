library(shiny)
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(DT)
library(jsonlite)
library(purrr)
library(googledrive)
library(pdftools)
library(ggplot2)
library(glue)



shinyServer(
  function(input, output, session) {
    source("helpers.R")
    
    # load in previous decisions if there is a cache
    if("cache_abstracts.Rdata" %in% list.files()){
      notif_cache <- showNotification("Loading cache")
      load("cache_abstracts.Rdata")
      
      if(exists("v")){
        if(is.reactivevalues(v)){
          # Update cache format
          cache <- isolate(reactiveValuesToList(v))
        }
      }
      v <- do.call(reactiveValues, cache)
      
      removeNotification(notif_cache)
    }
    else{
      v <- reactiveValues(
        data = list(),
        decisions = list(),
        statements = list(),
        online = FALSE,
        lastSync = "Never",
        ID = 1,
        email = "none",
        firstRun = TRUE
      )
    }
    
    
    notif_ui <- showNotification("Building UI")
    
    latest_decisions <- reactive({
      notif_data <- showNotification("Constructing dataset")
      out <- v$decisions %>%
        as_tibble %>%
        bind_rows(v$decisions) %>%
        filter(reviewer == v$email) %>%
        group_by(id) %>%
        filter(timestamp == max(timestamp)) %>%
        ungroup
      removeNotification(notif_data)
      out
    })
    
    
    tbl_data <- reactive({
      notif_tbl <- showNotification("Updating table")
      
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
               !(`No offer (reject)`) == 1)
      
      removeNotification(notif_tbl)
      out
    })
    
    tbl_filtered_data <- reactive({
      notif_tbl <- showNotification("Filtering table")
      
      out <- tbl_data() %>% 
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
    
    
    output$auth <- renderUI({
      
      if(!gs4_has_token()){
        # email = TRUE to match with any token in the .secrets cache
        drive_auth(cache = ".secrets", email = "stephanie.kobakian@monash.edu")
        gs4_auth(token = drive_token(), email = "stephanie.kobakian@monash.edu")
      }
      
        sidebarUserPanel(
          # gs4_user alternative for googlesheets4
          span("Authenticated as", gs4_user())
        )
      
    })  
    
    output$countryplot <- renderPlot({
      # Applicant origin
      # Visual summary
      
      tbl_data() %>% ggplot() + 
        geom_bar(aes(x = 1, fill = `Domestic/   International`), show.legend = FALSE) +           scale_fill_viridis_d() + coord_flip() +
        theme_void() + 
        theme(panel.background = element_rect(fill = "darkgrey"))
      
      })
      
    output$countrytext <- DT::renderDataTable({
      
      tbl_data() %>% count(Origin = `Domestic/   International`, name = "Total") %>%
        datatable(rownames = FALSE, 
                  filter = 'top',
                  selection = 'none',
                  style = "bootstrap", 
                  options = list(sDom  = '<"top">irt<"bottom">p',
                                 pageLength = 2, # make this a variable
                                 lengthChange = FALSE,
                                 scrollX = TRUE))
      
    })
    
    output$genderplot <- renderPlot({
      # Applicant origin
      # Visual summary
      
      tbl_data() %>% ggplot() + 
        geom_bar(aes(x = 1, fill = `M/F`), show.legend = FALSE) +           scale_fill_viridis_d() + coord_flip() +
        theme_void() + 
        theme(panel.background = element_rect(fill = "darkgrey"))
      
    })
    
    output$gendertext <- DT::renderDataTable({
      
      tbl_data() %>% count(Gender = `M/F`, name = "Total") %>%
        datatable(rownames = FALSE, 
                  filter = 'top',
                  selection = 'none',
                  style = "bootstrap", 
                  options = list(sDom  = '<"top">irt<"bottom">p',
                                 pageLength = 2, # make this a variable
                                 lengthChange = FALSE,
                                 scrollX = TRUE))
      
    })
    
    ## Get auth code from return URL
    access_token  <- reactive({
      ## gets all the parameters in the URL. The auth code should be one of them.
      pars <- parseQueryString(session$clientData$url_search)
      if (length(pars$code) > 0) {
        ## extract the authorization code
        notif_auth <- showNotification("Authenticating...")
        out <- gs_webapp_get_token(auth_code = pars$code)
        removeNotification(notif_auth)
        out
      }
    })
    
    output$sync <- renderUI({
      strong(paste0("Last synchronised: ", v$lastSync))
    })

    observe({
      if(!v$firstRun & input$btn_sync == 0 & !is.null(v$email)){
        return()
      }
      
      #if (!is.null(access_token())) {
        notif_sync <- showNotification("Synchronising... Please wait",
                                       duration = NULL)
        
        
        isolate({
          
          # Applications google sheet
          gsapps <- gs4_get("1xm-yqbHY07ELYNWiirA6y4VKaufJdGQdKvL3STq3vcI")
          
          ## Download data
          v$data <- gsapps %>% 
            # demog submission info on sheet 1
            read_sheet(sheet = 1) %>% tail(-1) %>% mutate(id = seq_len(NROW(.)))
          
          v$email <- gs4_user()
          v$firstRun <- FALSE
          v$lastSync <- Sys.time()
        })
        
        # pull new statements
        current_sops <- list.files("SoP/") %>% 
          tibble() %>% 
          mutate(files = parse_number(.))
        
        if (any(v$data$`Monash ID` %in% current_sops$files)) {
          
          # get the list of missing SoPs
          to_get <- v$data$`Monash ID`[!(v$data$`Monash ID` %in% current_sops$files)]
          
          # Download from drive
            ui_pdf_download <- showNotification("Downloading Statements of Purpose", duration = NULL)
            
            if (length(to_get) > 0){
            pdfnames <- v$data %>% 
              filter(`Monash ID` %in% to_get) %>% 
              mutate(name = paste0(Surname, ", ", `Given Name`, " ", `Monash ID`, " ", "SoP.pdf")) %>% pull(name)
            
            lapply(pdfnames, FUN = filedownload)
            }
            
            removeNotification(ui_pdf_download)
          }
          
          
        removeNotification(notif_sync)
     # }
    })
     
    observe({
      if(length(v$decisions) > 0){
        dt <- tbl_data()
      }
    })

    output$tbl_applicants <- DT::renderDataTable({
      if(NROW(v$data) > 0){
        ui_tbl_selector <- showNotification("Building table selector")
        
        out <- tbl_filtered_data() %>% 
          select(`M/F`,  WAM = `WAM/GPA`, 
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
        
        
        removeNotification(ui_tbl_selector)
        out
      }
      else{
        NULL
      }
    })
    
    observeEvent(input$tbl_applicants_rows_selected,{
      # Update
     
      
      v$ID <- tbl_filtered_data() %>% 
        filter(row_number() == input$tbl_applicants_rows_selected) %>%
        pull(`Monash ID`)
      
        
      output$abstract <- renderUI({
        if(is.null(input$tbl_applicants_rows_selected)){
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
        
        # Replace with pulling text from reactive value list
        # Use pdf tools to extract text from statement
        SoPtext <- pdf_text(file.path("SoP", pdfname)) %>% paste()
        
        # Show the Statement of Purpose
        box(
          width = "500px",
          title = "Statement of Purpose",
          formText(SoPtext),
          hr())
        
      })
      
      
      output$review <- renderUI({
        if(is.null(input$tbl_applicants_rows_selected)){
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
      
      
      output$ui_save <- renderUI({
        actionLink(
          "save",
          box(
            p("Save", style="text-align: center;"),
            width = NULL,
            background = switch(input$accept,
                                `Clearly In` = "green",
                                Pending = "light-blue",
                                Reject = "red")
          )
        )
      })
      

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
        range_delete(ss = "1xm-yqbHY07ELYNWiirA6y4VKaufJdGQdKvL3STq3vcI", range = replacing_row)
        
        # Remove the id column
        changes <- changes %>% select(-id)
        
        # Add the decision to the data set
        sheet_append(ss = "1xm-yqbHY07ELYNWiirA6y4VKaufJdGQdKvL3STq3vcI",           data = changes)
        
        # Get the new updated data
        gsapps <- gs4_get("1xm-yqbHY07ELYNWiirA6y4VKaufJdGQdKvL3STq3vcI")
        
        ## Download data
        v$data <- gsapps %>% 
          # demog submission info on sheet 1
          read_sheet(sheet = 1) %>% tail(-1) %>% mutate(id = seq_len(NROW(.)))
        
        
        # Clear changes 
        v$changes <- list()
        
      }
      removeNotification(notif_save)
      return(changes)
    }
    
    
    output$summaries <- renderUI({
      
      input$text_match
      
      
      # analyse text from Statements
      # Read in all statements
      current_statements <- v$statements$names
        browser()
        # get the list of missing SoPs
        to_get <- v$data$`Monash ID`[!(v$data$`Monash ID` %in% current_statements)]
        
        if (length(to_get) > 0){
          pdfnames <- v$data %>% 
            filter(`Monash ID` %in% to_get) %>% 
            mutate(name = 
                     paste0(Surname, ", ", `Given Name`, " ",
                            `Monash ID`, " ", "SoP.pdf")) %>% 
            pull(name)
          
          
        # Use pdf tools to extract text from statement
        v$statements <- purrr::map(pdfnames, function(a){
          file.path("SoP", a) %>% pdf_text() %>% paste()})
        
        
        v$statement_ids <- to_get
        # Will have to join to previous list of statements
        # fairly quick to pull the text if we cannot concatenate
        }
      
    })
    

    observeEvent(input$btn_debug, {
      browser()
    })
    
    
    onStop(function(){
      cache <- isolate(reactiveValuesToList(v))
      save(cache, file = "cache_abstracts.Rdata")
    })
    
    removeNotification(notif_ui)
  }
)