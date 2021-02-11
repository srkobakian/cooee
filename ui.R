library(shiny)
library(shinydashboard)
library(googlesheets)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "MBAT Application Review",
      titleWidth = "475px"
    ),
    dashboardSidebar(
      width = "475px",
      uiOutput("auth"),
      uiOutput("sync"),
      #radioButtons("net_mode", "Net Mode", c("Online", "Offline"), inline = TRUE),
      #radioButtons("admin_mode", "Operation Mode", c("Reviewer", "Administrator"), inline = TRUE),
      sidebarMenu(
        
        menuItem("Application Summaries", tabName = "summaries", icon = icon("dashboard")),
        menuItem("Application Filtering", tabName = "applications", icon = icon("th")),
        
        tags$li(
          actionLink("btn_debug",
                     style = "margin: 0;",
                     label = NULL,
                     class = "",
                     icon("bug"),
                     span("Debug")
          )
        ),
        tags$li(
          actionLink("btn_sync",
                     style = "margin: 0;",
                     label = NULL,
                     class = "",
                     icon("refresh"),
                     span("Synchronise")
          )
        )
      )
      # uiOutput("n_entries"),
    ),
    dashboardBody( 
      tabItems(
        # First tab for interacting
        tabItem(tabName = "summaries",
                tags$head(
                  tags$link(rel = "stylesheet", 
                            type = "text/css", 
                            href = "custom.css")),
                
                textInput("text_match", "Fuzzy text sorting"),
                
                uiOutput("summaries")
        ),
        tabItem(tabName = "Application View",
                
                uiOutput("abstract"),
                uiOutput("review")
                ),
        # second tab to display the applications
        tabItem(tabName = "applications",
                
                
                fluidRow(
                  column(4, 
                  plotOutput("countryplot", width = "300px", height = "50px"),
                  dataTableOutput("countrytext")),
                  column(4, 
                  plotOutput("genderplot", width = "300px", height = "50px"),
                  dataTableOutput("gendertext"))),
                
                hr("Filter and choose applications"),
                fluidRow(
                  column(4, checkboxGroupInput("decided", "Decision\nmade", choices = c("Y", "N"), selected = "N")),
                  column(4, checkboxGroupInput("stats_background", "Statistics\nBackground", choices = c("Y", "N"), selected = c("Y", "N"))),
                  column(4, sliderInput("slider_wam", "WAM",
                                        min = 50, max = 100, value = c(50, 100), step=5))
                ),
                DT::dataTableOutput("tbl_applicants")
        )
      )
    )
  )
)
