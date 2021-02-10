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
      ),
      # uiOutput("n_entries"),
      hr(),
      fluidRow(
        column(4, checkboxGroupInput("decided", "Decision\nmade", choices = c("Y", "N"), selected = "N")),
        column(4, sliderInput("slider_wam", "WAM",
                              min = 0, max = 100, value = c(50, 100), step=5)),
        column(4, checkboxGroupInput("stats_background", "Statistics\nBackground", choices = c("Y", "N"), selected = c("Y", "N")))
      ),
      textInput("text_match", "Fuzzy text sorting"),
      DT::dataTableOutput("tbl_applicants")
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        
      ),
      
      uiOutput("abstract"),
      uiOutput("review")
    )
  )
)
