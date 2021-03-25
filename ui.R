#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        dashboardHeader(
            title = "MBAT Application Review",
            titleWidth = "375px"
        ),
        dashboardSidebar(
            width = "375px",
            sidebarMenu(
                id = "tabs",
                
                
                textInput("text_match", "Fuzzy text sorting"),
                sliderInput("slider_wam", "WAM",
                            min = 50, max = 100, value = c(50, 100), step=5),
                
                checkboxGroupInput("decided", "Decision\nmade", choices = c("Y", "N"), selected = "N"),
                checkboxGroupInput("stats_background", "Statistics\nBackground", choices = c("Y", "N"), selected = c("Y", "N")),
                
                menuItem("Application Summaries", tabName = "summaries", icon = icon("dashboard")),
                
                menuItem("Application View", tabName = "abstract", icon = icon("dashboard"))
            )
        ),
        
        dashboardBody( 
            tabItems(
                # First tab for interacting
                tabItem(tabName = "summaries",
                        
                        h1("Applications"),
                        # data table of commonly used words
                        DT::DTOutput("tblapplicants"),
                        
                        h1("Commonly used words in applications"),
                        # data table of commonly used words
                        DT::DTOutput("commonwords"),
                        
                        h1("Topics mentioned"),
                        # filter according to search term
                        plotOutput("lda"),
                        
                        h1("Unique words used in statements"),
                        # data table of unique words found
                        plotOutput("uniquewords")
                        
                ),
                
                
                tabItem(tabName = "abstract",
                uiOutput("abstract"),
                uiOutput("review"))
            )
        )
    )
)
