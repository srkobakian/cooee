# This is old code, and has been replaced with download.R

# Prep code, as used in app
library(shiny)
library(shinydashboard)
library(googledrive)
library(googlesheets4)
library(DT)
library(pdftools)
library(SnowballC)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(progress)

email_address <- "dicook@monash.edu"
#email_address <- "stephanie.kobakian@monash.edu"
#email_address <- "Huize.Zhang@monash.edu"
gsheet <- "1LLEX7uxrjmrpCJh_5eTS4Ugham8OgbSndj-vBUlwY4U"
# gsheet <- "1xm-yqbHY07ELYNWiirA6y4VKaufJdGQdKvL3STq3vcI" # for testing
# sheet_num <- 2 # Worksheet used for application summary
sheet_num <- 2 # Worksheet
drive_auth(email = email_address)
#gargle::gargle_oauth_email("stephanie.kobakian@monash.edu"))
gs4_auth(email = email_address, token = drive_token())

gsapps <- gs4_get(gsheet)
data <- gsapps %>% 
  # demog submission info on sheet 2
  read_sheet(sheet = sheet_num) %>% tail(-1) %>% 
  mutate(id = seq_len(NROW(.)))

pdfnames <- data %>% 
  mutate(name = 
           paste0(Surname, ", ", str_to_title(`Given Name`), " ",
                  `Monash ID`, " - SoP.pdf")) %>% 
  pull(name)

current_statements <- list.files("SoP/")
keep <- pdfnames %in% current_statements
cat("These applicants have no statements: \n")
for (i in pdfnames[!keep])
  cat(i, "\n")
pdfnames <- pdfnames[keep]

download_files()

# download a single file, allow a reasonable return value
# allow downloads to continue and report errors if found

filedownload <- function(name){
  cat(file=stderr(), name$name, "\n")
  out <- tryCatch(
    {drive_download(name, 
                    path = paste0("./SoP/", name$name), 
                    overwrite = TRUE)},
    error = function(cond) {
      message(paste("Error: ", name, " does not identify at least one Drive file."))
      return(NA)
    })
}

# folder = "MBAT Testing"
# folder = "S1-2022 APPLICATION DOCUMENTS"
# try = "1nPEfJm0JB4bz5INZqzjwSHEUMPIi4ZYX"
download_files <- function(folder = "1nPEfJm0JB4bz5INZqzjwSHEUMPIi4ZYX"){
  
  drive_auth()
  
  on_drive <- drive_ls(path = folder, recursive = TRUE, pattern = "SoP.pdf", team_drive = "B6022 - Master of Business Analytics")
  
  current <- list.files("SoP/")
  
  to_get <- on_drive[!(on_drive$name %in% current),]
  
  if (!is_empty(to_get)){
    cat(file=stderr(), "Files to get: \n", paste(to_get$name, collapse = "\n"), "\n")
  }
  
  for (i in 1:nrow(to_get)){
    filedownload(to_get[i,])
  }
  
}
