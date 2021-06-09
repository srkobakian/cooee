library(googledrive)
library(googlesheets4)
library(pdftools)
library(tidyverse)
library(progress)

myteamdr <- "B6022 - Master of Business Analytics"
folder <- "S1-2022 APPLICATION DOCUMENTS"
download_files(folder, myteamdr)

download_files <- function(pttn = "2022", teamdr = "MBAT Testing - share"){

  drive_auth()
  
  this_year <- drive_ls(pattern = pttn, team_drive = teamdr)
  
  all_individual <- drive_ls(path = as_dribble(this_year), team_drive = teamdr)
  
  pb <- progress_bar$new(total = nrow(all_individual),
                         format = "Folders to scan [:bar] :current / :total")
  
  on_drive <- all_individual %>% 
    mutate(rn = dplyr::row_number()) %>% 
    nest(-rn) %>% 
    mutate(sop = purrr::map(data, ~{pb$tick(); drive_ls(path = as_dribble(.x), 
                                            team_drive = teamdr, 
                                            pattern = "SoP.pdf")})) %>% 
    unnest(sop)
  
  
  current <- list.files("SoP/")
  
  to_get <- setdiff(on_drive$name, current)
  
  to_get_df <- on_drive %>% filter(name %in% to_get)
  
  if (!is_empty(to_get_df)){
    for (i in 1:nrow(to_get_df)){
      filedownload(to_get_df$id[i], to_get_df$name[i])
    }
    
  }
  
}

# download a single file, allow a reasonable return value
# allow downloads to continue and report errors if found

filedownload <- function(id, name){
  #cat(file=stderr(), name, "\n")
  out <- tryCatch(
    {drive_download(as_id(id), 
                    path = paste0("./SoP/", name), 
                    overwrite = TRUE)},
    error = function(cond) {
      message(paste("Error: ", name, " does not identify at least one Drive file."))
      return(NA)
    })
}

