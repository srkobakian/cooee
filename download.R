library(googledrive)
library(googlesheets4)
library(pdftools)
library(tidyverse)
library(progress)

myteamdr <- "B6022 - Master of Business Analytics"
folder <- "S1-2022 APPLICATION DOCUMENTS"
download_files(folder, myteamdr)

# The function assumes the following file structure in the google drivethat applications are stored in a shared drive on googledrive (teamdr) and 
# the SoPs of interest are stored inside a folder (pttn) in the shared drive
# 
# B6022 - Master of Business Analytics (teamdr)
# ├── S1-2022 APPLICATION DOCUMENTS (pttn)
# │   └── Wallaby, Red 56781234
# │       └── Wallaby, Red 56781234 SoP.pdf
# │       └── ...
# │   ├── Rosella, Green 81234567
# │       └── Rosella, Green 81234567 SoP.pdf
# │       └── ...
# │   ├── ...
# ├── S1-2023 APPLICATION DOCUMENTS (pttn)
# │   └── Numbat, Orange 34567812
# │       └── Numbat, Orange 34567812 SoP.pdf
# │   └── ..
# where `teamdr` is the google drive name of a shared drive and `pttn` is the folder name of the year of interest

download_files <- function(pttn = "2022", teamdr = "MBAT Testing - share"){

  drive_auth()
  
  # extract the metadata of current year folder
  this_year <- drive_ls(pattern = pttn, team_drive = teamdr)
  
  # extract the metadata of each individual 
  all_individual <- drive_ls(path = as_dribble(this_year), team_drive = teamdr)
  
  pb <- progress_bar$new(total = nrow(all_individual),
                         format = "Folders to scan [:bar] :current / :total")
  
  # extract the metadata of the SoP.pdf file for each individual
  on_drive <- all_individual %>% 
    mutate(rn = dplyr::row_number()) %>% 
    nest(-rn) %>% 
    mutate(sop = purrr::map(data, ~{pb$tick(); drive_ls(path = as_dribble(.x), 
                                            team_drive = teamdr, 
                                            pattern = "SoP.pdf")})) %>% 
    unnest(sop)
  
  # find the SoP to download by comparing the those SoP.pdf on drive and those stored locally in SoP/ folder
  current <- list.files("SoP/")
  to_get <- setdiff(on_drive$name, current)
  to_get_df <- on_drive %>% filter(name %in% to_get)
  
  # download the SoP.pdf
  if (!is_empty(to_get_df)){
    for (i in 1:nrow(to_get_df)){
      filedownload(to_get_df$id[i], to_get_df$name[i])
    }
    
  }
  
}

# download a single file, allow a reasonable return value
# allow downloads to continue and report errors if found

filedownload <- function(id, name){
  out <- tryCatch(
    {drive_download(as_id(id), 
                    path = paste0("./SoP/", name), 
                    overwrite = TRUE)},
    error = function(cond) {
      message(paste("Error: ", name, " does not identify at least one Drive file."))
      return(NA)
    })
}

