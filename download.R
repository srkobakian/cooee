download_files <- function(){

  drive_auth()
  
  this_year <- drive_ls(pattern = "2022", team_drive = "MBAT Testing - share")
  
  all_individual <- drive_ls(path = as_dribble(this_year), team_drive = "MBAT Testing - share")
  
  pb <- progress_bar$new(total = nrow(all_individual),
                         format = "Folders to scan [:bar] :current / :total")
  
  on_drive <- all_individual %>% 
    mutate(rn = dplyr::row_number()) %>% 
    nest(-rn) %>% 
    mutate(sop = purrr::map(data, ~{pb$tick(); drive_ls(path = as_dribble(.x), 
                                            team_drive = "MBAT Testing - share", 
                                            pattern = "SoP.pdf")})) %>% 
    unnest(sop) %>% 
    pull(name)
  
  
  current <- list.files("SoP/")
  
  to_get <- setdiff(on_drive, current)
  
  if (!is_empty(to_get)){
    cat(file=stderr(), "Files to get: \n", paste(to_get, collapse = "\n"), "\n")
    for (i in 1:length(to_get)){
      filedownload(to_get[i])
    }
    
  }
  
}

# download a single file, allow a reasonable return value
# allow downloads to continue and report errors if found

filedownload <- function(name){
  cat(file=stderr(), name, "\n")
  out <- tryCatch(
    {drive_download(name, 
                    path = paste0("./SoP/", name), 
                    overwrite = TRUE)},
    error = function(cond) {
      message(paste("Error: ", name, " does not identify at least one Drive file."))
      return(NA)
    })
}

