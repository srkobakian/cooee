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
  
  on_drive <- drive_ls(path = folder, recursive = TRUE, pattern = "SoP.pdf", team_drive = "Master of Business Analytics B6022")
  
  current <- list.files("SoP/")
  
  to_get <- on_drive[!(on_drive$name %in% current),]
  
  if (!is_empty(to_get)){
    cat(file=stderr(), "Files to get: \n", paste(to_get$name, collapse = "\n"), "\n")
  }
  
  for (i in 1:nrow(to_get)){
    filedownload(to_get[i,])
  }
  
}
