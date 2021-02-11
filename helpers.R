has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}

# function to split lines read into character strings
# to replicate line and paragraph spacing of document
formText <- function(...){
  paste(...) %>%
    strsplit("\n", fixed=TRUE) %>%
    unlist %>%
    map(~ p(.)) %>%
    tagList
}

fuzzyMatching <- function(pattern, dataset){
  browser()
  if(pattern == ""){
    return(0)
  }
  dataset %>%
    split(seq_len(NROW(.))) %>% 
    map(~ as.character(.) %>% strsplit(" ") %>% unlist) %>%
    map_dbl(~ sum(agrepl(gsub(" ", "|", pattern), .))/length(.))
}

# download a single file, allow a reasonable return value
# allow downloads to continue and report errors if found

filedownload <- function(name){
  out <- tryCatch(
    {drive_download(name, path = file.path("SoP", name))},
    error = function(cond) {
      message("Check that the file exists on the drive:")
      message(paste("Error: file name does not identify at least one Drive file."))
      return(NA)
    })
  }

  
