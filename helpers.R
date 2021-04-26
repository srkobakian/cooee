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
  #browser()
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



LDAwords <- function(data, topics = 5, matrix = "beta"){
  
  out <- data %>% 
    unnest_tokens(word, statement) %>%
    anti_join(stop_words) %>%
    count(ID, word, sort = TRUE) %>%
    ungroup() %>%
    cast_dtm(ID, word, n) %>% 
    LDA(., k = topics, control = list(seed = 1234)) %>% 
    tidy(., matrix = matrix)
  out
}


getUniqueWords <- function(statement_names = v$statement_names, statements = v$statements){
  
  statements_words <- tibble(ID = statement_names, statement = unlist(statements)) %>%
    unnest_tokens(word, statement) %>%
    anti_join(stop_words) %>%
    count(ID, word, sort = TRUE)
  
  # Remove student name
  
  total_words <- statements_words %>%
    group_by(ID) %>% 
    summarise(total = sum(n))
  
  statements_words <- left_join(statements_words, total_words)
  
  out <- statements_words %>% 
    group_by(ID) %>% 
    mutate(rank = row_number(), 
           `term frequency` = n/total) %>%
    ungroup() %>%
    bind_tf_idf(word, ID, n)%>%
    arrange(desc(tf_idf)) 
  
  out
}

download_files <- function(){
  
  drive_auth()
  
  on_drive <- drive_ls(path = "MBAT Testing", recursive = TRUE, pattern = "SoP.pdf")
  
  current <- list.files("SoP/")
  
  to_get <- on_drive[!(on_drive$name %in% current),]
  
  if (!is_empty(to_get)){
    cat(file=stderr(), "Files to get: \n", paste(to_get$name, collapse = "\n"), "\n")
  }
  
  for (i in 1:nrow(to_get)){
    filedownload(to_get[i,])
  }
  
}
