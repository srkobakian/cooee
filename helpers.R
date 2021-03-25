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
  cat(file=stderr(), name, "\n")
  out <- tryCatch(
    {drive_download(name, path = file.path("SoP", name))},
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
