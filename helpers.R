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

count_words_SoP <- function(data){
  
  out <- data %>% 
    unnest_tokens(word, statement) %>%
    anti_join(stop_words) %>%
    count(ID, word, sort = TRUE) %>%
    filter(!is.na(iconv(word, "latin2", "ASCII"))) %>% #only English
    filter(!str_detect(word, "^[0-9]")) %>%
    mutate(l = str_length(word)) %>%
    filter(l > 2) %>%
    select(-l)
   
  out
}

tf_idf_words <- function(id_words) {
  book_tf_idf <- id_words %>%
    bind_tf_idf(word, ID, n)
  
  book_tf_idf
}

LDAwords <- function(data, topics = 5, matrix = "beta"){
  
  out <- data %>% 
    unnest_tokens(word, statement) %>%
    anti_join(stop_words) %>%
    count(ID, word, sort = TRUE) %>%
    filter(!is.na(iconv(word, "latin2", "ASCII"))) %>% #only English
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

