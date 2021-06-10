# Functions to do analysis outside the shiny app
library(shiny)
library(shinydashboard)
library(googledrive)
library(googlesheets4)
library(DT)
library(pdftools)
library(SnowballC)
library(tidyverse)
library(tidytext)
library(textcat)
library(topicmodels)
library(progress)
library(widyr)
library(ggraph)
library(igraph)
library(plotly)

email_address <- "dicook@monash.edu"
gsheet <- "1LLEX7uxrjmrpCJh_5eTS4Ugham8OgbSndj-vBUlwY4U"
sheet_num <- 2 # Worksheet

drive_auth(email = email_address)
gs4_auth(email = email_address, token = drive_token())

# Get sheet
gsapps <- gs4_get(gsheet)

# Extract data
applications <- gsapps %>% 
  read_sheet(sheet = sheet_num) %>% 
  slice(-1)

# Analysis of applications
# Fix UG WAM
applications <- applications %>% 
  mutate(`UG WAM/GPA` = ifelse(`UG WAM/GPA` == "TBC", NA, `UG WAM/GPA`)) %>%
  mutate(`UG WAM/GPA` = ifelse(`UG WAM/GPA` == "TBA", NA, `UG WAM/GPA`)) %>%
  mutate(`UG WAM/GPA` = ifelse(`UG WAM/GPA` == "NULL", NA, `UG WAM/GPA`)) %>%
  mutate(`UG WAM/GPA` = as.numeric(`UG WAM/GPA`))

# Get PG WAM if available
applications <- applications %>%
  mutate(WAM = ifelse(!is.na(`PG WAM/GPA`), `PG WAM/GPA`, `UG WAM/GPA`))

# Plot WAMs  
ggplot(applications, aes(x=WAM)) + 
  geom_histogram() +
  xlim(c(50,100))

# Pull out top WAMs
applications %>% 
  filter(`UG WAM/GPA` > 70) %>%
  select(Surname, `Given Name`) %>%
  print(n=50)

# Examine demographics
applications %>% count(`M/F`)
applications %>% 
  mutate(Qualification = str_replace(Qualification, "Bachelor of ", "")) %>%
  mutate(Qualification = str_replace(Qualification, " (Monash)", "")) %>%
  mutate(Qualification = str_replace(Qualification, " (Honours)", "")) %>%
  mutate(Qualification = str_replace(Qualification, " (Computer)", "")) %>%
  mutate(Qualification = str_replace(Qualification, " (Mechanical Engineering)", "")) %>%
  mutate(Qualification = str_replace(Qualification, " (Accounting and Finance)", "")) %>%
  mutate(Qualification = str_replace(Qualification, " (Accounting)", "")) %>%
  mutate(Qualification = str_replace(Qualification, " (Computer Science and Engineering)", "")) %>%
  mutate(Qualification = str_replace(Qualification, " (Electrical and Electrics Engineering)", "")) %>%
  mutate(Qualification = str_replace(Qualification, " (Electronics and communication engineering)", "")) %>%
  mutate(Qualification = str_replace(Qualification, " (Mechanical and Automation Engineering)", "")) %>%
  mutate(Qualification = str_replace(Qualification, " in Mechanical Engineering", "")) %>%
  mutate(Qualification = str_replace(Qualification, "BSci", "Science")) %>%
  mutate(Qualification = str_replace(Qualification, " in", "")) %>%
  count(Qualification, sort = TRUE) %>% 
  print(n=100)

# Now examine the SoPs
# Extract text from names
pdfnames <- applications %>% 
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

# Read text of statements
statements <- statements <- purrr::map(pdfnames, function(a){
  cat(a, "\n") 
  file.path("SoP", a) %>% 
    pdf_text() %>% 
    str_trim() %>% 
    toString()})

statement_names <- pdfnames

# Get word counts for each applicant
statements_tbl <- tibble(ID = statement_names, 
                        statement = unlist(statements))
id_terms <- count_words_SoP(statements_tbl)

# Compute top words across all applicants
id_terms %>% 
  group_by(ID) %>%
  top_n(10) %>%
  ungroup() %>%
  group_by(word) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(30) %>%
  ggplot(aes(x=fct_reorder(word, n), y=n)) + 
    geom_col() + 
    coord_flip() +
    xlab("")

# Now compute different word usage
SoP_tf_idf <- tf_idf_words(id_terms)

# Look at samples of students top unique words
SoP_tf_idf_sample <- SoP_tf_idf %>%
  group_by(ID) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()
keep <- SoP_tf_idf_sample %>%
  select(ID) %>%
  distinct() %>%
  sample_n(12) 
SoP_tf_idf_sample %>% filter(ID %in% keep$ID) %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = ID)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ID, ncol = 4, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# Cluster the applicants based on word use

SoP_cors <- SoP_tf_idf %>%
  filter(tf_idf > 0.005) %>%
  pairwise_cor(ID, word, tf_idf, sort = TRUE)

SoP_cors %>%
  ggplot(aes(x=correlation)) + geom_histogram()

# Join WAM on to name in graph
appl_WAM <- applications %>%
  select(Surname, `Given Name`, `Monash ID`, WAM) %>%
  mutate(name = paste0(Surname, ", ", str_to_title(`Given Name`), " ",
         `Monash ID`)) %>%
  select(name, WAM)

appl_WAM <- appl_WAM[-c(45,46,48,49),]

SoP_cors_labels <- SoP_cors %>%
  mutate(item1 = str_replace(item1, " - SoP.pdf", "")) %>%
  select(item1) %>% 
  distinct() %>%
  left_join(appl_WAM, by=c("item1"="name")) %>%
  mutate(label = paste(item1, WAM))

# Make graph
SoP_cors %>%
  mutate(item1 = str_replace(item1, " - SoP.pdf", ""),
         item2 = str_replace(item2, " - SoP.pdf", "")) %>%
  #filter(correlation > .05) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(aes(label = SoP_cors_labels$label), size = 6, color = "lightblue") +
  geom_node_text(aes(label = SoP_cors_labels$label), repel = TRUE) +
  theme_void()

ggplotly()
