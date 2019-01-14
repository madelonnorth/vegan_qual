library(dplyr)
library(stringr)

D1  <- readRDS("~/Documents/OneDrive - Deakin University/Maddie's Laptop/vegan qual/vegan_qual_analysis/data_frames/qcoder_documents_vegan_qual_analysis.rds")

vegan <- D1 %>% 
  #This uses Regex to look for the word Vegan (pattern) at the start of each section of text (string)
  filter(str_detect(string = document_text, pattern = "^Vegan")) 

omni <- D1 %>% 
  #This uses Regex to look for the word Omnivore (pattern) at the start of each section of text (string)
  filter(str_detect(string = document_text, pattern = "^Omnivore")) %>%
  # Then use str_extract to pull the codes from the text
  mutate(codes = str_extract_all(document_text, "\\(QCODE\\).+\\}"))

vegetarian <- D1 %>% 
  #This uses Regex to look for the word Vegetarian (pattern) at the start of each section of text (string)
  filter(str_detect(string = document_text, pattern = "^Vegetarian")) 
