
#devtools::install_github("lingtax/Qualtrics")
library(Qualtrics)
library(dplyr)
library(readr)
library(qcoder)

# read data in 

data <- read_qualtrics("Vegan Definition_December 7, 2018_19.53.csv")

metadata <- as.data.frame(names(data))
metadata <- read_csv("metadata.csv")
data <- meta_rename(data, metadata, old, new)

data <- data %>% 
  filter(age >= 18 & !is.na(why_diet)) 
  
data <- select(data, response_id, diet, why_diet)

#devtools::source_url("https://gist.github.com/benmarwick/9266072/raw/csv2txts.R")

#change csv to txt

## save this data to a csv. But first, make sure it's the only thing in the folder you're saving to

#setwd("~/Documents/OneDrive - Deakin University/Maddie's Laptop/vegan qual")

#write_csv(data, "vegan_qual_analysis/maddie_doc/documents.csv") 

#csv2txt("vegan_qual_analysis/maddie_doc")

#qcode()

#create new project

#create_qcoder_project("vegan_qual_analysis")
#import_project_data(project = "vegan_qual_analysis")
#qcode()

# create units file
#units <- NULL
#units_document_map$doc_path <- data$response_id
#units_document_map$unit_id <- 1:1501
#units_document_map <- as.data.frame(units_document_map)

#write_csv(units_document_map, "vegan_qual_analysis/units/unit_document_map.csv")

#units$unit_id <- 1:1501
#units$name <- data$diet
#units <- as.data.frame(units)
 
#write_csv(units, "vegan_qual_analysis/units/units.csv")

#create_qcoder_project("vegan_qual_analysis")
#import_project_data(project = "vegan_qual_analysis")
#qcode()

library(tidyverse)

df <- readRDS("vegan_qual_analysis/data_frames/qcoder_documents_vegan_qual_analysis_2019_01_11_09_45_04.rds")
df <- df[1:999,] #remove the uncoded ones from the bottom

df2 <- readRDS("qcoder_documents_qual_codes_2019_01_15_12_17_41.rds")

df2 <- df2 %>% 
  mutate(document_text = str_remove(document_text, word(document_text))) %>% 
  mutate(document_text = str_trim(document_text))

df <- bind_rows(df, df2)

df_cleaned <- df %>% 
  mutate(dietary_category = word(document_text)) %>% 
  mutate(dietary_category = str_remove(dietary_category, "\\(QCODE\\)")) %>% 
  mutate(cleaned_text = str_remove(document_text, word(document_text))) %>% 
  mutate(health = str_detect(document_text, "#health"),
         animals = str_detect(document_text,  "#animals"),
         environment = str_detect(document_text,  "#environment"),
         norms = str_detect(document_text,  "#norms"),
         ethics = str_detect(document_text,  "#ethics"),
         preference = str_detect(document_text,  "#preference"),
         convenience =str_detect(document_text,  "#convenience"),
         laziness =str_detect(document_text,  "#laziness"),
         culture =str_detect(document_text,  "#culture"),
         habit = str_detect(document_text,  "#habit"),
         religion = str_detect(document_text,  "#religion"),
         lifestyle =str_detect(document_text,  "#lifestyle"),
         cost = str_detect(document_text,  "#cost"),
         difficulty = str_detect(document_text,  "#difficulty"),
         no_reason = str_detect(document_text,  "#no reason")) %>% 
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\(QCODE\\)")) %>% 
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\(/QCODE\\)")) %>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#health\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#animals\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#environment\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#norms\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#ethics\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#preference\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#convenience\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#laziness\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#culture\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#habit\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#religion\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#lifestyle\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#cost\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#difficulty\\}"))%>%
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#no reason\\}")) %>% 
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#culture,#norms\\}")) %>% 
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#habit,#norms\\}")) %>% 
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#convenience,#preference\\}")) %>% 
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#ethics,#animals\\}")) %>% 
  mutate(cleaned_text = str_remove_all(cleaned_text, "\\{#animals,#ethics\\}")) %>% 
  mutate(num_reasons = health + animals + environment + norms + ethics + preference 
         + convenience + laziness + culture + habit + religion + lifestyle + cost + difficulty)

summary <- df_cleaned %>% 
  group_by(dietary_category) %>% 
  summarise(health = sum(health),
            animals = sum(animals),
            laziness = sum(environment),
            norms = sum(norms),
            ethics = sum(ethics),
            preference = sum(preference),
            convenience = sum(convenience),
            laziness = sum(laziness),
            culture = sum(culture),
            habit = sum(habit),
            religion = sum(religion),
            lifestyle = sum(lifestyle),
            cost = sum(cost),
            difficulty = sum(difficulty),
            no_reason = sum(no_reason))

no_themes <- df_cleaned %>% filter(num_reasons == 0)

multi_themes <- df_cleaned %>% filter(num_reasons >= 2) %>% 
  mutate(health = case_when(health == TRUE ~ "health", health == FALSE ~ ""),
         animals = case_when(animals == TRUE ~ "animals", animals == FALSE ~ ""),
         ethics = case_when(ethics == TRUE ~ "ethics", ethics == FALSE ~ ""),
         norms = case_when(norms == TRUE ~ "norms", norms == FALSE ~ ""),
         preference = case_when(preference == TRUE ~ "preference", preference == FALSE ~ ""),
         environment = case_when(environment == TRUE ~ "environment", environment == FALSE ~ ""),
         convenience = case_when(convenience == TRUE ~ "convenience", convenience == FALSE ~ ""),
         laziness = case_when(laziness == TRUE ~ "laziness", laziness == FALSE ~ ""),
         culture = case_when(culture == TRUE ~ "culture", culture == FALSE ~ ""),
         habit = case_when(habit == TRUE ~ "habit", habit == FALSE ~ ""),
         religion = case_when(religion == TRUE ~ "religion", religion == FALSE ~ ""),
         lifestyle = case_when(lifestyle == TRUE ~ "lifestyle", lifestyle == FALSE ~ ""),
         cost = case_when(cost == TRUE ~ "cost", cost == FALSE ~ ""),
         difficulty = case_when(difficulty == TRUE ~ "difficulty", difficulty == FALSE ~ "")
  ) %>% 
  mutate(themes = paste(health, animals, ethics, norms, preference, environment, 
                        convenience, laziness, culture, habit, religion, lifestyle, cost,
                        difficulty)) %>% 
  group_by(themes, dietary_category) %>% 
  summarise(n = n())

