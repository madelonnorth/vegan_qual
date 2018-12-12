
devtools::install_github("lingtax/Qualtrics")
library(Qualtrics)
library(dplyr)
library(readr)

# read data in 

data <- read_qualtrics("~/Documents/OneDrive - Deakin University/Maddie's Laptop/vegan qual/Vegan Definition_December 7, 2018_19.53.csv")

metadata <- as.data.frame(names(data))
write_csv(metadata, "metadata.csv")
metadata <- read_csv("~/Desktop/metadata.csv")
data <- meta_rename(data, metadata, old, new)

data <- data %>% 
  filter(age >= 18 & !is.na(why_diet)) 
  
data <- select(data, response_id, diet, why_diet)

devtools::source_url("https://gist.github.com/benmarwick/9266072/raw/csv2txts.R")

#change csv to txt

## save this data to a csv. But first, make sure it's the only thing in the folder you're saving to

setwd("~/Documents/OneDrive - Deakin University/Maddie's Laptop/vegan qual")

write_csv(data, "vegan_qual_analysis/maddie_doc/documents.csv") 

csv2txt("vegan_qual_analysis/maddie_doc")

qcode()

#create new project

create_qcoder_project("vegan_qual_analysis")
import_project_data(project = "vegan_qual_analysis")
qcode()

# create units file
units <- NULL
units_document_map$doc_path <- data$response_id
units_document_map$unit_id <- 1:1501
units_document_map <- as.data.frame(units_document_map)

write_csv(units_document_map, "vegan_qual_analysis/units/unit_document_map.csv")

units$unit_id <- 1:1501
units$name <- data$diet
units <- as.data.frame(units)
 
write_csv(units, "vegan_qual_analysis/units/units.csv")

create_qcoder_project("vegan_qual_analysis")
import_project_data(project = "vegan_qual_analysis")
qcode()



