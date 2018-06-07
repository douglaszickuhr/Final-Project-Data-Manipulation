library(tidyverse)
source("functions.R")

path <- "data/procon/"
google_api_json <- "data/googleapi/credential.json"

# Recoding Regions
translate_regions <- function(df){
  regions <- df %>%
    distinct(Regiao) %>%
    arrange(Regiao)
  
  regions$Region <- translate(regions$Regiao,
                              source = "pt-br",
                              target = "en",
                              json_auth_file = google_api_json)
  
  write_file_with_path(regions,path,"Regions.csv")
  regions
}


translate_subjects <- function(df){
  subjects <- df %>%
    distinct(DescricaoAssunto) %>%
    arrange(DescricaoAssunto)
  
  subjects$Subject <- translate(as.character(subjects$DescricaoAssunto),
                                source = "pt-br",
                                target = "en",
                                json_auth_file = google_api_json)
  
  write_file_with_path(subjects,path,"Subjects.csv") 
  subjects
}


translate_issues <- function(df){
  issues <- df %>%
    distinct(DescricaoProblema) %>%
    arrange(DescricaoProblema)
  
  issues$Issues <- translate(as.character(issues$DescricaoProblema),
                             source = "pt-br",
                             target = "en",
                             json_auth_file = google_api_json)
  write_file_with_path(issues,path,"Issues.csv") 
  issues
}


translate_age <- function(df){
  age <- df %>%
    distinct(FaixaEtariaConsumidor)
  
  age$Age <- translate(as.character(age$FaixaEtariaConsumidor),
                       source = "pt-br",
                       target = "en",
                       json_auth_file = google_api_json)
  write_file_with_path(age,path,"Ages.csv") 
  age
}

translate_column <- function(df, 
                             column,
                             filename = ""){
  require("dplyr")
  x <- df %>%
    distinct_(column)
  
  x$translated <- translate(as.character(x[[column]]),
                            source = "pt-br",
                            target = "en",
                            json_auth_file = google_api_json)
  if (missing(filename)){
    filename <- parse(column,".csv")
  }
  write_file_with_path(x,path,filename)
  x
}

fields <- c("Regiao","DescricaoAssunto")

translate_column(df, "Regiao", "Regions.csv")
translate_column(df, "DescricaoAssunto", "Subjects.csv")
translate_column(df, "DescricaoProblema", "Issues.csv" )
translate_column(df, "FaixaEtariaConsumidor", "Ages.csv")

