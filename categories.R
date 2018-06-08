library(tidyverse)
source("functions.R")

path <- "data/procon/"
google_api_json <- "data/googleapi/credential.json"

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

