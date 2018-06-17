library(tidyverse)
library(lubridate)
library(reshape2)
source("functions.R")


# Defines path
path <- "data/procon/"

# Function to read and return a list with all the activities types
read_activities <- function(path,files) {
  
  # Creates a new list
  return_list <- list()
  
  # Iterates on files array
  for (file in files){
    
    # Removing the extension from the file name and lowering 
    df_name <- tolower(str_replace_all(file,"\\..*$",""))
    
    # Appends the list with the new 
    return_list[[df_name]] <- read_csv(paste0(path,file))
  }
  
  return_list
}

# Reading the files into a single df
read_complaint_files <- function (path,files,col_types,na = c("","NA","NULL")){
  
  #df <- read_csv(files[1],
  #               na = c("","NULL"))
   df <- map_df(files, 
                read_csv, 
                col_types = col_types,
                na = na)
  df
}


manipulates_complaints_files <- function(df, activities){
  df$DataAbertura <- ymd_hms(df$DataAbertura)
  df$DataArquivamento <- ymd_hms(df$DataArquivamento)
  df$CodigoRegiao <- NULL
  df$Regiao <- factor(df$Regiao)
  df$UF <- factor(df$UF)
  df$strRazaoSocial <- factor(df$strRazaoSocial)
  df$Tipo <- factor(df$Tipo)
  df$RazaoSocialRFB <- factor(df$RazaoSocialRFB)
  df$Atendida <- factor(df$Atendida)
  df$CodigoAssunto <- NULL
  df$DescricaoAssunto <- factor(df$DescricaoAssunto)
  df$CodigoProblema <- NULL
  df$DescricaoProblema <- factor(df$DescricaoProblema)
  df$SexoConsumidor <- factor(df$SexoConsumidor)
  df$FaixaEtariaConsumidor <- factor(df$FaixaEtariaConsumidor)
  df$DescCNAEPrincipal <- NULL
  df$strNomeFantasia <- NULL
  df$NomeFantasiaRFB <- NULL
  df$NumeroCNPJ <- NULL
  
  df <- df  %>%
    mutate(CNAEPrincipal = as.character(CNAEPrincipal)) %>%
    mutate(CNAEPrincipal = if_else(str_length(CNAEPrincipal) == 5, paste0("00",CNAEPrincipal),CNAEPrincipal),
           CNAEPrincipal = if_else(str_length(CNAEPrincipal) == 6, paste0("0",CNAEPrincipal), CNAEPrincipal))
  
  df <- df %>%
    mutate(CNAEPrincipal = if_else(CNAEPrincipal == "5822100", "5822101", CNAEPrincipal),
           CNAEPrincipal = if_else(CNAEPrincipal == "8888888", NA_character_, CNAEPrincipal),
           CNAEPrincipal = if_else(CNAEPrincipal == "6201500", "6201501", CNAEPrincipal),
           CNAEPrincipal = if_else(CNAEPrincipal == "8020000", "8020001", CNAEPrincipal),
           CNAEPrincipal = if_else(CNAEPrincipal == "5812300", "5812301", CNAEPrincipal),
           CNAEPrincipal = if_else(CNAEPrincipal == "9412000", "9412001", CNAEPrincipal),
           CNAEPrincipal = if_else(CNAEPrincipal == "4751200", "4751201", CNAEPrincipal),
           CNAEPrincipal = if_else(CNAEPrincipal == "7410201", "7410202", CNAEPrincipal),
           CNAEPrincipal = if_else(CNAEPrincipal == "2013400", "2013401", CNAEPrincipal)
    )
  
  df <- df %>%
    left_join(activities[["subclasses"]],
              by = c("CNAEPrincipal" = "Subclass")) %>%
    left_join(activities[["sections"]],
              by = c("Section" = "Section"),
              suffix = c("",".section")) %>%
    left_join(activities[["divisions"]],
              by = c("Division" = "Division"),
              suffix = c("",".division")) %>%
    left_join(activities[["groups"]],
              by = c("Group" = "Group"),
              suffix = c("",".group")) %>%
    left_join(activities[["classes"]],
              by = c("Class","Class"),
              suffix = c("",".class"))
  
  df <- df %>%
    mutate(Atendida = fct_recode(Atendida,
                                 "Y" = "S",
                                 "N" = "N")
           )
    
  df
}

translate_categories <- function(df,categories){
  
  df <- df %>%
    left_join(categories[["ages"]],
              by = c("FaixaEtariaConsumidor" = "FaixaEtariaConsumidor"),
              suffix = c("","_age")) %>%
    left_join(categories[["issues"]],
              by = c("DescricaoProblema" = "DescricaoProblema"),
              suffix = c("","_issue")) %>%
    left_join(categories[["regions"]],
              by = c("Regiao" = "Regiao"),
              suffix = c("","_region")) %>%
    left_join(categories[["subjects"]],
              by = c("DescricaoAssunto" = "DescricaoAssunto"),
              suffix = c("","_subject"))
  df
}

cleanup_df <- function(df){
  df$Regiao <- NULL
  df$strRazaoSocial <- NULL
  df$DescricaoAssunto <- NULL
  df$DescricaoProblema <- NULL
  df$FaixaEtariaConsumidor <- NULL
  df$Section <- NULL
  df$Division <- NULL
  df$Group <- NULL
  df$Class <- NULL
  
  variables <- c("complaint_year", "complaint_closed_date", "complaint_entered_date", "consumer_state", "type",
                 "company_number", "company_name", "company_activity_code", "complaint_attended", "consumer_sex",
                 "consumer_post_code", "company_subclass", "company_sector", "company_division", "company_group",
                 "company_class", "consumer_age", "complaint_issue_type", "consumer_region", "complaint_subject")
  names(df) <- variables
  df %>% 
    mutate(complaint_issue_type = factor(complaint_issue_type),
           complaint_subject = factor(complaint_subject),
           consumer_region = factor(consumer_region),
           consumer_age = factor(consumer_age),
           company_activity_code = factor(company_activity_code),
           company_sector = factor(company_sector),
           company_division = factor(company_division),
           company_group = factor(company_group),
           company_class = factor(company_class),
           company_subclass = factor(company_subclass)
           ) %>%
    select(complaint_year, complaint_entered_date, complaint_closed_date, complaint_subject, complaint_issue_type, complaint_attended,
           consumer_region, consumer_post_code, consumer_state, consumer_age, consumer_sex,  
           company_number, company_name, company_activity_code, company_sector, company_division, company_group, company_class, company_subclass)
}

sector_summarisation <- function(df){
  df <- df %>%
    mutate(complaint_duration = if_else(complaint_attended == "Y", 
                                        round(difftime(complaint_closed_date,
                                                 complaint_entered_date,
                                                 units = c("days")),digits = 0),
                                        NA_real_)) %>%
    group_by(company_sector) %>%
    summarise(complaints_total = n(),
              complaints_solved = sum(complaint_attended == "Y"),
              complaints_issue_variation = n_distinct(complaint_issue_type),
              complaint_average_duration = round(mean(complaint_duration,
                                            na.rm = T),digits = 2))
}

# Loading Files names
files <- paste0(path,list.files(path,pattern = "recl.*\\.csv"))

# Loading Activities
activities <- read_activities("data/cnae/",
                              list.files("data/cnae/",pattern = "*.csv"))

# Loading Activities
categories <- read_activities("data/procon/",
                              list.files("data/procon/",pattern = "^[^recla.*].*"))

df <- read_complaint_files(path,
                           files = files,
                           col_types = "ccccccccccccccccccccccc")

df <- manipulates_complaints_files(df, activities)

df <- translate_categories(df,categories)

df <- cleanup_df(df)

sector_summary <- sector_summarisation(df)
sector_summary <- sector_summary %>%
  select(company_sector,
         complaints_total,
         complaints_solved) %>%
  melt()

ggplot(sector_summary) + 
  geom_bar(aes(x=company_sector,
               y=value,
               fill=variable),
           stat = "identity",
           position="dodge") + 
  coord_flip()

test <- df %>%
  mutate(complaint_duration = if_else(complaint_attended == "Y", 
                                      round(difftime(complaint_closed_date,
                               y                       complaint_entered_date,
                                                     units = c("days")),digits = 0),
                                      NA_real_)) %>%
  group_by(company_name) %>%
  summarise(complaints_total = n(),
            complaints_solved = sum(complaint_attended == "Y"),
            complaints_issue_variation = n_distinct(complaint_issue_type),
            complaint_average_duration = round(mean(complaint_duration,
                                                    na.rm = T),digits = 2))
  


