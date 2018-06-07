library(tidyverse)
library(lubridate)
library(readxl)
source("functions.R")

# Defines local path and activities file
path <- "data/cnae/"
cnae <- "data/cnae/Lista.xlsx"
google_api_json <- "data/googleapi/credential.json"

read_activities_file <- function(file,
                      col_names = c("Section",
                                    "Division",
                                    "Group",
                                    "Class",
                                    "Subclass",
                                    "Description")) {
  # Reading Activities List
  df <- readxl::read_xlsx(
    path = file,
    col_names = col_names,
    trim_ws = T,
    skip = 1
  )
  df
}

activities_wrangling <- function(df){
  df %>%
    filter((
      !str_detect(Section, "conti") &
        !str_detect(Section, "2.2 Estrutu") &
        !str_detect(Section, "Seç") &
        !str_detect(Section, "esolu") &
        !str_detect(Section, "conc")
    ) | (is.na(Section))) %>%
    filter(!str_detect(Description, "Denomi")) %>%
    filter_all(any_vars(!is.na(.))) %>%
    mutate(
      Class = if_else(
        is.na(Section) &
          is.na(Division) & 
          is.na(Group),
        fillNAgaps(Class),
        NA_character_
      ),
      Group = if_else(
        is.na(Section) & 
          is.na(Division),
        fillNAgaps(Group),
        NA_character_
      ),
      Division = if_else(is.na(Section), fillNAgaps(Division), NA_character_),
      Section = fillNAgaps(Section)
    ) %>%
    mutate(
      Division = str_replace_all(Division, "[\\.\\-]", replacement = ""),
      Group = str_replace_all(Group, "[\\.\\-\\/]", replacement = ""),
      Class = str_replace_all(Class, "[\\.\\-\\/]", replacement = ""),
      Subclass = str_replace_all(Subclass, "[\\.\\-\\/]", replacement = "")
    )
}

return_section_from_activities <- function(df){
  df <- df %>%
    filter(is.na(Division) &
             is.na(Group) & is.na(Class) & is.na(Subclass)) %>%
    select(Section, Description)
  print(df)
  
  df$Description <-
    translate(df$Description,
              source = "pt-br",
              target = "en",
              json_auth_file = google_api_json
    )
  write_file_with_path(df,"Sections.csv")
  df
}

return_division_from_activities <- function(df){
  df <- df %>%
    filter(!is.na(Division) &
             is.na(Group) & is.na(Class) & is.na(Subclass)) %>%
    select(Division, Description)
  
  df$Description <-
    translate(df$Description,
              source = "pt-br",
              target = "en",
              json_auth_file = google_api_json)
  write_file_with_path(df, "Divisions.csv")
  df
}

return_group_from_activities <- function(df){
  df <- df %>%
    filter(!is.na(Division) &
             !is.na(Group) & is.na(Class) & is.na(Subclass)) %>%
    select(Group, Description)
  
  
  df$Description <-
    translate(df$Description,
              source = "pt-br",
              target = "en",
              json_auth_file = google_api_json)
  write_file_with_path(df, "Groups.csv")
  df
}

return_class_from_activities <- function(df){
  df <- df %>%
    filter(!is.na(Division) &
             !is.na(Group) & !is.na(Class) & is.na(Subclass)) %>%
    select(Class, Description)
  
  
  df$Description <-
    translate(df$Description,
              source = "pt-br",
              target = "en",
              json_auth_file = google_api_json)
  write_file_with_path(df, "Classes.csv")
  df
}

return_subclass_from_activities <- function(df){
  df <- df %>%
    filter(!is.na(Subclass))
  
  
  df$Description <-
    translate(df$Description,
              source = "pt-br",
              target = "en",
              json_auth_file = google_api_json)
  write_file_with_path(df, "SubClasses.csv")
  df
}


# Read Excel File
activities <- read_activities_file(cnae)

# Data wrangling on activities list
activities <- activities_wrangling(activities)

# Return sections from activities list
sections <- return_section_from_activities(activities)

# Return divisions from activities list
divisions <- return_division_from_activities(activities)

# Return group from activities list
groups <- return_group_from_activities(activities)

# Return class from activities list
classes <- return_class_from_activities(activities)

# Return subclass from activities list
subclasses <- return_subclass_from_activities(activities)