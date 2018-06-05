library(tidyverse)
library(tidyr)
library(lubridate)
library(reshape2)
library(scales)
library(googleLanguageR)
library(readxl)
library(zoo)

path <- "data/procon/"
gapijson <- "data/googleapi/credential.json"
cnae <- "data/cnae/Lista.xlsx"


# Read files
files <- paste0(path,list.files(path))

# Function to auth on google
authGoogle <- function(json){
  gl_auth(json)
}

# Wrapper function to translate text
translate <- function(text_to_translate,source,target){
  gl_translate(t_string = text_to_translate,
               target = target,
               source = source,
               format = "text",
               model = c("nmt", "base")
  )
}

# Function to translate text
fillNAgaps <- function(x, firstBack=FALSE) {
  ## NA's in a vector or factor are replaced with last non-NA values
  ## If firstBack is TRUE, it will fill in leading NA's with the first
  ## non-NA value. If FALSE, it will not change leading NA's.
  
  # If it's a factor, store the level labels and convert to integer
  lvls <- NULL
  if (is.factor(x)) {
    lvls <- levels(x)
    x    <- as.integer(x)
  }
  
  goodIdx <- !is.na(x)
  
  # These are the non-NA values from x only
  # Add a leading NA or take the first good value, depending on firstBack   
  if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
  else             goodVals <- c(NA,            x[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  x <- goodVals[fillIdx]
  
  # If it was originally a factor, convert it back
  if (!is.null(lvls)) {
    x <- factor(x, levels=seq_along(lvls), labels=lvls)
  }
  
  x
}


# Reading Activities List
Activities_List <- readxl::read_xlsx(path = cnae,
                          col_names = c("Section","Division","Group","Class","Subclass","Description"),
                          trim_ws = T,
                        skip = 1)

# Reading Activities List
Activities_List <- Activities_List %>%
  filter((!str_detect(Section,"conti") &
          !str_detect(Section,"2.2 Estrutu") &
          !str_detect(Section,"Seç") & 
          !str_detect(Section,"esolu") & 
          !str_detect(Section,"conc")) | (is.na(Section))) %>%
  filter_all(any_vars(!is.na(.))) %>%
  mutate(
    Class = if_else(is.na(Section) & is.na(Division) & is.na(Group), fillNAgaps(Class), NA_character_),
    Group = if_else(is.na(Section) & is.na(Division), fillNAgaps(Group), NA_character_),
    Division = if_else(is.na(Section), fillNAgaps(Division), NA_character_),
    Section = fillNAgaps(Section)
  ) %>%
  mutate(
    Division = str_replace_all(Division,"[\\.\\-]",replacement = ""),
    Group = str_replace_all(Group,"[\\.\\-\\/]",replacement = ""),
    Class = str_replace_all(Class,"[\\.\\-\\/]",replacement = ""),
    Subclass = str_replace_all(Subclass,"[\\.\\-\\/]",replacement = "")
  )

Activities_Section <- Activities_List %>%
  filter(is.na(Division) & is.na(Group) & is.na(Class) & is.na(Subclass)) %>%
  select(Section,Description)

Activities_Division <- Activities_List %>%
  filter(!is.na(Division) & is.na(Group) & is.na(Class) & is.na(Subclass)) %>%
  select(Division,Description)

Activities_Group <- Activities_List %>%
  filter(!is.na(Division) & !is.na(Group) & is.na(Class) & is.na(Subclass)) %>% 
  select(Group,Description)

Activities_Class <- Activities_List %>%
  filter(!is.na(Division) & !is.na(Group) & !is.na(Class) & is.na(Subclass)) %>% 
  select(Class,Description)





# Reading the files into a single df
df <- map_df(files, 
             read_csv, 
             col_types = "icccccccicccciccicicccc",
             na = c("","NULL"))
df$DataAbertura <- ymd_hms(df$DataAbertura)
df$DataArquivamento <- ymd_hms(df$DataAbertura)
df$Regiao <- factor(df$Regiao)
df$UF <- factor(df$UF)
df$strRazaoSocial <- factor(df$strRazaoSocial)
df$Tipo <- factor(df$Tipo)
df$RazaoSocialRFB <- factor(df$RazaoSocialRFB)

# Recoding Regions
Regions <- df %>%
  distinct(CodigoRegiao,Regiao) %>%
  arrange(CodigoRegiao)

token <- authGoogle(gapijson)

translate(Regions$text,
          source = "pt-br",
          target = "en")

Regions <- gl_translate(as.character(Regions$Regiao), 
             target = "en", 
             format = c("text"), 
             source = "pt",
             model = c("nmt", "base"))


Subject <- df %>%
  distinct(CodigoAssunto,DescricaoAssunto) %>%
  arrange(CodigoAssunto)

Subject_Translated <- translate(Subject$DescricaoAssunto,
          source = "pt-br",
          target = "en")


Issues <- df %>%
  distinct(CodigoProblema,DescricaoProblema) %>%
  arrange(CodigoProblema)

Issues_Translated <- translate(Issues$DescricaoProblema,
                               source = "pt-br",
                               target = "en")

Age <- df %>%
  distinct(FaixaEtariaConsumidor)

Activity <- df %>%
  distinct(DescCNAEPrincipal)

Activity_Translated <- translate(Activity$DescCNAEPrincipal,
                                 source = "pt-br",
                                 target = "en")
