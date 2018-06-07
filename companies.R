library(data.table)
library(tidyverse)

companies <- fread("/Users/douglaszickuhr/Downloads/ReceitaFederal_QuadroSocietario.csv")

companies <- companies[,c("nr_cnpj","nm_razao_social")]
companies <- companies %>%
  distinct(nr_cnpj,nm_razao_social)
companies <- tbl_df(companies)
head(companies)
