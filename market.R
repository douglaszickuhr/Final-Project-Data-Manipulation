library(tidyverse)
library(tidyr)
library(reshape2)
library(scales)

df <- read_csv("data/commodity_trade_statistics_data.csv",
               col_types = "cncccnncnc")
df <- df %>%
  filter(comm_code != "TOTAL")

head(df)
problems(df)
glimpse(df)

br <- df %>% 
  filter(comm_code != "TOTAL") %>%
  group_by(year,country_or_area,category,flow) %>%
  summarise(trade_usc = sum(trade_usd)) %>%
  spread(flow,trade_usc,
         fill=0) %>%
  mutate(balance = Export-Import) %>%
  arrange(country_or_area,year,category)

comm_type <- df %>%
  group_by(category) %>%
  summarise()

comm <- df %>%
  filter(comm_code != "TOTAL") %>%
  group_by(category,flow) %>%
  summarise(trade_usd = sum(trade_usd)) %>%
  spread(flow,trade_usd,
         fill = 0) %>%
  rename(export_usd = Export,
         import_usd = Import,
         reexport_usd = `Re-Export`,
         reimport_usd = `Re-Import`)

countries <- df %>%
  distinct(country_or_area)

brics <- df %>%
  distinct(country_or_area) %>%
  filter(country_or_area %in% c("Brazil","Russian Federation","India","China","Rep. of Korea"))

top_exporter <- df %>%
  filter(category == "31_fertilizers") %>%
  filter(flow == "Export") %>%
  group_by(country_or_area) %>%
  summarise(quantity = sum(quantity)) %>%
  arrange(desc(quantity)) %>%
  head(10)
  

fertilizer_import <- df %>%
  filter(flow %in% c("Import")) %>%
  filter(category == "31_fertilizers") %>%
  group_by(category,year,country_or_area,flow) %>%
  semi_join(brics) %>%
  summarise(trade_usd = sum(trade_usd)) %>%
  spread(flow,trade_usd,
         fill = 0) %>%
  rename(import = Import)

export <- df %>%
  filter(flow %in% c("Export")) %>%
  # filter(category %in% c("07_edible_vegetables_and_certain_roots_and_tubers",
  #                        "08_edible_fruit_nuts_peel_of_citrus_fruit_melons",
  #                        "09_coffee_tea_mate_and_spices",
  #                        "10_cereals",
  #                        "11_milling_products_malt_starches_inulin_wheat_glute",
  #                        "12_oil_seed_oleagic_fruits_grain_seed_fruit_etc_ne",
  #                        "13_lac_gums_resins_vegetable_saps_and_extracts_nes",
  #                        "14_vegetable_plaiting_materials_vegetable_products_nes")) %>%
  group_by(year,country_or_area,flow) %>%
  semi_join(brics) %>%
  summarise(trade_usd = sum(trade_usd)) %>%
  spread(flow,trade_usd,
         fill = 0) %>%
  rename(export = Export)
  

brics <- df %>%
  filter(flow == "Export") %>%
  group_by(category,year,country_or_area,flow) %>%
  semi_join(brics) %>%
  summarise(trade_usd = sum(trade_usd)) %>%
  spread(flow,trade_usd,
         fill = 0) %>%
  rename(export_usd = Export,
         import_usd = Import,
         reexport_usd = `Re-Export`,
         reimport_usd = `Re-Import`) %>%
  arrange(year) %>%
  ggplot() + 
  geom_line(aes(x=year,y=import_usd,colour=country_or_area))

df %>%
  filter(comm_code != "TOTAL") %>%
  filter(category == "31_fertilizers") %>%
  filter(flow == "Export") %>%
  group_by(year,country_or_area,flow) %>%
  summarise(trade_usd = sum(quantity)) %>%
  semi_join(top_exporter) %>%
  arrange(year) %>%
  ggplot() + 
  geom_line(aes(x=year,y=trade_usd,colour=country_or_area)) + 
  
  facet_grid(~flow)

fert_exportation <- export %>%
  left_join(fertilizer_import)

ggplot(fert_exportation) + 
  geom_point(aes(x=export,
                 y=import,
                 colour=country_or_area,
                 size=export),
             alpha = 0.5)

ggplot(fert_exportation) + 
  geom_line(aes(x=year,
                y=import,
                colour=country_or_area)) + 
  scale_y_continuous(labels = dollar) + 
  xlab("Years") +
  ylab("Fertilizer Import")


cor(fert_exportation$export,fert_exportation$import)
