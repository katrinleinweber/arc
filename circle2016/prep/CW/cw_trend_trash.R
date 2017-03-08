## load necessary packages and set up path directories
library(dplyr)
library(tidyr)
# library(devtools)
# devtools::install_github("ohi-science/ohicore@dev")
library(ohicore)

# setwd('globalprep/cw_trend_trash/v2016') #comment out when knitting

source('~/github/ohiprep/src/R/common.R')
## get and format data:
trash <- read.csv(file.path('/home/shares/ohi/git-annex/globalprep/_raw_data/MarinePlastics_Jambeck/d2016/1260352_SupportingFile_Suppl_modified.csv')) %>%
  select(Country, mpw_2010 = Mismanaged_plastic_waste_in_2010, mpw_2025=Mismanaged_plastic_waste_in_2025) %>%
  mutate(mpw_2010 = as.character(mpw_2010)) %>%
  mutate(mpw_2010 = gsub(",", "", mpw_2010)) %>%
  mutate(mpw_2010 = as.numeric(mpw_2010)) %>%
  mutate(mpw_2025 = as.character(mpw_2025)) %>%
  mutate(mpw_2025 = gsub(",", "", mpw_2025)) %>%
  mutate(mpw_2025 = as.numeric(mpw_2025)) %>%
  gather("mpw", "value", starts_with("mpw"))

trash <- trash %>%
  filter(!(Country %in% c("Channel Islands")))

antilles <- data.frame(Country = "Netherlands Antilles", country2 = c("Bonaire", "Sint Eustatius", "Saba"))%>% # already included: Curacao, Sint Maarten)
  mutate(country2 = as.character(country2)) %>%
  mutate(Country = as.character(Country))

trash_country_mod <- trash %>%
  left_join(antilles, by="Country") %>%
  mutate(country2 = ifelse(is.na(country2), Country, country2)) %>%
  select(Country=country2, mpw, value)

trash_rgn <- name_2_rgn(df_in = trash_country_mod,
                        fld_name = 'Country',
                        flds_unique = 'mpw')


### For duplicate regions, weight by region area
weights <- data.frame(Country = c("Puerto Rico", "USVI",
                                  "Northern Mariana Islands", "Guam",
                                  "China", "Hong Kong", "Macao",
                                  "Guadeloupe", "Martinique"),
                      rgn_name =c("Puerto Rico and Virgin Islands of the United States", "Puerto Rico and Virgin Islands of the United States",
                                  "Northern Mariana Islands and Guam", "Northern Mariana Islands and Guam",
                                  "China", "China", "China",
                                  "Guadeloupe and Martinique", "Guadeloupe and Martinique"),
                      weight = c(3515, 134, 179, 210, 3705000, 426, 11, 629, 436))

trash_rgn <- trash_rgn %>%
  left_join(weights, by=c("Country", "rgn_name")) %>%
  mutate(weight = ifelse(is.na(weight), 1, weight)) %>%
  group_by(rgn_id, rgn_name, mpw) %>%
  summarize(value = weighted.mean(value, weight)) %>%
  data.frame()
