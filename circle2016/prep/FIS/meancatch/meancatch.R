##############################################
## Preparing mean catch data for ohi-arc
## Arc OHI
###############################################
library(dplyr)
library(tidyr)
source('~/github/ohiprep/src/R/common.R')
catch <- catch <- read.csv('prep/FIS/SAUP_rgns/spatial_catch_saup_new.csv') %>%
  rename(common = Common_Name, fao_id = fao_rgn, species=Scientific_Name)
summary(catch)

#Filter out non-FAO/OHI region cells and banana
catch <- catch %>%
  filter(!is.na(rgn_id)) %>%
  filter(!is.na(fao_id)) %>%
  filter(rgn_id<9)


catch <- catch %>%
  dplyr::select(year, rgn_id, fao_id, stock_id, TaxonKey, tons) %>%
  group_by(rgn_id, fao_id, TaxonKey, stock_id, year) %>%
  summarize(catch = sum(tons)) %>%
  ungroup()

#---------------------------------------------
# for years with no reported catch, add zero values
# (after first reported catch)
# --------------------------------------------

## these data have no zero catch values, so this is added here:
catch_zeros <- catch %>%
  spread(year, catch) %>%
  data.frame() %>%
  gather("year", "catch", num_range("X", 1950:2014)) %>%
  mutate(year = as.numeric(gsub("X", "", year))) %>%
  mutate(catch = ifelse(is.na(catch), 0, catch))

## this part eliminates the zero catch values prior to the first reported non-zero catch
catch_zeros <- catch_zeros %>%
  group_by(fao_id, TaxonKey, stock_id, rgn_id) %>%
  arrange(year) %>%
  mutate(cum_catch = cumsum(catch)) %>%
  filter(cum_catch > 0) %>%
  dplyr::select(-cum_catch) %>%
  ungroup()

#---------------------------------------------
### Calculate mean catch for ohi regions (using data from 1980 onward)
### These data are used to weight the RAM b/bmys values
# --------------------------------------------

mean_catch <- catch_zeros %>%
  filter(year >= 1980) %>%
  group_by(rgn_id, fao_id, TaxonKey, stock_id) %>%
  mutate(mean_catch=mean(catch, na.rm=TRUE))%>%
  filter(mean_catch != 0)  %>%      ## some stocks have no reported catch for time period
  ungroup()

#---------------------------------------------
# Toolbox formatting and save
# --------------------------------------------
mean_catch_toolbox <- mean_catch %>%
  mutate(stock_id_taxonkey = paste(stock_id, TaxonKey, sep="_")) %>%
  dplyr::select(rgn_id, stock_id_taxonkey, year, mean_catch) %>%
  filter(year>=2001) %>%  # filter to include only analysis years
  data.frame()


write.csv(mean_catch_toolbox, "prep/FIS/meancatch/mean_catch_new.csv", row.names=FALSE)

total_catch_FP <- mean_catch %>%
  group_by(rgn_id, year) %>%
  summarize(fis_catch = sum(catch)) %>%
  dplyr::select(rgn_id, year, fis_catch) %>%
  filter(year >= 2005) # filter to include only the relevant analysis years

write.csv(total_catch_FP, "prep/FIS/FP_fis_catch_new.csv", row.names=FALSE)
