###Format CMSY data for toolbox######
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)

cmsy <- read.csv('prep/FIS/catch_model_bmsy/cmsy_bbmsy.csv') %>%
  mutate(prior = 'constrained') %>%
  filter(!is.na(bbmsy_mean))

comsir <- read.csv('prep/FIS/catch_model_bmsy/comsir_bbmsy.csv') %>%
  mutate(prior = 'NA') %>%
  filter(!is.na(bbmsy_mean))

new_b_bmsy <- function(b_bmsy=constrained, method = "comsir"){
  b_bmsy <- b_bmsy %>%
    dplyr::select(stock_id, year, bbmsy_mean, prior, model) %>%
    arrange(stock_id, year) %>%
    group_by(stock_id) %>%
    mutate(mean_5year = rollmean(bbmsy_mean, 5, align="right", fill=NA))
  write.csv(b_bmsy, sprintf('prep/FIS/meanbmsy/%s_b_bmsy_%s_mean5yrs.csv', method, unique(b_bmsy$prior)), row.names=FALSE)
}

new_b_bmsy(cmsy, method="cmsy")
new_b_bmsy(comsir, method="comsir")

###### Final formatting

cmsy <- read.csv('circle2016/prep/FIS/meanbmsy/cmsy_b_bmsy_constrained_mean5yrs.csv') %>%
  dplyr::select(stock_id, year, cmsy_bbmsy=mean_5year)

comsir <- read.csv('prep/FIS/meanbmsy/comsir_b_bmsy_NA_mean5yrs.csv') %>%
  dplyr::select(stock_id, year, comsir_bbmsy=mean_5year)

## Mean catch data created in "meanCatch.R"
mean_catch <- read.csv("prep/FIS/meancatch/fis_mean_catch_arc2016.csv") %>%
  mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
  mutate(taxon_key = str_sub(stock_id_taxonkey, -6, -1)) %>%
  mutate(stock_id = substr(stock_id_taxonkey, 1, nchar(stock_id_taxonkey)-7))

## combine data
setdiff(cmsy$stock_id, mean_catch$stock_id)
setdiff(mean_catch$stock_id, cmsy$stock_id)
intersect(mean_catch$stock_id, cmsy$stock_id) #946

###CMSY join with mean catch
data <- mean_catch %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  left_join(cmsy, by=c("stock_id", "year")) %>%
  ungroup()%>%
  mutate(bbmsy = cmsy_bbmsy) %>%
  dplyr::select(rgn_id, stock_id, taxon_key, year, bbmsy, mean_catch) %>%
  filter(year >= 2001) %>%
  unique()%>%
  dplyr::select(rgn_id, stock_id, year, bbmsy) %>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(data, file='circle2016/prep/FIS/fis_cmsy_bbmsy_noRAM.csv', row.names=FALSE)

###COMSIR join with mean catch
comsir_mc <- mean_catch %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  left_join(comsir, by=c("stock_id", "year")) %>%
  ungroup()%>%
  mutate(bbmsy = comsir_bbmsy) %>%
  dplyr::select(rgn_id, stock_id, taxon_key, year, bbmsy, mean_catch) %>%
  filter(year >= 2001) %>%
  unique()%>%
  dplyr::select(rgn_id, stock_id, year, bbmsy) %>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(comsir_mc, file='prep/FIS/fis_comsir_bbmsy_noRAM.csv', row.names=FALSE)

###Explore for high BBMSY###

high_bmsy <- filter(data, bbmsy<0.5) %>%
  group_by(stock_id) %>%
  mutate(regions_high = length(bbmsy)) %>%
  ungroup() %>%
  filter(regions_high > 1) %>%  #ignore stocks with high bbmsy values only found in one ohi region
  filter(stock_id == "Scomberomorus_cavalla-31") %>%
  dplyr::select(rgn_id, stock_id, year, regions_high, bbmsy) %>%
  mutate(high_bmsy = "yes")
