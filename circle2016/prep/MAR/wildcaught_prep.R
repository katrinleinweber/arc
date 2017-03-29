###############################################
## Proportion FIS/(MAR+FIS) catch to calculate
## FP
## MRF: 9/12/2016
###############################################
library(dplyr)
library(tidyr)

source('src/R/common.R')

mar <- read.csv('layers/mar_harvest_tonnes_arc2016.csv')
mar <- mar %>%
  group_by(rgn_id, year) %>%
  summarize(mar_t = sum(tonnes, na.rm=TRUE)) %>%
  dplyr::select(rgn_id, year, mar_t) %>%
  ungroup()

fis <- read.csv("prep/FIS/FP_fis_catch.csv") %>%
  dplyr::select(rgn_id, year, fis_t = fis_catch)

## function to combine different years of data:
scenarioYear=2012; marYear=2009; fisYear=2006
extract <- function(scenarioYear, marYear, fisYear){
  fis_tmp <- fis[fis$year == fisYear, ] %>%
    dplyr::select(rgn_id, fis_t)

  mar_tmp <- mar[mar$year == marYear, ] %>%
    dplyr::select(rgn_id, mar_t)

  tmp <- merge(fis_tmp, mar_tmp, by='rgn_id', all=TRUE)

  tmp <- tmp %>%
    mutate(fis_t = ifelse(is.na(fis_t), 0, fis_t)) %>%
    mutate(mar_t = ifelse(is.na(mar_t), 0, mar_t)) %>%
    mutate(w_fis = fis_t/(fis_t + mar_t)) %>%
    dplyr::select(rgn_id, w_fis)

  #hist(tmp$w_fis)

  write.csv(tmp, sprintf('prep/FIS/wildcaught_weight_v%s.csv', scenarioYear), row.names=FALSE)
}

extract(scenarioYear=2016, marYear=2013, fisYear=2010)
extract(scenarioYear=2015, marYear=2012, fisYear=2009)
extract(scenarioYear=2014, marYear=2011, fisYear=2008)
extract(scenarioYear=2013, marYear=2010, fisYear=2007)
extract(scenarioYear=2012, marYear=2009, fisYear=2006)
