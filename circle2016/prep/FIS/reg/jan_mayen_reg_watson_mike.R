# using the Reg Watson data

# set the mazu data_edit share based on operating system
dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

library(sf) #install.packages("sf")
library(dplyr)
library(seaaroundus)

###LOAD DATA#####

cells_df<- read.csv("circle2016/prep/FIS/SAUP_rgns/final_saup_ohi_fao.csv")
df <- data.frame()  #create an empty dataframe to start then you will add data from each year

for(yr in 1950:2014){

  print(yr)
  new_catch_data <- readRDS(paste0(file.path(dir_M,'marine_threats/impact_acceleration/stressors/comm_fish/int/catch_data_'),yr,'.rds'))%>%
    filter(Seq %in% cells_df$CellID)            #this removes all cells not in your region, a significant space saver

  df <- rbind(new_catch_data,df)
}

cells_df<-rename(cells_df, Seq=CellID)
test<- left_join(df, cells_df, by = "Seq") # this joins by cell id for Arctic regions and then drops the rest.

##Add catch together
test2<- test %>%
  rowwise() %>%
  mutate(catch = sum(LSF_CR, SSF_CR, IUU_CR, Discards_CR)*OceanArea)

##remove some unneeded data to help memory
test2<- test2 %>% select(-Lat, -Lon, -CNumber, -CountryName, -bycatch, -FunctionalGroupDescription, -fish_type)
test2<- test2 %>% select(-Gear, -GearName)
##combine multiple years
mess<- test2
mess[c("rgn_id")][is.na(mess[c("rgn_id")])] <- 11#converts rgn_id that are NA to 11 to ensure that catches don't end up as NA
mess[c("area")][is.na(mess[c("area")])] <- 1 #converts areas from NA to 1 so catches are complete

df2 <- data.frame()


for (i in 1950:2014){

  print(i)

  #1. subset the allocation data to year i
  data_yearly <- subset(mess, Year==i)




  all_data <- data_yearly%>%
    #mutate(catch_prop = catch_sum * area,
           #year = i)%>%
    group_by(rgn_id,fao_id, TaxonName, CommonName, Taxonkey)%>%
    summarise(total_catch = sum(catch))%>%
    ungroup()%>%
    mutate(year     = i,
           stock_id = gsub(" ", "_", paste(TaxonName, fao_id, sep='-'), fixed=TRUE))%>%
    rename(fao_rgn  = fao_id,
           tons     = total_catch,
           TaxonKey = Taxonkey,
           Scientific_Name = TaxonName,
           Common_Name = CommonName)



  df2 = rbind(df2, all_data)

}

write.csv(df2, "circle2016/prep/FIS/reg/reg_rgns/spatial_catch_reg_new.csv", row.names=FALSE)


##############Prep for BBMSY##################

# add the taxon_resilence data to catch for b/bmsy calculations
taxon_res = read.csv("circle2016/prep/FIS/SAUP_rgns/taxon_resilience_lookup2.csv", stringsAsFactors = FALSE) %>%
  mutate(common = ifelse(common %in% "Silver croaker", paste(common, sciname, sep=" "), common)) %>%
  dplyr::select(Common_Name=common, Resilience)

#Filter out all stocks that don’t meet our conditions:
#Keep all stocks that have at least 1000 tons mean annual harvest
#Keep all stocks with time series of 20 years or more

#set variables to filter by
min_yrs = 20
min_tons = 1000

#read in catch data created above
df <- read.csv("circle2016/prep/FIS/reg/reg_rgns/spatial_catch_reg_new.csv",stringsAsFactors=F)

#create dataset ready to run through catch only models

stks <- df%>%
  filter(TaxonKey >= 600000,               #remove all records of catch reported at higher taxonomic levels than species
         tons     > 0)%>%                  #remove records of 0 catch
  dplyr::select(-rgn_id)%>%                       #remove rgn_id since we aggregate stocks to the FAO level
  group_by(stock_id,year,fao_rgn,Scientific_Name,Common_Name,TaxonKey)%>%
  summarise(tons = sum(tons))%>%           #calculate total tons per stock
  ungroup()%>%
  group_by(stock_id)%>%
  mutate(nyrs = n(),                       #get the total number of years the stock has records for
         avg_ann_catch = mean(tons))%>%    #calculate the mean catch over all catch years
  ungroup()%>%
  filter(avg_ann_catch >= min_tons,        #keep only those stocks that meet our conditions
         nyrs >= min_yrs)%>%
  left_join(taxon_res)%>%                  #add resilience information
  dplyr::select(year,Scientific_Name,Common_Name,fao_rgn,stock_id,TaxonKey,Resilience,tons)
write.csv(stks, "circle2016/prep/FIS/reg/reg_rgns/spatial_catch_pre_bbmsy_reg.csv", row.names=FALSE)

################# Load Catch Data###########

catch<- read.csv('circle2016/prep/FIS/reg/reg_rgns/spatial_catch_pre_bbmsy_reg.csv')%>%
  rename(common = Common_Name)

fis_dir<- 'circle2016/prep/FIS'
####Catch MSY#####

cmsy_fits <- plyr::dlply(catch, c("stock_id", "common"), function(x) {

  #make sure the data is ordered from 1950 to 2014
  x <- arrange(x,year)
  out <- cmsy(ct = x$tons, yr = x$year,  start_r = resilience(x$Resilience[1]),
              reps = 2e4)
  out$year <- x$year
  out
}, .parallel = TRUE)
saveRDS(cmsy_fits, file = file.path(fis_dir,"reg/catch_model_bmsy_reg/cmsy-fits.rds"))
fake_data <- data.frame(bbmsy_q2.5 = NA, bbmsy_q25 = NA, bbmsy_q50 = NA,
                        bbmsy_q75 = NA, bbmsy_q97.5 = NA)

cmsy_bbmsy <- plyr::ldply(cmsy_fits, function(x) {
  bbmsy_cmsy <- x$biomass[, -1] / x$bmsy
  bbmsy_out <- tryCatch({
    bbmsy_out <- summarize_bbmsy(bbmsy_cmsy)
    bbmsy_out$year <- x$year
    bbmsy_out}, error = function(e) fake_data)
})
cmsy_bbmsy$model <- "CMSY"
write.csv(cmsy_bbmsy, "circle2016/prep/FIS/reg/catch_model_bmsy_reg/cmsy_bbmsy_reg.csv", row.names=FALSE)

##############################################
## Preparing mean catch data for ohi-arc
## Arc OHI
###############################################
library(dplyr)
library(tidyr)
source('~/github/ohiprep/src/R/common.R')
catch <- catch <- read.csv('circle2016/prep/FIS/reg/reg_rgns/spatial_catch_reg_new.csv') %>%
  rename(common = Common_Name, fao_id = fao_rgn, species=Scientific_Name)
summary(catch)

#Filter out non-FAO/OHI region cells and banana
catch <- catch %>%
  filter(!is.na(rgn_id)) %>%
  filter(!is.na(fao_id)) %>%
  filter(rgn_id<10)


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


write.csv(mean_catch_toolbox, "circle2016/prep/FIS/reg/meancatchreg/mean_catch_reg.csv", row.names=FALSE)

total_catch_FP <- mean_catch %>%
  group_by(rgn_id, year) %>%
  summarize(fis_catch = sum(catch)) %>%
  dplyr::select(rgn_id, year, fis_catch) %>%
  filter(year >= 2005) # filter to include only the relevant analysis years

write.csv(total_catch_FP, "circle2016/prep/FIS/reg/FP_fis_catch_reg.csv", row.names=FALSE)

###Format CMSY data for toolbox######
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)

cmsy <- read.csv('circle2016/prep/FIS/reg/catch_model_bmsy_reg/cmsy_bbmsy_reg.csv') %>%
  mutate(prior = 'constrained') %>%
  filter(!is.na(bbmsy_mean))

#comsir <- read.csv('prep/FIS/catch_model_bmsy/comsir_bbmsy.csv') %>%
#mutate(prior = 'NA') %>%
#filter(!is.na(bbmsy_mean))

new_b_bmsy <- function(b_bmsy=constrained, method = "cmsy"){
  b_bmsy <- b_bmsy %>%
    dplyr::select(stock_id, year, bbmsy_mean, prior, model) %>%
    arrange(stock_id, year) %>%
    group_by(stock_id) %>%
    mutate(mean_5year = rollmean(bbmsy_mean, 5, align="right", fill=NA))
  write.csv(b_bmsy, sprintf('circle2016/prep/FIS/reg/meanbmsy/%s_b_bmsy_%s_mean5yrs_reg.csv', method, unique(b_bmsy$prior)), row.names=FALSE)
}

new_b_bmsy(cmsy, method="cmsy")
#new_b_bmsy(comsir, method="comsir")

###### Final formatting

cmsy <- read.csv('circle2016/prep/FIS/reg/meanbmsy/cmsy_b_bmsy_constrained_mean5yrs_reg.csv') %>%
  dplyr::select(stock_id, year, cmsy_bbmsy=mean_5year)
ram<- read.csv('circle2016/prep/FIS/reg/ram_bmsy.csv')

#comsir <- read.csv('prep/FIS/meanbmsy/comsir_b_bmsy_NA_mean5yrs.csv') %>%
#dplyr::select(stock_id, year, comsir_bbmsy=mean_5year)

## Mean catch data created in "meanCatch.R"
mean_catch <- read.csv("circle2016/prep/FIS/reg/meancatchreg/mean_catch_reg.csv") %>%
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

write.csv(data, file='circle2016/prep/FIS/reg/fis_cmsy_bbmsy_noRAM_reg.csv', row.names=FALSE)

######ADD RAM DATA############
setdiff(ram$stock_id, mean_catch$stock_id)
setdiff(mean_catch$stock_id, ram$stock_id)
intersect(ram$stock_id, mean_catch$stock_id) #256 stocks with RAM-B/Bmsy data (although RAM is matched by fao and rgn ids)

data <- mean_catch %>%
  left_join(ram, by=c('stock_id', "year")) %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  summarize(ram_bmsy = mean(ram_bmsy, na.rm=TRUE),
            gapfilled = ifelse(all(is.na(gapfilled)), NA, max(gapfilled, na.rm=TRUE))) %>%
  left_join(cmsy, by=c("stock_id", "year")) %>%
  ungroup()

## select best data and indicate gapfilling
data <- data %>%
  mutate(bmsy_data_source = ifelse(!is.na(ram_bmsy), "RAM", NA)) %>%
  mutate(bmsy_data_source = ifelse(is.na(bmsy_data_source) & !is.na(cmsy_bbmsy), "CMSY", bmsy_data_source)) %>%
  mutate(bbmsy = ifelse(is.na(ram_bmsy), cmsy_bbmsy, ram_bmsy)) %>%
  select(rgn_id, stock_id, taxon_key, year, bbmsy, bmsy_data_source, RAM_gapfilled=gapfilled, mean_catch) %>%
  filter(year >= 2001) %>%
  unique()

bbmsy <- data %>%
  select(rgn_id, stock_id, year, bbmsy) %>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(bbmsy, 'circle2016/prep/FIS/reg/fis_cmsy_bbmsy_RAM_reg.csv')
###Explore for high BBMSY###

high_bmsy <- filter(data, bbmsy<0.5) %>%
  group_by(stock_id) %>%
  mutate(regions_high = length(bbmsy)) %>%
  ungroup() %>%
  filter(regions_high > 1) %>%  #ignore stocks with high bbmsy values only found in one ohi region
  filter(stock_id == "Scomberomorus_cavalla-31") %>%
  dplyr::select(rgn_id, stock_id, year, regions_high, bbmsy) %>%
  mutate(high_bmsy = "yes")


#### TESTING OUT SCCORES########
FIS = function(layers, status_year){

  #catch data
  c<- read.csv('circle2016/prep/FIS/reg/meancatchreg/mean_catch_reg.csv') %>%
    dplyr::select(
      rgn_id,
      stock_id_taxonkey,
      year,
      catch          = mean_catch)
  # b_bmsy data
  b<- read.csv('circle2016/prep/FIS/reg/fis_cmsy_bbmsy_RAM_reg.csv') %>%
    dplyr::select(
      rgn_id,
      stock_id,
      year,
      bmsy           = bbmsy)

  #comsir data
  #f = SelectLayersData(layers, layer='fis_comsir_bmsy_arc2016', narrow = TRUE) %>%
  #dplyr::select(
  #rgn_id         = id_num,
  #stock_id      = category,
  #year,
  #bmsy           = val_num)

  # The following stocks are fished in multiple regions and have high b/bmsy values
  # Due to the underfishing penalty, this actually penalizes the regions that have the highest
  # proportion of catch of these stocks.  The following corrects this problem:
  #filter(b, stock_id %in% c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47'))

 high_bmsy <- c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47', 'Sardinella_aurita-34', 'Scomberomorus_cavalla-31')

  b <- b %>%
    mutate(bmsy = ifelse(stock_id %in% high_bmsy, 1, bmsy))


  # separate out the stock_id and taxonkey:
  c <- c %>%
    mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
    mutate(taxon_key = stringr::str_sub(stock_id_taxonkey, -6, -1)) %>%
    mutate(stock_id = substr(stock_id_taxonkey, 1, nchar(stock_id_taxonkey)-7)) %>%
    mutate(catch = as.numeric(catch)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(taxon_key = as.numeric(as.character(taxon_key))) %>%
    dplyr::select(rgn_id, year, stock_id, taxon_key, catch)

  # general formatting:
  b <- b %>%
    mutate(bmsy = as.numeric(bmsy)) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(stock_id = as.character(stock_id))


  # ------------------------------------------------------------------------
  # STEP 1. Calculate scores for Bbmsy values
  # -----------------------------------------------------------------------
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05

  b$score = ifelse(b$bmsy < lowerBuffer, b$bmsy,
                   ifelse (b$bmsy >= lowerBuffer & b$bmsy <= upperBuffer, 1, NA))
  b$score = ifelse(!is.na(b$score), b$score,
                   ifelse(1 - alpha*(b$bmsy - upperBuffer) > beta,
                          1 - alpha*(b$bmsy - upperBuffer),
                          beta))
  b$score = ifelse(b$rgn_id == "1" & b$bmsy >1, 1, b$score)


  # ------------------------------------------------------------------------
  # STEP 1. Merge the b/bmsy data with catch data
  # -----------------------------------------------------------------------
  data_fis <- c %>%
    left_join(b, by=c('rgn_id', 'stock_id', 'year')) %>%
    dplyr::select(rgn_id, stock_id, year, taxon_key, catch, bmsy, score)


  # ------------------------------------------------------------------------
  # STEP 2. Estimate scores for taxa without b/bmsy values
  # Mean score of other fish in the region is the starting point
  # Then a penalty is applied based on the level the taxa are reported at
  # -----------------------------------------------------------------------

  ## this takes the mean score within each region
  data_fis_gf <- data_fis %>%
    group_by(rgn_id, year) %>%
    mutate(Mean_score = mean(score, na.rm=TRUE)) %>%
    ungroup()

  ## this takes the median score across all regions (when no stocks have scores within a region)
  #data_fis_gf <- data_fis_gf %>%
  #group_by(year) %>%
  #mutate(Median_score_global = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
  #ungroup() %>%
  #mutate(Median_score = ifelse(is.na(Median_score), Median_score_global, Median_score)) %>%
  #dplyr::select(-Median_score_global)

  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  status_year=2014
  penaltyTable <- data.frame(TaxonPenaltyCode=1:6,
                             penalty=c(0.1, 0.25, 0.5, 0.8, 0.9, 1))

  data_fis_gf <- data_fis_gf %>%
    mutate(TaxonPenaltyCode = as.numeric(substring(taxon_key, 1, 1))) %>%
    left_join(penaltyTable, by='TaxonPenaltyCode') %>%
    mutate(score_gf = Mean_score * penalty) %>%
    mutate(score_gapfilled = ifelse(is.na(score), "Mean gapfilled", "none")) %>%
    mutate(score = ifelse(is.na(score), score_gf, score))


  gap_fill_data <- data_fis_gf %>%
    mutate(gap_fill = ifelse(is.na(bmsy), "mean", "none")) %>%
    dplyr::select(rgn_id, stock_id, taxon_key, year, catch, score, gap_fill) %>%
    filter(year == status_year)
  write.csv(gap_fill_data, 'circle2016/temp/FIS_summary_gf_reg.csv', row.names=FALSE)
  write.csv(data_fis_gf, 'circle2016/temp/FIS_summary_gf2_reg.csv', row.names=FALSE)

  status_data <- data_fis_gf %>%
    dplyr::select(rgn_id, stock_id, year, catch, score)


  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each region
  # -----------------------------------------------------------------------

  # 4a. To calculate the weight (i.e, the relative catch of each stock per region),
  # the mean catch of taxon i is divided by the
  # sum of mean catch of all species in region/year

  status_data <- status_data %>%
    group_by(year, rgn_id) %>%
    mutate(SumCatch = sum(catch)) %>%
    ungroup() %>%
    mutate(wprop = catch/SumCatch)

  status_data <- status_data %>%
    group_by(rgn_id, year) %>%
    summarize(status = weighted.mean(score, wprop)) %>%
    ungroup()

  # ------------------------------------------------------------------------
  # STEP 5. Get yearly status and trend
  # -----------------------------------------------------------------------

  status <-  status_data %>%
    group_by(rgn_id) %>% #group by rgn_id and add max year in to get status
    filter(year >= max(year, na.rm=T)) %>%
    mutate(
      score     = round(status*100, 1),
      dimension = 'status') %>%
    dplyr::select(region_id=rgn_id, score, dimension)

  status_year<- 2014
  trend_years <- status_year:(status_year-4)
  first_trend_year <- min(trend_years)

  trend <- status_data %>%
    filter(year %in% trend_years) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(status ~ year, data=.),
       adjust_trend = .$status[.$year == first_trend_year]) %>%
    summarize(region_id = rgn_id,
              score = round(coef(mdl)['year']/adjust_trend * 5, 4),
              dimension = 'trend') %>%
    ungroup() %>%
    mutate(score = ifelse(score > 1, 1, score)) %>%
    mutate(score = ifelse(score < (-1), (-1), score))

  # return scores
  scores= full_join(status, trend)%>%
    mutate(goal='FIS')%>%
    data.frame()



write.csv(scores, 'circle2016/prep/FIS/reg/reg_scores.csv')

  return(scores)
}
