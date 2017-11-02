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
