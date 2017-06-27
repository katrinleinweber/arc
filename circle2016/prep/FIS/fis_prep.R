####FIS PREP#######

source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(sp)
library(dplyr)
library('rgeos')    ### vector spatial analysis tools
library('maptools') ### an alternate package with good spatial analysis tools
library('spatstat')
library('tiff')
library(readr)
library(data.table)
library(parallel)
library(datalimited)


## Paths for data
path_data               = "/home/shares/ohi/git-annex/globalprep/_raw_data/SAUP/d2016/Data"
file_allocation_data    = "SeaAroundUs/AllocationData.dat"
file_allocation_results = "SeaAroundUs/AllocationResult.dat"
file_taxon              = "SeaAroundUs/taxon.dat"
file_entity             = "FishingEntity.dat"
path_data2               = "/home/shares/ohi/git-annex/globalprep/_raw_data/SAUP/d2017/annual_data/"

spatial_dir<- 'circle2016/prep/spatial'
fis_dir<- 'prep/FIS'
ohi_rgns <- raster(file.path(spatial_dir, "sp_mol_raster.tif"))

###LOAD DATA#####
# load the Allocation info using fread, and define column names
dt_data           <- fread(file.path(path_data,file_allocation_data), sep=";", header = FALSE)
colnames(dt_data) <- c("UniversalDataID","DataLayerID","FishingEntityID", "Year", "TaxonKey",
                       "InputTypeID", "sector_type_name", "catch_type_name",
                       "reporting_status_name")
for(yr in 1950:2013){
dt_data_new2<- readRDS(paste0(file.path(path_data2), "saup_data_",yr,".rds"))
}
dt_data_new3<- rbind(dt_data_new, dt_data_new2)
#load the Results data (largest file, usually takes up to 10 minutes to read!)
dt_results           <- fread(file.path(path_data,file_allocation_results), sep=";", header = FALSE)
colnames(dt_results) <- c("UniversalDataID","CellID","AllocatedCatch")
dt_results2<- select(dt_data_new3, cell_id, catch_sum)
# setkey(dt_results,UniversalDataID) # not necessary the data seems to be already ordered with the keys (univ and Cell IDs)


#load the Taxon data
dt_taxon           <- fread(file.path(path_data,file_taxon), sep=";", header = FALSE)
colnames(dt_taxon) <- c("TaxonKey","Scientific_Name","Common_Name","FunctionalGroupDescription")
setkey(dt_taxon,TaxonKey)
dt_taxon2<- select(dt_data_new3, taxon_key, taxon_scientific_name, taxon_common_name, functional_group_name)
colnames(dt_taxon2) <- c("TaxonKey","Scientific_Name","Common_Name","FunctionalGroupDescription")



#load the fishing entity data
dt_entity           <- fread(file.path(path_data,file_entity), sep = ";", header=FALSE)
colnames(dt_entity) <- c("FishingEntityID","Name")
setkey(dt_entity,FishingEntityID)
dt_entity2<- select(dt_data_new3, fishing_entity_name, fishing_entity_id)
colnames(dt_entity2) <- c("FishingEntityID","Name")


##There are a lot of cells that slighlty overlap the OHI regions shapefile, leaving them with a proportional area less than 1.
#This would cause us to lose catch when assigning catch to cells.
#To fix this, we define a vector of cellids that have a proportionArea <1 and are NOT duplicated
#(i.e. the other portion of the area missing is not accounted for) and assign a proportionArea of 1 to these cells.

fis_dir<- save_loc<- 'circle2016/prep/FIS'
cells_raw <- read.csv(file.path(fis_dir, "SAUP_rgns/saup_rasters_to_ohi_rgns.csv"))%>%
  rename(CellID=saup_cell_id) %>%
  group_by(CellID) %>%
  mutate(total_area = sum(proportionArea))

### list of cells with areas > 1 (indicates something strange is going on)
bad_ones <- filter(cells_raw, total_area>1) %>%
  arrange(CellID)

data.frame(bad_ones)
## cells should have a total area of <=1, what causes some cells to have more...and is this a large problem
## This seems to happen because of polygon overlap.
## scenario 1: FAO region 262 overlaps other FAO region polygons by a small amount.  In this case a small proportion of the catch within a cell will
## be assigned to two regions.  This will be a small error. (No correction)
## scenario 2: In many cases, the total area is very close to one which may reflect rounding error and is not significant. (Usually no correction)
## scenario 3: In some cases, the regions are small islands where there doesn't appear to be a hole so both the land and underlying eez are counted. ##            This can also occur along any eez/land boundary...but it looks like it mainly happens for islands. If the
##            overlap is for land/eez within the same region, this will be corrected.

cells <- read.csv(file.path(fis_dir, "SAUP_rgns/saup_rasters_to_ohi_rgns.csv")) %>%
  rename(CellID=saup_cell_id) %>%
  group_by(CellID, rgn_id) %>%   # groups land and eez data (that way the cell catch is fully applied to the region...rather than cutting the portion that overlaps land)
  dplyr::summarise(area = sum(proportionArea)) %>%
  mutate(area = ifelse(area > 1, 1, area))%>%  ## this corrects when there is land/eez overlap within the same region resulting in cell area >1 (scenario 3 above)
  ungroup()

# get the list of cell ids that are duplicated, and use this list of values to adjust the area to equal 1 ONLY for those cells that are not duplicated


## Id duplicated cells:
dup <- cells$CellID[duplicated(cells$CellID)]

## these are the cells that were cut off prematurely due to edge effects, etc.
tmp <- filter(cells, !(CellID %in% dup) & area < 1)
head(tmp)


#read in the dataset matching each cell to an FAO region (need both OHI and FAO region for analysis)
fao_cells <- read.csv(file.path(fis_dir, "SAUP_rgns/saup_rasters_to_ohi_fao_rgns.csv")) %>%
  rename(CellID=saup_cell_id)

cells_df <- cells %>%
  mutate(area = ifelse(CellID %in% dup, area, 1))%>%  # if the cell doesn't cover >1 region, then change cell areas to one to capture entire cell's catch (these are <1 area due to edge effects)
  left_join(fao_cells)

write.csv(cells_df, "final_saup_ohi_fao.csv", row.names=FALSE)
cells_df<- read.csv("prep/FIS/SAUP_rgns/final_saup_ohi_fao.csv")
test<- inner_join(dt_data_new3, cells_df, by = "cell_id") # this joins by cell id for Arctic regions and then drops the rest.

########### Aggregate Catch################
df <- data.frame()


for (i in 1950:2014){

  print(i)

  #1. subset the allocation data to year i
  #data_yearly <- test[year==i,]

  #2. Now we want it ordered by UniversalDataID
  #setkey(data_yearly,UniversalDataID)

  #3. Get unique UniversalDataID

  #udi <- unique(data_yearly$UniversalDataID)

  #4. Subset results

  #results_sub <- dt_results[UniversalDataID %in% udi]

  #setkey(results_sub,UniversalDataID) #sort by UDI


  #5. Join allocation, taxon, entity, resilience data to results to create final catch dataset and removing all catch reported at non-species level


  all_data <- test%>%
    mutate(catch_prop = catch_sum * area,
           year = i)%>%
    group_by(rgn_id,fao_id, taxon_scientific_name, taxon_common_name, taxon_key)%>%
    summarise(catch = sum(catch_prop))%>%
    ungroup()%>%
    mutate(year     = i,
           stock_id = gsub(" ", "_", paste(taxon_scientific_name, fao_id, sep='-'), fixed=TRUE))%>%
    rename(fao_rgn  = fao_id,
           tons     = catch,
           TaxonKey = taxon_key,
           Scientific_Name = taxon_scientific_name,
           Common_Name = taxon_common_name)



  df = rbind(df,all_data)

}
write.csv(df, "prep/FIS/SAUP_rgns/spatial_catch_saup_new.csv", row.names=FALSE)

##############Prep for BBMSY##################

# add the taxon_resilence data to catch for b/bmsy calculations
taxon_res = read.csv("prep/FIS/SAUP_rgns/taxon_resilience_lookup2.csv", stringsAsFactors = FALSE) %>%
  mutate(common = ifelse(common %in% "Silver croaker", paste(common, sciname, sep=" "), common)) %>%
  dplyr::select(Common_Name=common, Resilience)

#Filter out all stocks that don’t meet our conditions:
#Keep all stocks that have at least 1000 tons mean annual harvest
#Keep all stocks with time series of 20 years or more

#set variables to filter by
min_yrs = 20
min_tons = 1000

#read in catch data created above
df <- read.csv("prep/FIS/SAUP_rgns/spatial_catch_saup_new.csv",stringsAsFactors=F)

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
write.csv(stks, "prep/FIS/SAUP_rgns/spatial_catch_pre_bbmsy_new.csv", row.names=FALSE)

################# Load Catch Data###########

catch<- read.csv('prep/FIS/SAUP_rgns/spatial_catch_pre_bbmsy_new.csv')%>%
  rename(common = Common_Name)

####Catch MSY#####

cmsy_fits <- plyr::dlply(catch, c("stock_id", "common"), function(x) {

  #make sure the data is ordered from 1950 to 2014
  x <- arrange(x,year)
  out <- cmsy(ct = x$tons, yr = x$year,  start_r = resilience(x$Resilience[1]),
              reps = 2e4)
  out$year <- x$year
  out
}, .parallel = TRUE)

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
write.csv(cmsy_bbmsy, "prep/FIS/catch_model_bmsy/cmsy_bbmsy_new.csv", row.names=FALSE)

nas <- cmsy_bbmsy%>%
  group_by(stock_id)%>%
  summarize(m = mean(bbmsy_mean))%>%
  filter(is.na(m))

nrow(nas)

##### Catch MSY with a Uniform Prior########
cmsy_fits_uni <- plyr::dlply(catch, c("stock_id", "common"), function(x) {

  #make sure the data is ordered from 1950 to 2010
  x <- arrange(x,year)

  out <- cmsy(x$tons, yr = x$year,  start_r = resilience(x$Resilience[1]),
              reps = 2e4, finalbio = c(0.01, 0.7))
  out$year <- x$year
  out
}, .parallel = TRUE)

fake_data <- data.frame(bbmsy_q2.5 = NA, bbmsy_q25 = NA, bbmsy_q50 = NA,
                        bbmsy_q75 = NA, bbmsy_q97.5 = NA)

cmsy_bbmsy_uni <- plyr::ldply(cmsy_fits_uni, function(x) {
  bbmsy_cmsy <- x$biomass[, -1] / x$bmsy
  bbmsy_out <- tryCatch({
    bbmsy_out <- summarize_bbmsy(bbmsy_cmsy)
    bbmsy_out$year <- x$year
    bbmsy_out}, error = function(e) fake_data)
})
cmsy_bbmsy_uni$model <- "CMSY_uniform"

write.csv(cmsy_bbmsy_uni, "prep/FIS/catch_model_bmsy/cmsy_bbmsy_uni_prior_new.csv", row.names=FALSE)

