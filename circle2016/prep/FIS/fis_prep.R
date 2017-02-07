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


## Paths for data
path_data               = "/home/shares/ohi/git-annex/globalprep/_raw_data/SAUP/d2016/Data"
file_allocation_data    = "SeaAroundUs/AllocationData.dat"
file_allocation_results = "SeaAroundUs/AllocationResult.dat"
file_taxon              = "SeaAroundUs/taxon.dat"
file_entity             = "FishingEntity.dat"

spatial_dir<- 'circle2016/prep/spatial'
fis_dir<- 'circle2016/prep/FIS'
ohi_rgns <- raster(file.path(spatial_dir, "sp_mol_raster.tif"))

###LOAD DATA#####
# load the Allocation info using fread, and define column names
dt_data           <- fread(file.path(path_data,file_allocation_data), sep=";", header = FALSE)
colnames(dt_data) <- c("UniversalDataID","DataLayerID","FishingEntityID", "Year", "TaxonKey",
                       "InputTypeID", "sector_type_name", "catch_type_name",
                       "reporting_status_name")


#load the Results data (largest file, usually takes up to 10 minutes to read!)
dt_results           <- fread(file.path(path_data,file_allocation_results), sep=";", header = FALSE)
colnames(dt_results) <- c("UniversalDataID","CellID","AllocatedCatch")
# setkey(dt_results,UniversalDataID) # not necessary the data seems to be already ordered with the keys (univ and Cell IDs)


#load the Taxon data
dt_taxon           <- fread(file.path(path_data,file_taxon), sep=";", header = FALSE)
colnames(dt_taxon) <- c("TaxonKey","Scientific_Name","Common_Name","FunctionalGroupDescription")
setkey(dt_taxon,TaxonKey)


#load the fishing entity data
dt_entity           <- fread(file.path(path_data,file_entity), sep = ";", header=FALSE)
colnames(dt_entity) <- c("FishingEntityID","Name")
setkey(dt_entity,FishingEntityID)
