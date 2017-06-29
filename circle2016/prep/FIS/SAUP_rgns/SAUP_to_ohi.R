###############################################
## Associates the SAUP raster cells with the
## corresponding ohi regions
##
###############################################
install.packages("devtools")
devtools::install_github("SeaAroundUs/rseaaroundus")

library(reshape2)
library(dplyr)
library(seaaroundus)
library(rgdal)
library(raster)

source('~/github/ohiprep/src/R/common.R')

spatial_dir<- 'circle2016/prep/spatial'
layer_arc<- 'arctic_eezs'
save_loc<- 'circle2016/prep/FIS'

ohi_regions <- readOGR(dsn= spatial_dir, layer = layer_arc, stringsAsFactors = FALSE)


saup_cells  <- getcells("POLYGON ((-180 90,-180 -90, 180 -90, 180 90, -180 90))")
saup_rast   <- raster(ncol=720, nrow=360)
saup_rast[] <- saup_cells
p4s_arc<- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0') #create p4s of arc map
ohi_regions2 <- spTransform(ohi_regions, p4s_arc) #reproject
ohi_to_saup_raster <- raster::extract(saup_rast, ohi_regions2, weights = TRUE, normalizeWeights = FALSE, progress = 'text')
names(ohi_to_saup_raster) <- paste(ohi_regions@data$rgn_id, ohi_regions@data$rgn_name, sep="_")
region_prop_df            <- plyr::ldply(ohi_to_saup_raster, rbind)

###save csv

write.csv(region_prop_df, "saup_rasters_to_ohi_rgns.csv", row.names=FALSE)

##add fao

fao_rgns <- read.csv(file.path(dir_M,
                               "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_fao_rgns.csv"))
final <- fao_rgns %>%
  left_join(region_prop_df, by="saup_cell_id")%>%
  filter(fao_id %in% c("18", "21", "27"))
write.csv(final, "prep/FIS/SAUP_rgns/saup_rasters_to_ohi_fao_rgns.csv", row.names=FALSE)
