######################################
## SST extract for regions
##
######################################

source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(sp)
library(dplyr)

##Arc region in mol

spatial_dir<- 'circle2016/prep/spatial'
layer_arc<- 'arctic_eezs'
poly_arc_rgn<- readOGR(dsn= spatial_dir, layer = layer_arc, stringsAsFactors = FALSE)
p4s_arc<- CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs') #create p4s in mol to match chem data
poly_arc_rgn2 <- spTransform(poly_arc_rgn, p4s_arc) #transform to mol


# raster/zonal data
sst_loc <- file.path(dir_M, "git-annex/globalprep/prs_sst/v2016/output")

# save location
sst_save_loc <- "globalprep/prs_sst/v2016"

# read in raster files
rasts <- list.files(file.path(sst_loc, "sea_ice_mask"))

pressure_stack <- stack()

for(raster in rasts){ #raster="sea_ice_mask_sst_1986-1990_1985-1989.tif"
  tmp <- raster(file.path(sst_loc, "sea_ice_mask", raster))
  pressure_stack <- stack(pressure_stack, tmp)
}

## some exploring:
plot(pressure_stack[[5]], col=rev(heat.colors(255)))
click(pressure_stack[[5]])

##extract
sst_data <- raster::extract(pressure_stack, poly_arc_rgn2, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
