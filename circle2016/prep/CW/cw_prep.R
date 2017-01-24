source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(dplyr)

##Create 1km raster of Arc study region

spatial_dir<- 'circle2016/prep/spatial'
layer_arc<- 'arctic_eezs'
poly_arc_rgn<- readOGR(dsn= spatial_dir, layer = layer_arc, stringsAsFactors = FALSE)
p4s_arc<- CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs') #create p4s in mol to match chem data
poly_arc_rgn2 <- spTransform(poly_arc_rgn, p4s_arc) #transform to mol

### have blanked this out as raster doesn't work. Have extracted by shapefile instead.
#ext2 <- extent(poly_arc_rgn2); ext2
#ext3 <- poly_arc_rgn2@bbox; ext3
#ext2@xmin <- round(ext2@xmin - 5000, -4); ext2@ymin <- round(ext2@ymin - 5000, -4) #what does the -4 mean?
### expand the extents out to round 10 km edges
#ext2@xmax <- round(ext2@xmax + 5000, -4); ext2@ymax <- round(ext2@ymax + 5000, -4)

#reso <- 934.479 ### BC Albers uses meters as units, set resolution to a 1km grid to match chem_data
#xcol <- (ext2@xmax - ext2@xmin)/reso ### determine # of columns from extents and resolution
#yrow <- (ext2@ymax - ext2@ymin)/reso ### determine # of rows
#rast_base2 <- raster(ext2, yrow, xcol, crs = p4s_arc)

#rast_base2 ### inspect it: resolution and extents are nice and clean

#rast_arc<- rasterize(poly_arc_rgn2, rast_base2) #rasterize EEZs with the rasterbase

##read in pollution data
# save location
save_loc_rasts <- file.path(dir_M, "git-annex/globalprep/prs_chem/v2016/int")
save_loc_data <- file.path(dir_M, "git-annex/globalprep/prs_chem/v2016")

##################
## to get the chemical pressure: pesticides + ocean pollution + inorganic pollution

chem_scaled <- raster(file.path(save_loc_rasts, "chemical_pollution_2013_scaled.tif"))

# extract data for each eez region:
chem_scaled2<- raster::crop(chem_scaled, rast_arc, progress='text')#crop to arctic


data <- raster::extract(chem_scaled2, poly_arc_rgn2, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
write.csv(data, "po_chemicals_arc2016.csv")
