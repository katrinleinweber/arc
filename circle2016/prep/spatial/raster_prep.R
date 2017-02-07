create raster:

source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(dplyr)

save_loc <- file.path('circle2016/prep/spatial')


## raster template that is used for the 1km grid
raster_template <- raster(file.path(dir_M,
                                    'git-annex/globalprep/prs_oa/v2015/working/annual_oa_rescaled_1km/annual_oa_rescaled_1km_2005.tif'))

# read and explore region file.
spatial_dir<- 'circle2016/prep/spatial'
layer_arc<- 'arctic_eezs'
poly_arc_rgn<- readOGR(dsn= spatial_dir, layer = layer_arc, stringsAsFactors = FALSE)
#mol reproject
p4s_mol<- CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')
regions_mol<- spTransform(poly_arc_rgn, p4s_mol) #reprojected arc so can crop it

## Make sure the overlap between the shape file and the raster looks about right:
plot(raster_template)
plot(regions_mol, add=TRUE)
#It looks like the raster and spatial file align ok.

#explore the data:
head(regions_mol@data)
table(regions_mol@data$rgn_typ)
table(regions_mol@data$ant_typ)
