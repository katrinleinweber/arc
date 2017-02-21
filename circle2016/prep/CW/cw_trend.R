## Need to extract 3nm data - should have done this before (in addition to a pressure, this will be used for CW and the CW trend)
# (going to try using the polygon, rather than converting to raster)

source('~/github/ohiprep/src/R/common.R')
library(raster)
library(rgdal)
library

# save location
save_loc_rasts <- file.path(dir_M, "git-annex/globalprep/prs_chem/v2016/int")
save_loc_data <- file.path(dir_M, "git-annex/globalprep/prs_chem/v2016")

##################
## to get the chemical pressure: pesticides + ocean pollution + inorganic pollution

chem_scaled <- raster(file.path(save_loc_rasts, "chemical_pollution_2013_scaled.tif"))

spatial_dir<- 'prep/spatial'
rast_arc<- raster::raster(file.path(spatial_dir, "rast_arc_3km.tif"))

poly_arc_3nm<- rasterToPolygons(rast_arc, dissolve=TRUE) #convert raster to polygon - need to add region information

writeOGR(poly_arc_3nm, dsn = 'prep/spatial', layer = 'poly_arc_3nm', driver = "ESRI Shapefile")



# this is here in case I decide to use this method instead of using the polygons to extract the data:
# tmp <- raster(file.path(rast_locs, "Frazier/global_plumes_fert_2007_raw_log_extend.tif"))
# rasterize(inland_3nm_poly, tmp, field='rgn_id',
#           filename=file.path(rast_locs, "Frazier/inland_3nm.tif"), overwrite=TRUE,
#           progress='text')

data <- raster::extract(chem_scaled, poly_arc_3nm, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
