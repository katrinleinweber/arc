###Source Common.R###
knitr::opts_chunk$set(fig.width = 6, fig.height = 4, fig.path = 'Figs/',
                      echo = FALSE, message = FALSE, warning = FALSE)

# library(foreign)
# library(sp)
# library(rgdal)
# library(raster)
# library(maptools)
library(readr)
library(tmap)
source('~/github/ohiprep/src/R/common.R')

### Sorting Shape Files ####
library('dplyr'); library('tidyr'); library('stringr')

### We will do some visualization of maps toward the end with `ggplot2` and `tmap` packages.
library('ggplot2')
library('tmap')
library('RColorBrewer') ### to go along with ggplot - getting better color selection into your plots

### Spatial packages: If you did Jamie's raster workshop, you probably have most of these already, but can't hurt to update.
library('sp')       ### spatial classes and basic spatial functionality
library('rgdal')    ### GDAL functionality in
library('rgeos')    ### vector spatial analysis tools
library('raster')   ### raster stuff, but some handy tools that work great for vector spatial data as well
library('maptools') ### an alternate package with good spatial analysis tools
library('spatstat')

##Read in Shape file map of arctic

##laeaCRS <- CRS("+init=epsg:3572") #Arctic Projection





##Try and add buffer
arc_rgn_buff<- rgeos::gBuffer(poly_arc_rgn, byid=FALSE, width=1000,capStyle = "ROUND")

##Read in Arctic Land file
land_arc<- 'arctic_land'
arc_land<- readOGR(dsn= spatial_dir, layer = land_arc, stringsAsFactors = FALSE)

### Simplify Polygons in order to intersect (well known hack)
arc_land <- gBuffer(poly_arc_land, byid=TRUE, width=0)
poly_arc_rgn <- gBuffer(poly_arc_rgn, byid=TRUE, width=0)
# simplify the polgons a tad (tweak 0.00001 to your liking)
poly_arc_rgn <- gSimplify(poly_arc_rgn, tol = 0.00001)
poly_arc_land <- gSimplify(poly_arc_land, tol = 0.00001)

##Merge Land and EEZ

arc_merge<-raster::union(arc_land, poly_arc_rgn)

poly_arc_ebsa<- readOGR(dsn= spatial_dir, layer = 'EBSA_0511_JC_area_sort') # read in ebsa file
p4s_arc<- CRS('+proj=laea +lat_0=90 +lon_0=-150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0') #create p4s of arc map
poly_arc_ebsa<- spTransform(poly_arc_ebsa, p4s_arc) #reproject ebsa map to same as arctic
### Calculate area of EBSAs in each region, and attach to polygons
poly_arc_ebsa@data$area_km2 <- gArea(poly_arc_ebsa, byid = TRUE) / 1e6
poly_arc_rgn@data$area_km2 <- gArea(poly_arc_rgn, byid = TRUE) / 1e6

### Simplify Polygons in order to intersect (well known hack)
poly_arc_ebsa <- gBuffer(poly_arc_ebsa, byid=TRUE, width=0)
poly_arc_rgn <- gBuffer(poly_arc_rgn, byid=TRUE, width=0)
# simplify the polgons a tad (tweak 0.00001 to your liking)
poly_arc_rgn <- gSimplify(poly_arc_rgn, tol = 0.00001)
poly_arc_ebsa <- gSimplify(poly_arc_ebsa, tol = 0.00001)

### Summarize EBSA in each region
poly_arc_ebsa_rgn<- raster::intersect(poly_arc_ebsa, poly_arc_rgn) #intersect ebsa and map
ebsa_area_df <- poly_arc_ebsa@data %>%
  group_by(rgn_id, rgn_name, rgn_code) %>%
  summarize(ebsa_area_km2 = sum(area_km2)) %>%
  left_join(poly_arc_rgn@data %>%
              select(rgn_id, tot_area_km2 = area_km2),
            by = 'rgn_id') %>%
  mutate(ebsa_area_pct = round(ebsa_area_km2 / tot_area_km2, 3) * 100)


##### Sorting out MPA Layer ######

source('~/github/ohiprep/src/R/common.R')

goal     <- 'lsp'
scenario <- 'v2016'
dir_anx       <- file.path(dir_M, 'git-annex/globalprep')
dir_goal      <- file.path('~/github/ohiprep/globalprep', goal, scenario)
dir_goal_anx  <- file.path(dir_anx,            goal, scenario)
dir_data_wdpa <- file.path(dir_anx, '_raw_data/wdpa_mpa', 'd2016')

#source(file.path(dir_goal, 'lsp_prep_wdpa_poly.R'))
#arctic_ext <- extent(c(-180, +180, -60, +60)) ##create extent object for study area

#wdpa_dir<- "/home/shares/ohi/git-annex/globalprep/_raw_data/wdpa_mpa/d2016/WDPA_May2016-shapefile"
#wdpa_lay<- 'WDPA_May2016-shapefile-polygons'
#wdpa_poly<- readOGR(dsn= wdpa_dir, layer= wdpa_lay, stringsAsFactors = FALSE) #read in WDPA data
#p4s_arc<- CRS('+proj=laea +lat_0=90 +lon_0=-150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0') #create p4s of arc map
# wdpa_poly <- spTransform(wdpa_poly, p4s_arc) #reproject
#arctic_mpa<- raster::crop(wdpa_poly, arctic_ext) # crop to arctic region

## Above crop doesn't work so read in raster of wdpa
###for rasters:
#To retrieve or describe the CRS:
  #projection(x)
#projection(x)<‐ CRS(“+init=epsg:28992”)
##To transform from one CRS to another:
  ##newData <‐ projectRaster(x, CRS(proj4string(OtherData)))

rast_wdpa_file <- file.path(dir_goal_anx, 'int/wdpa_designated_mol.tif')
rast_wdpa <- raster::raster(rast_wdpa_file)

raster::plot(rast_wdpa)

##WDPA file read in - next rasterise buffer layers.
spatial_dir<- 'circle2016/prep/spatial'
layer_arc<- 'arctic_eezs'
poly_arc_rgn<- readOGR(dsn= spatial_dir, layer = layer_arc, stringsAsFactors = FALSE)







ext <- extent(poly_arc_rgn); ext
ext1 <- poly_arc_rgn@bbox; ext1
ext@xmin <- round(ext@xmin - 5000, -4); ext@ymin <- round(ext@ymin - 5000, -4) #what does the -4 mean?
### expand the extents out to round 10 km edges
ext@xmax <- round(ext@xmax + 5000, -4); ext@ymax <- round(ext@ymax + 5000, -4)

reso <- 500 ### BC Albers uses meters as units, set resolution to a 0.5-km grid
xcol <- (ext@xmax - ext@xmin)/reso ### determine # of columns from extents and resolution
yrow <- (ext@ymax - ext@ymin)/reso ### determine # of rows
rast_base <- raster(ext, yrow, xcol, crs = p4s_arc)

rast_base ### inspect it: resolution and extents are nice and clean

rast_arc<- rasterize(poly_arc_rgn, rast_base)

rast_wdpa_proj<- projectRaster(rast_wdpa, rast_arc, method= 'ngb') #reproject wdpa into arc CRS

wdpa_by_rgn <- raster::extract(rast_wdpa_proj, poly_arc_rgn, weights = FALSE, progress='text')
names(wdpa_by_rgn) <- poly_arc_rgn@data$rgn_id

### For the dataframe without cell weights, each list is just a
### vector of values, so we can simply assign that to a column in
### the data frame.
wdpa_rgn_df <- data.frame()
for (rgn_id in names(wdpa_by_rgn)) {
  temp_df <- data.frame(rgn_id = as.numeric(rgn_id),
                        year   = unlist(wdpa_by_rgn[[rgn_id]]))
  wdpa_rgn_df <- rbind(wdpa_rgn_df, temp_df)
}

prot_area_df <- wdpa_rgn_df %>%
  group_by(rgn_id) %>%
  summarize(cells_mpa     = sum(!is.na(year)),
            cells_tot     = n(),
            prot_area_pct = round(cells_mpa/cells_tot, 3) * 100) %>%
  left_join(poly_arc_rgn@data %>%
              dplyr::select(rgn_id, rgn_name),
            by = 'rgn_id')

knitr::kable(prot_area_df)

###Sort out extent of MPA layer
rast_wdpa<- raster::trim(rast_wdpa, values=NA)

ext <- raster::extent(c('xmin' = -12e6, 'xmax' = -8e6, 'ymin' = 5e6, 'ymax' = 1e7))
rast_wdpa_crop <- raster::crop(rast_wdpa, ext)

wdpa_p4s<- CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs') # get CRS of the wdpa raster
poly_arc_mol<- spTransform(poly_arc_rgn, wdpa_p4s) # transform the arc rgn map to same as wdpa raster and extract the extent
ext <- raster::extent(c('xmin' = -9791080, 'xmax' = 9429227, 'ymin' = 6524604, 'ymax' = 8922460)) #use mol extent to crop the wdpa raster
rast_wdpa_crop <- raster::crop(rast_wdpa, ext)
