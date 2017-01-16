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
library('tiff')

##Read in Shape file map of arctic

##laeaCRS <- CRS("+init=epsg:3572") #Arctic Projection


spatial_dir<- 'circle2016/prep/spatial'
layer_arc<- 'arctic_eezs'
poly_arc_rgn<- readOGR(dsn= spatial_dir, layer = layer_arc, stringsAsFactors = FALSE)


##Try and add buffer
#arc_rgn_buff<- rgeos::gBuffer(poly_arc_rgn, byid=FALSE, width=1000,capStyle = "ROUND")

##Read in Arctic Land file
land_arc<- 'arctic_land'
arc_land<- readOGR(dsn= spatial_dir, layer = land_arc, stringsAsFactors = FALSE)

### Had some trouble with croping arc land so Simplify Polygons in order to intersect (well known hack)# simplify the polgons a tad (tweak 0.00001 to your liking)
poly_arc_rgn <- gSimplify(poly_arc_rgn, tol = 0.00001)
arc_land <- gSimplify(arc_land, tol = 0.00001)
arc_land <- gBuffer(arc_land, byid=TRUE, width=0) # these go in to solve topology problems
poly_arc_rgn <- gBuffer(poly_arc_rgn, byid=TRUE, width=0) # solve topology problems

#crop arc_land to scale of poly_arc_rgn
crop_arc_land<- raster::crop(arc_land, extent(-2104837, 3571809, -2556503, 2595893))


##Apply positive buffer to land shapefile
crop_arc_land_buffer <- gBuffer(crop_arc_land, byid=FALSE, width=5556) #5556m = 3nm
arc_3km_buffer<- raster::intersect(crop_arc_land_buffer, poly_arc_rgn) ##intersect to get 3km buffer separated into regions

## create raster of arc_3km_buffer
p4s_arc<- CRS('+proj=laea +lat_0=90 +lon_0=-150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0') #create p4s of arc map
ext <- extent(arc_3km_buffer); ext
ext1 <- arc_3km_buffer@bbox; ext1
ext@xmin <- round(ext@xmin - 5000, -4); ext@ymin <- round(ext@ymin - 5000, -4) #what does the -4 mean?
### expand the extents out to round 10 km edges
ext@xmax <- round(ext@xmax + 5000, -4); ext@ymax <- round(ext@ymax + 5000, -4)

reso <- 500 ### BC Albers uses meters as units, set resolution to a 0.5-km grid
xcol <- (ext@xmax - ext@xmin)/reso ### determine # of columns from extents and resolution
yrow <- (ext@ymax - ext@ymin)/reso ### determine # of rows
rast_base <- raster(ext, yrow, xcol, crs = p4s_arc)

rast_base ### inspect it: resolution and extents are nice and clean

rast_arc_3km<- rasterize(rast_arc_3km, rast_base) #rasterize EEZs with the rasterbase
writeRaster(rast_arc_3km, filename="rast_arc_3km.tif", overwrite=TRUE)


##Need to create land shape file which is divided into regions
#poly_arc_land <- gSimplify(crop_arc_land, tol = 0.00001)
#poly_arc_land<- raster::intersect(poly_arc_land, poly_arc_rgn)
## Going to try and read in tif file for 1km inshore buffer.


rast_1km_file <- file.path(dir_M, 'git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_500m/rgn_inland1km_mol_500mcell.tif')
rast_1km_buffer <- raster::raster(rast_1km_file)

raster::plot(rast_1km_buffer)
##Need to reproject into arc projection
p4s_1km<- CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')
p4s_arc<- CRS('+proj=laea +lat_0=90 +lon_0=-150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0') #create p4s of arc map
crop_arc_reproject<- spTransform(crop_arc_land, p4s_1km) #reprojected arc so can crop it
#crop raster to arctic
arc_1km<- crop(rast_1km_buffer, crop_arc_reproject) #crop to wider arctic
arc_1km <- projectRaster(arc_1km, crs = p4s_arc) #project back to arctic coordinates
## save progress - but need to work out how to split into regions
writeRaster(arc_1km, filename="rast_arc_1km.tif", overwrite=TRUE)



##### Sorting out MPA Layer ######

source('~/github/ohiprep/src/R/common.R')

goal     <- 'lsp'
scenario <- 'v2016'
dir_anx       <- file.path(dir_M, 'git-annex/globalprep')
dir_goal      <- file.path('~/github/ohiprep/globalprep', goal, scenario)
dir_goal_anx  <- file.path(dir_anx, goal, scenario)
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




ext2 <- extent(poly_arc_rgn); ext2
ext3 <- poly_arc_rgn@bbox; ext3
ext2@xmin <- round(ext2@xmin - 5000, -4); ext2@ymin <- round(ext2@ymin - 5000, -4) #what does the -4 mean?
### expand the extents out to round 10 km edges
ext2@xmax <- round(ext2@xmax + 5000, -4); ext2@ymax <- round(ext2@ymax + 5000, -4)

reso <- 500 ### BC Albers uses meters as units, set resolution to a 0.5-km grid
xcol <- (ext2@xmax - ex2t@xmin)/reso ### determine # of columns from extents and resolution
yrow <- (ext2@ymax - ext2@ymin)/reso ### determine # of rows
rast_base2 <- raster(ext2, yrow, xcol, crs = p4s_arc)

rast_base2 ### inspect it: resolution and extents are nice and clean

rast_arc<- rasterize(poly_arc_rgn, rast_base2) #rasterize EEZs with the rasterbase

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
### Cross tabulate OHI/EEZ rasters. This givees number of protected cells with year of protection within each region. NA = unprotected cells

rast_df <- raster::crosstab(rast_wdpa_proj, rast_arc, useNA = TRUE, progress = 'text') %>%
  as.data.frame() %>%
  setNames(c('year', 'rgn_id', 'n_cells')) %>%
  mutate(year   = as.integer(as.character(year)),
         rgn_id = as.integer(as.character(rgn_id))) %>%
  arrange(rgn_id, year)

#### calculate protected area total by region####
lsp_thresh<- 0.30

prot_eez <- rast_df %>%
  group_by(rgn_id) %>%
  mutate(n_cells_tot = sum(n_cells),
         n_cells_cum = cumsum(n_cells),
         a_tot_km2   = n_cells_tot / 4,
         a_prot_km2  = n_cells_cum / 4) %>%
  ungroup() %>%
  filter(!is.na(year))  %>% ### this ditches non-protected cell counts but already counted in n_cells_tot
  mutate(pct_prot   = round(n_cells_cum / n_cells_tot, 4),
         lsp_status = round(ifelse(pct_prot > lsp_thresh, 100, (pct_prot / lsp_thresh) * 100), 2)) %>%
  distinct() #still have NA in rgn_id?
write_csv(prot_eez, file.path('~/github/arc/circle2016/prep/spatial/area_protected_eez.csv'))
