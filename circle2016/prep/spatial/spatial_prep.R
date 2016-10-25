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

##Packages
library('dplyr'); library('tidyr'); library('stringr'); library('readr')

### We will do some visualization of maps toward the end with `ggplot2` and `tmap` packages.
library('ggplot2')
library('tmap')
library('RColorBrewer') ### to go along with ggplot - getting better color selection into your plots

### Spatial packages: If you did Jamie's raster workshop, you probably have most of these already, but can't hurt to update.
library('sp')       ### spatial classes and basic spatial functionality
library('rgdal')    ### GDAL functionality in R
library('rgeos')    ### vector spatial analysis tools
library('raster')   ### raster stuff, but some handy tools that work great for vector spatial data as well
library('maptools') ### an alternate package with good spatial analysis tools

### Sorting Shape Files ####
##Read in Shape file map of arctic
spatial_dir<- 'circle2016/prep/spatial'
layer_arc<- 'arctic_eezs'
poly_arc_rgn<- readOGR(dsn= spatial_dir, layer = layer_arc, stringsAsFactors = FALSE)

##Try add buffer
arc_buffer<- rgeos::gBuffer(poly_arc_rgn, byid = FALSE, id=NULL, width=1000) #units = m

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
