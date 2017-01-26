######################################
## SLR extract for regions
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
slr_loc <- file.path(dir_M, "git-annex/globalprep/prs_slr/v2016/output")

# save location
slr_save_loc <- "globalprep/prs_slr/v2016"

# read in raster files
rasts <- list.files(slr_loc)

pressure_stack <- stack()
for(raster in rasts){ #raster="slr_1993.tif"
  tmp <- raster(file.path(slr_loc, raster))
  pressure_stack <- stack(pressure_stack, tmp)
}

## some exploring:
plot(pressure_stack[[5]], col=rev(heat.colors(255)))
click(pressure_stack[[5]])

##extract
slr_data <- raster::extract(pressure_stack, poly_arc_rgn2, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
prs_slr<- gather(slr_data, "year", "pressure_score", starts_with("slr"))
prs_slr2 <- prs_slr %>%
  mutate(year=gsub('slr_', '', year)) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::select(ID, year, pressure_score)

###extract data more easily
saveData <- function(newYear){

  assessYear <- newYear + 1
  criteria_year <- ~year == newYear

  slr  <- prs_slr2 %>%
    filter_(criteria_year) %>%
    dplyr::select(ID, pressure_score) %>%
    arrange(ID)

  write.csv(slr, file.path(spatial_dir, sprintf('slr_%s.csv', assessYear)), row.names=FALSE)
}
### extract data
for(newYear in (max(prs_slr2$year) - 4):(max(prs_slr2$year))){
  saveData(newYear)
}
