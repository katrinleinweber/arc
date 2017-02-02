######################################
## UV extract for regions
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
uv_loc <- file.path(dir_M, "git-annex/globalprep/prs_uv/v2016/output")

# save location
uv_save_loc <- "globalprep/prs_uv/v2016"

# read in raster files
rasts <- list.files(uv_loc)

pressure_stack <- stack()
for(raster in rasts){ # raster = "uv_2007_2011-2005-2009_rescaled_moll_1km.tif"
  tmp <- raster(file.path(uv_loc, raster))
  pressure_stack <- stack(pressure_stack, tmp)
}

## some exploring:
plot(pressure_stack[[5]], col=rev(heat.colors(255)))
click(pressure_stack[[5]])

##extract
uv_data <- raster::extract(pressure_stack, poly_arc_rgn2, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
prs_uv<- gather(uv_data, "year", "pressure_score", starts_with("uv"))
prs_uv2 <- prs_uv %>%
  mutate(year = substring(year, 9, 12)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(year = year + 1) %>%
  dplyr::select(ID, year, pressure_score)


###extract data more easily
saveData <- function(newYear){


  criteria_year <- ~year == newYear

  uv  <- prs_uv2 %>%
    filter_(criteria_year) %>%
    dplyr::select(ID, pressure_score) %>%
    arrange(ID)

  write.csv(uv, file.path(spatial_dir, sprintf('uv_%s.csv', newYear)), row.names=FALSE)
}
### extract data
for(newYear in (max(prs_uv2$year) - 4):(max(prs_uv2$year))){
  saveData(newYear)
}

