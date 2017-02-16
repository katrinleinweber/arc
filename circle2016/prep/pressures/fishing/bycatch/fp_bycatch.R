####Extract bycatch#####
source('~/github/ohiprep/src/R/common.R')

#set options for all chunks in code
knitr::opts_chunk$set(warning=FALSE, message=FALSE,fig.width = 6, fig.height = 4, fig.path = 'figs/')

library(parallel)
library(foreach)
library(doParallel)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(raster)
library(rgdal)
library(dplyr)

zones <- raster('prep/spatial/rast_arc.tif')  # raster data
### Low bycatch data first
# get raster data:
rasts <- list.files(file.path(dir_M,'git-annex/globalprep/prs_fish/v2016/out/low_bycatch'))

pressure_stack <- stack()
for(raster in rasts){ # raster = "lb_fish_pressure_2003-2007.tif"
  tmp <- raster(file.path(dir_M,'git-annex/globalprep/prs_fish/v2016/out/low_bycatch', raster))
  pressure_stack <- stack(pressure_stack, tmp)
}

spatial_dir<- 'prep/spatial'
layer_arc<- 'arctic_eezs'
poly_arc_rgn<- readOGR(dsn= spatial_dir, layer = layer_arc, stringsAsFactors = FALSE)
p4s_arc<- CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs') #create p4s in mol to match chem data
poly_arc_rgn2 <- spTransform(poly_arc_rgn, p4s_arc) #transform to mol

##Extract and organise

region_stats <- raster::extract(pressure_stack, poly_arc_rgn2, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
fp_prs<-  gather(region_stats, "year", "pressure_score", starts_with("lb"))
lb_data <- fp_prs %>%
  mutate(year = substring(year, 23, 26)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(year = year + 6) %>%
  dplyr::select(ID, year, pressure_score)

#extract

# function to extract data more easily
saveData <- function(newYear){ # newYear= 2012

  criteria_year <- ~year == newYear

  tmp  <- lb_data %>%
    filter_(criteria_year) %>%
    dplyr::select(ID, pressure_score) %>%
    arrange(ID)

  write.csv(tmp, sprintf('prep/pressures/fishing/bycatch/low/comm_fish_lb_%s.csv', newYear), row.names=FALSE)
}

### extract data
for(newYear in (max(lb_data$year) - 3):(max(lb_data$year))){
  saveData(newYear)
}


#####HIGH BYCATCH######
### Get the high bycatch data

rasts <- list.files(file.path(dir_M,'git-annex/globalprep/prs_fish/v2016/out/high_bycatch'))

pressure_stack <- stack()
for(raster in rasts){ # raster = "lb_fish_pressure_rescaled_2003-2007.tif"
  tmp <- raster(file.path(dir_M,'git-annex/globalprep/prs_fish/v2016/out/high_bycatch', raster))
  pressure_stack <- stack(pressure_stack, tmp)
}

##Extract and organise

region_stats <- raster::extract(pressure_stack, poly_arc_rgn2, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
fp_prs<-  gather(region_stats, "year", "pressure_score", starts_with("hb"))
hb_data <- fp_prs %>%
  mutate(year = substring(year, 23, 26)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(year = year + 6) %>%
  dplyr::select(ID, year, pressure_score)

#extract

# function to extract data more easily
saveData <- function(newYear){ # newYear= 2012

  criteria_year <- ~year == newYear

  tmp  <- lb_data %>%
    filter_(criteria_year) %>%
    dplyr::select(ID, pressure_score) %>%
    arrange(ID)

  write.csv(tmp, sprintf('prep/pressures/fishing/bycatch/high/comm_fish_hb_%s.csv', newYear), row.names=FALSE)
}

### extract data
for(newYear in (max(hb_data$year) - 3):(max(hb_data$year))){
  saveData(newYear)
}
