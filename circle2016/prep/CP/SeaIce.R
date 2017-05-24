######SEA ICE##############
### Regions need to be reprojected into '+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'

require(sp)
library(raster)
library(rgdal)
library(maptools)
library(gridExtra)
library(fields)
library(maps)
library(gpclib)
library(animation)
#library(plyr)
library(reshape2)

source('~/github/ohiprep/src/R/common.R')
poly_arc_rgn<- readOGR(dsn='prep/spatial', layer = 'arctic_eezs')
p4s_ice<- CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs')
#regions_ice<- spTransform(poly_arc_rgn, p4s_ice) #reprojected arc so can crop it
#writeOGR(regions_ice, dsn = 'prep/spatial', layer = 'regions_n_seaice', driver = "ESRI Shapefile"

maps<- file.path('prep/spatial')

# identify the year to save raw data file (Need to create this file)
assessYear <- 'v2016'

# final year of data:
final.year <- 2015

# Establish: CRS, website to collect data, data selection parameters
pixel = 25000    # pixel dimension in meters for both x and y
# epsg projection 3411 - nsidc sea ice polar stereographic north (http://spatialreference.org/ref/epsg/3411/)
# epsg projection 3412 - nsidc sea ice polar stereographic south (http://spatialreference.org/ref/epsg/3412/)
prj.n = '+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'
prj.s = '+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'
prj.mol = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# URL base (ub), filename format for final monthly data is nt_YYYYMM_SSS_vVV_R.bin
ub.n = 'ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/monthly'
ub.s = 'ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/monthly'

poles = 'p'
years = c(1979:final.year)  #Full range of data
months = 1:12
n.pym=length(poles)*length(years)*length(months)
i.pym = 0
t0 = Sys.time()

##run this first so packages can be detached in obtaining data###
detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}
source("prep/CP/ObtainingData.R")

ref.years <- 1979:2000
source("prep/CP/Status_Trend.R")

edge <- read.csv("prep/CP/int/n_IceEdgeHabitat_ref1979to2000.csv")
edge  <- edge %>%
  filter(Reference_avg1979to2000monthlypixels != 0) %>%
  mutate(habitat="seaice_edge")

shore <- read.csv("prep/CP/int/n_IceShoreProtection_ref1979to2000.csv")
shore <- shore %>%
  filter(Reference_avg1979to2000monthlypixels != 0) %>%
  mutate(habitat="seaice_shoreline")

data <- rbind(edge, shore)
data  <- data %>%
  mutate(km2 = Reference_avg1979to2000monthlypixels/12 * (pixel/1000)^2)%>%
  dplyr::filter(zone!=10)%>%
  rename(rgn_id=zone)

write.csv(data, "prep/CP/int/sea_ice.csv", row.names=FALSE)

## ice health data subset/format/save
iceHealth <- function(type='eez', year=final.year){
  dataYear <- paste("pctdevR", year, sep="_")
  filterdata <- data

  filterdata <- subset(filterdata, select=c(rgn_id, habitat, get(dataYear)))
  names(filterdata) <- c('rgn_id', 'habitat', 'health')
  filterdata$health <- ifelse(filterdata$health>1, 1, filterdata$health)
  write.csv(filterdata, sprintf("prep/CP/output/hab_ice_health_%s_%s.csv", type, year), row.names=FALSE)
}

## can also do fao and ccamlr by changing type (not sure if we will need this)
iceHealth(year='2015') # 2016 analysis
iceHealth(year='2014') # 2015 analysis
iceHealth(year='2013') # 2014 analysis
iceHealth(year='2012') # 2013 analysis
iceHealth(year='2011') # 2013 analysis


## ice trend data subset/format/save
iceTrend <- function(type ="eez", year=final.year){
  dataYear <- sprintf("Trend_%sto%s_pctdevRperyr", year-4, year)
  filterdata <- data

  filterdata <- subset(filterdata, select=c(rgn_id, habitat, get(dataYear)))
  names(filterdata) <- c('rgn_id', 'habitat', 'trend')
  filterdata$trend <- filterdata$trend*5
  filterdata$trend <- ifelse(filterdata$trend>1, 1, filterdata$trend)
  filterdata$trend <- ifelse(filterdata$trend<(-1), -1, filterdata$trend)
  write.csv(filterdata, sprintf("prep/CP/output/hab_ice_trend_%s_%s.csv", type, year), row.names=FALSE)
}

## can also do fao and ccamlr by changing type (not sure if we will need this)
iceTrend(year=2015) # 2016 analysis
iceTrend(year=2014) # 2015 analysis
iceTrend(year=2013) # 2014 analysis
iceTrend(year=2012) # 2013 analysis
iceTrend(year=2011) # 2013 analysis

## ice extent data subset/format/save
filterdata <- data

filterdata <- subset(filterdata, select=c(rgn_id, habitat, km2))
type ="eez"
write.csv(filterdata, sprintf("prep/CP/output/hab_ice_extent_%s.csv", type), row.names=FALSE)


