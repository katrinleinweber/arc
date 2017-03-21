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


###############################################
## Combining the most current data for each habitat (2016 analysis)
## Currently, only the sea ice is updated regularly
#############################################

source("src/R/common.R")
pathCoral <- "globalprep/hab_coral/v2012/data"
pathMangrove <- "globalprep/hab_mangrove/v2015/data"
pathRockyreef <- "globalprep/hab_rockyreef/v2012/data"
pathSaltmarsh <- "globalprep/hab_saltmarsh/v2012/data"
pathSeagrass <- "globalprep/hab_seagrass/v2012/data"
pathSoftbottom <- "globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output"
pathSeaice <- "globalprep/hab_seaice/v2016/output"

## Extent data ----
coral_extent <- read.csv(file.path(pathCoral, "habitat_extent_coral.csv"))
mangrove_extent <- read.csv(file.path(pathMangrove, "habitat_extent_mangrove.csv"))
rockyreef_extent <- read.csv(file.path(pathRockyreef, "habitat_extent_rocky_reef.csv"))
saltmarsh_extent <- read.csv(file.path(pathSaltmarsh, "habitat_extent_saltmarsh.csv"))
seagrass_extent <- read.csv(file.path(pathSeagrass, "habitat_extent_seagrass.csv"))
softbottom_extent <- read.csv(file.path(pathSoftbottom, "habitat_extent_softbottom.csv"))
seaice_extent <- read.csv(file.path(pathSeaice, "hab_ice_extent_eez.csv"))

head(coral_extent)
head(mangrove_extent)
head(rockyreef_extent)
head(saltmarsh_extent)
head(seagrass_extent)
head(softbottom_extent)
head(seaice_extent)


habitat_extent_v2015 <- rbind(coral_extent, mangrove_extent, rockyreef_extent,
                              saltmarsh_extent, seagrass_extent, softbottom_extent, seaice_extent)
table(habitat_extent_v2015$habitat)
write.csv(habitat_extent_v2015, "globalprep/hab_combined/v2016/output/habitat_extent_v2015.csv", row.names=FALSE)
## habitat extent uses the same data for all years of analysis

## Health data ----
# everything stays the same except for sea ice and soft-bottom(which are updated every year)
coral_health <- read.csv(file.path(pathCoral, "habitat_health_coral.csv"))
mangrove_health <- read.csv(file.path(pathMangrove, "habitat_health_mangrove.csv")) %>%
  select(rgn_id, habitat, health)
saltmarsh_health <- read.csv(file.path(pathSaltmarsh, "habitat_health_saltmarsh.csv"))
seagrass_health <- read.csv(file.path(pathSeagrass, "habitat_health_seagrass.csv"))

allbutsi_sb_health <- rbind(coral_health, mangrove_health, saltmarsh_health, seagrass_health)
table(allbutsi_sb_health$habitat)


# health function
habitat_summary <- function(ScenarioYear, dataYear_si, dataYear_sb){
  seaice_health <- read.csv(file.path(pathSeaice, sprintf("hab_ice_health_eez_%s.csv", dataYear_si)))
  softbottom_health <- read.csv(file.path(pathSoftbottom, sprintf("habitat_health_softbottom_v%s.csv", dataYear_sb)))
  habitat_health <- rbind(allbutsi_sb_health, seaice_health, softbottom_health)
  write.csv(habitat_health, sprintf("globalprep/hab_combined/v2016/output/habitat_health_%s.csv", ScenarioYear), row.names=FALSE)
}

habitat_summary(ScenarioYear=2016, dataYear_si=2015, dataYear_sb=2010)
habitat_summary(ScenarioYear=2015, dataYear_si=2014, dataYear_sb=2009)
habitat_summary(ScenarioYear=2014, dataYear_si=2013, dataYear_sb=2008)
habitat_summary(ScenarioYear=2013, dataYear_si=2012, dataYear_sb=2007)
habitat_summary(ScenarioYear=2012, dataYear_si=2011, dataYear_sb=2006)


## Trend data ----
coral_trend <- read.csv(file.path(pathCoral, "habitat_trend_coral.csv"))
saltmarsh_trend <- read.csv(file.path(pathSaltmarsh, "habitat_trend_saltmarsh.csv"))
seagrass_trend <- read.csv(file.path(pathSeagrass, "habitat_trend_seagrass.csv"))

allbut_si_sb_trend <- rbind(coral_trend, saltmarsh_trend, seagrass_trend)
table(allbut_si_sb_trend$habitat)


# trend 2015
trend_summary <- function(ScenarioYear, dataYear_si, dataYear_man, dataYear_sb){
  seaice_trend <- read.csv(file.path(pathSeaice, sprintf("hab_ice_trend_eez_%s.csv", dataYear_si)))
  mangrove_trend <- read.csv(file.path(pathMangrove, sprintf('habitat_trend_mangrove_v%s.csv', dataYear_man)))
  softbottom_trend <- read.csv(file.path(pathSoftbottom, sprintf('habitat_trend_softbottom_v%s.csv', dataYear_sb)))
  habitat_trend <- rbind(allbut_si_sb_trend, seaice_trend, mangrove_trend, softbottom_trend)
  summary(habitat_trend)
  write.csv(habitat_trend, sprintf("globalprep/hab_combined/v2016/output/habitat_trend_%s.csv", ScenarioYear), row.names=FALSE)
}

trend_summary(ScenarioYear=2016, dataYear_si=2015, dataYear_man=2015, dataYear_sb=2010)
trend_summary(ScenarioYear=2015, dataYear_si=2014, dataYear_man=2015, dataYear_sb=2009)
trend_summary(ScenarioYear=2014, dataYear_si=2013, dataYear_man=2014, dataYear_sb=2008)
trend_summary(ScenarioYear=2013, dataYear_si=2012, dataYear_man=2013, dataYear_sb=2007)
trend_summary(ScenarioYear=2012, dataYear_si=2011, dataYear_man=2012, dataYear_sb=2006)
