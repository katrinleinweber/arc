## Need to extract 3nm data - should have done this before (in addition to a pressure, this will be used for CW and the CW trend)
# (going to try using the polygon, rather than converting to raster)

source('~/github/ohiprep/src/R/common.R')
library(raster)
library(rgdal)


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
status_data<- data %>% dplyr::select(ID, pressure_score = chemical_pollution_2013_scaled)
write.csv(status_data, "cw_chemical_score_3nm_arc2016.csv", row.names=FALSE)


###chemical trend: pesticides + ocean pollution + inorganic pollution

rast_locs<- file.path('/home/shares/ohi/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/raw_global_results')
list_chem <- files <- grep("chemical_pollution", list.files(file.path(rast_locs, "Frazier")), value=TRUE)
list_chem_scaled <- grep("_scaled", list_chem, value=TRUE)

pressure_stack <- stack()
for(rast in list_chem_scaled){
  tmp <- raster(file.path(rast_locs, "Frazier", rast))
  pressure_stack <- stack(pressure_stack, tmp)}

poly_arc_3nm<- readOGR(dsn = 'prep/spatial', layer = 'poly_arc_3nm')
poly_arc_rgn<- readOGR(dsn='prep/spatial', layer = 'arctic_eezs')
regions_stats <- raster::extract(pressure_stack, poly_arc_3nm, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
eez_stats <- raster::extract(pressure_stack, poly_arc_rgn, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")

#write.csv(regions_stats, 'prep/CW/chemical_data_offshore_3nm.csv', row.names = FALSE)

#######EEZ SCORE DATA########
data <- gather(eez_stats, "year", "pressure_score", starts_with("chemical"))
data <- data %>%
  mutate(year = gsub("chemical_pollution_", "", year)) %>%
  mutate(year = gsub("_scaled", "", year)) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  dplyr::select(ID, year, pressure_score)

for(scenario_year in 2012:2015){ #scenario_year=2015
  #calculate/save pressure score data
  score_data <- data %>%
    filter(year == (scenario_year-3)) %>%
    dplyr::select(ID, pressure_score)
  write.csv(score_data, file.path('prep/CW', sprintf('cw_chemical_score_%s.csv', scenario_year)), row.names=FALSE)
}

#####3nm DATA########

data <- gather(regions_stats, "year", "pressure_score", starts_with("chemical"))
data <- data %>%
  mutate(year = gsub("chemical_pollution_", "", year)) %>%
  mutate(year = gsub("_scaled", "", year)) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  dplyr::select(ID, year, pressure_score)

## trend should be calculated on 3nm (not eez)
for(scenario_year in 2012:2015){ #scenario_year=2015
  #calculate/save trend data
  trend_data <- data %>%
    filter(year %in% (scenario_year-7):(scenario_year-3)) %>%
    group_by(ID) %>%
    do(mdl = lm(pressure_score ~ year, data = .)) %>%
    summarize(ID,
              trend = coef(mdl)['year'] * 5) %>%
    ungroup()
  write.csv(trend_data, file.path('prep/CW', sprintf('cw_chemical_trend_%s.csv', scenario_year)), row.names=FALSE)
  score_data <- data %>%
    filter(year == (scenario_year-3)) %>%
    dplyr::select(ID, pressure_score)
  write.csv(score_data, file.path('prep/CW', sprintf('cw_chemical_score_3nm_%s.csv', scenario_year)), row.names=FALSE)
  }

#####TREND CALCULATIONS#######

chem_data<- read.csv('prep/CW/cw_chemical_score_3nm_arc2016.csv')
new_data<- chem_data%>%
  mutate(year = 2013) %>%
  rename(rgn_id = ID)

old_data <- read.csv("prep/CW/chemical_data_offshore_3nm.csv")
old_data <- gather(old_data, "year", "pressure_score", starts_with("chemical"))
old_data <- old_data %>%
  mutate(year = gsub("chemical_pollution_", "", year)) %>%
  mutate(year = gsub("_scaled", "", year)) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  dplyr::select(ID, year, pressure_score)%>%
  rename(rgn_id = ID)

trend_data <- rbind(new_data, old_data)
summary(trend_data)

## trend calculated on 3nm (not eez)
for(scenario_year in 2012:2016){ #scenario_year=2012

  ## NOTE: trends for 2012 are calculated with 3 years of data
  ##      trends for 2013 are calculaed with 4 years of data, the other years are calculated with 5 years data
  trend_years <- (scenario_year-7):(scenario_year-3)
  adj_trend_year <- ifelse(scenario_year %in% 2016:2014, (scenario_year-7), 2007)

  trends <- trend_data %>%
    filter(!is.na(pressure_score)) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(pressure_score ~ year, data=., subset=year %in% trend_years),
       adjust_trend = .$pressure_score[.$year == adj_trend_year]) %>%
    summarize(rgn_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup() %>%
    mutate(trend = ifelse(trend>1, 1, trend)) %>%
    mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    mutate(trend = round(trend, 4)) %>%
    dplyr::select(rgn_id, trend)
  write.csv(trends, file.path('prep/CW/final_trend', sprintf('cw_chemical_trend_%s_new.csv', scenario_year)), row.names=FALSE)
}
