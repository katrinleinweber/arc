
library(foreign)
library(data.table) # for fread()
library(sp)
library(rgdal)
library(raster)
library(maptools)
library(readr)
##############################################################################=
  ### Generate half-degree raster using am_cells to assign LOICZID to CenterLong x CenterLat.
  ### * Template and output in <dir_anx>/rgns
  ### * rasterize takes about 10 seconds...
  ##############################################################################=
source('/home/burgass/github/ohiprep/src/R/common.R')
dir_anx<- '/home/shares/ohi/git-annex/globalprep/spp_ico/rgns'
dir_data_am<- '/home/shares/ohi/git-annex/globalprep/_raw_data/aquamaps/d2015'
loiczid_raster_file  <- file.path(dir_anx, 'loiczid_raster.grd') #had to change file location as structure changed.
raster_template_file <- file.path(dir_anx, 'spp_ico/rgns/am_cells_template.tif')

loiczid_raster <- raster::raster(loiczid_raster_file)


##############################################################################=
ogr_location = file.path('~/circle2016/prep/spatial')
rgn_layer    = 'arctic_eezs'
ohi_type     = 'global'  # ohi_type = 'HS'    ohi_type = 'AQ'
  ### Determines proportional area of each cell covered by region polygons.  Returns data frame
  ### of rgn_id, loiczid, csq, and proportional area of loiczid cell covered by the rgn_id region.
  ### * reload: re-extract region IDs to cell IDs from indicated shape file?
  ### * ogr_location: where is the region vector layer information (without layer name)
  ### * rgn_layer:    which vector layer to use (no file extension, e.g. .shp)
  ### * ohi_type:     what type of assessment: global, HS, AQ
  ###
  ### Should not be year-specific, so leave prepped files in SpeciesDiversity/rgns, or change reload to TRUE.
  ### ??? TO DO: compare loiczid regions <-> CenterLong and CenterLat to last year's table, to make sure consistent from year to year.
  ##############################################################################=


    message(sprintf('Reading regions shape file %s - come back in about 4 minutes.\n  %s/%s', rgn_layer, ogr_location, poly_arc_rgn))
    regions        <- poly_arc_rgn
    # slow command... ~ 4 minutes

   # regions <- switch(ohi_type,
                      #global = regions[regions@data$rgn_typ %in% c('eez', 'eez-disputed', 'eez-inland'), ],
                      #HS     = regions[regions@data$rgn_typ %in% c('fao'), ],
                      #AQ     = regions[regions@data$ant_typ %in% c('eez-ccamlr'), ],
                      #regions[regions@data$rgn_typ %in% c('eez', 'eez-disputed', 'eez-inland'), ]) #default to same as global

    raster_file    <- file.path(dir_anx, 'spp_ico/rgns/loiczid_raster')
    loiczid_raster <- get_loiczid_raster(reload = FALSE)

    message('Extracting proportional area of LOICZID cells per region polygon.  Come back in 15-20 minutes.')
    region_prop <- raster::extract(loiczid_raster,  regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text')
    # small = TRUE returns 1 for rgn_id 232, not what we want.
    # slow command... ~15 minutes (even with the small = TRUE)


    ### assign rgn_id and rgn_name identifiers (from `regions`) to region_prop, convert to data.frame

      rgn_id_name <- data.frame(regions@data$rgn_id, regions@data$rgn_name) %>%
        unite(combo, regions.data.rgn_id, regions.data.rgn_name, sep = '_')


    names(region_prop) <- rgn_id_name$combo
    region_prop_df     <- plyr::ldply(region_prop, rbind) # ??? still a plyr function.
    # length(unique(region_prop_df$.id))
    #   WAS: less than 254 due to repeats of Canada and one small region (232: Bosnia/Herzegovina) with no rasters identified
    #   IS:  278, including a cell for #232.
    region_prop_df <- region_prop_df %>%
      separate(.id, c('rgn_id', 'rgn_name'), sep = '_') %>%
      rename(loiczid = value,
             proportionArea = weight)


    ### ??? add in this region -  Bosnia/Herzegovina (BIH), which appears to be too small to catch using this method (<1% of area)
    ### ??? SKIPPING THIS FOR NOW!!!
    # cells_2013[cells_2013$rgn_id==232, ]
    # cells_2013[cells_2013$csq=='1401:227:4', ]
    # am_cells[am_cells$CsquareCode == '1401:227:4', ]
    # 6.034664/2269.83
    # bih <- data.frame(rgn_id=232, LOICZID=68076, proportionArea=0.002658641)
    # region_prop_df <- rbind(region_prop_df, bih)

    file_loc <- file.path(dir_data_am, 'csv/hcaf_truncated.csv')
    message(sprintf('Loading AquaMaps half-degree cell authority file.  Less than 1 minute.\n  %s ', file_loc))
    am_cells <- fread(file_loc, header = TRUE, stringsAsFactors = FALSE) %>%
      as.data.frame() %>%
      dplyr::select(csquarecode, loiczid, cellarea)
    stopifnot(sum(duplicated(am_cells$csq)) == 0)

    message('Joining csq values and cell areas to loiczid values.')
    region_prop_df <- region_prop_df %>%
      left_join(am_cells, by = 'loiczid')

    message(sprintf('Writing loiczid/csq/cell proportions/cell areas by region to: \n  %s', rgn_prop_file))
    write_csv(region_prop_df, 'rgn_prop_file.csv')

####Read in species cell summary in######

summary_by_loiczid<- read_csv('~/github/ohiprep/globalprep/spp_ico/v2016/summary/cell_spp_summary_by_loiczid.csv')
region_prop_df<- read_csv('circle2016/prep/ICO/rgn_prop_file.csv')

region_summary<- summary_by_loiczid %>%
  inner_join(region_prop_df, by= 'loiczid')%>%
  mutate(cell_area_weight_cat     = cellarea * n_cat_spp * proportionArea,
         cell_area_weight_trend   = cellarea * n_tr_spp * proportionArea,
         area_weighted_mean_cat   = weighted_mean_cat   * cell_area_weight_cat,
         area_weighted_mean_trend = (weighted_mean_trend/20) * cell_area_weight_trend) %>%
  arrange(loiczid)

region_sums <- region_summary %>%
  group_by(rgn_id) %>%
  summarize(rgn_mean_cat   = sum(area_weighted_mean_cat)/sum(cell_area_weight_cat),
            rgn_mean_trend = sum(area_weighted_mean_trend, na.rm = TRUE)/sum(cell_area_weight_trend))%>%
  mutate(status = ((1 - rgn_mean_cat) - 0.25) / 0.75)
write_csv(region_sums, 'circle2016/prep/SPP/spp_summary_new.csv')

#####3nm layer
##############################################################################=

extract_cell_id_per_region <- function(reload       = FALSE,
                                       ogr_location = file.path('prep/spatial'),
                                       rgn_layer    = 'poly_arc_3nm',
                                       ohi_type     = 'global')

### Determines proportional area of each cell covered by region polygons.  Returns data frame
### of rgn_id, loiczid, csq, and proportional area of loiczid cell covered by the rgn_id region.
### * reload: re-extract region IDs to cell IDs from indicated shape file?
### * ogr_location: where is the region vector layer information (without layer name)
### * rgn_layer:    which vector layer to use (no file extension, e.g. .shp)
### * ohi_type:     what type of assessment: global, HS, AQ
###
### Should not be year-specific, so leave prepped files in SpeciesDiversity/rgns, or change reload to TRUE.
### ??? TO DO: compare loiczid regions <-> CenterLong and CenterLat to last year's table, to make sure consistent from year to year.
##############################################################################=


message(sprintf('Getting cell ID per %s region based upon region file: %s\n  %s\n', ohi_type, rgn_layer, file.path(ogr_location, rgn_layer)))

message(sprintf('Reading regions shape file %s - come back in about 4 minutes.\n  %s/%s\n', rgn_layer, ogr_location, rgn_layer))
regions        <- readOGR(dsn = ogr_location, layer = rgn_layer)
# slow command... ~ 4 minutes

# regions <- switch(ohi_type,
#global = regions[regions@data$rgn_typ %in% c('eez', 'eez-disputed', 'eez-inland'), ],
#HS     = regions[regions@data$rgn_typ %in% c('fao'), ],
#AQ     = regions[regions@data$ant_typ %in% c('eez-ccamlr'), ],
#regions[regions@data$rgn_typ %in% c('eez', 'eez-disputed', 'eez-inland'), ]) #default to same as global

##############################################################################=

message('Extracting proportional area of LOICZID cells per region polygon.  Come back in 15-20 minutes.')
region_prop <- raster::extract(loiczid_raster,  regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text')
# small = TRUE returns 1 for rgn_id 232, not what we want.
# slow command... ~15 minutes (even with the small = TRUE)


### assign rgn_id and rgn_name identifiers (from `regions`) to region_prop, convert to data.frame

rgn_id_name <- data.frame(regions@data$rgn_id) %>%
  unite(combo, regions.data.rgn_id, sep = '_')


names(region_prop) <- rgn_id_name$combo
region_prop_df     <- plyr::ldply(region_prop, rbind) # ??? still a plyr function.
# length(unique(region_prop_df$.id))
#   WAS: less than 254 due to repeats of Canada and one small region (232: Bosnia/Herzegovina) with no rasters identified
#   IS:  278, including a cell for #232.
region_prop_df <- region_prop_df %>%
  separate(.id, c('rgn_id'), sep = '_') %>%
  rename(loiczid = value,
         proportionArea = weight)


### ??? add in this region -  Bosnia/Herzegovina (BIH), which appears to be too small to catch using this method (<1% of area)
### ??? SKIPPING THIS FOR NOW!!!
# cells_2013[cells_2013$rgn_id==232, ]
# cells_2013[cells_2013$csq=='1401:227:4', ]
# am_cells[am_cells$CsquareCode == '1401:227:4', ]
# 6.034664/2269.83
# bih <- data.frame(rgn_id=232, LOICZID=68076, proportionArea=0.002658641)
# region_prop_df <- rbind(region_prop_df, bih)

file_loc <- file.path(dir_data_am, 'csv/hcaf_truncated.csv')
message(sprintf('Loading AquaMaps half-degree cell authority file.  Less than 1 minute.\n  %s ', file_loc))
am_cells <- fread(file_loc, header = TRUE, stringsAsFactors = FALSE) %>%
  as.data.frame() %>%
  dplyr::select(csquarecode, loiczid, cellarea)
stopifnot(sum(duplicated(am_cells$csq)) == 0)

message('Joining csq values and cell areas to loiczid values.')
region_prop_df <- region_prop_df %>%
  left_join(am_cells, by = 'loiczid')

message(sprintf('Writing loiczid/csq/cell proportions/cell areas by region to: \n  %s', rgn_prop_file))
write_csv(region_prop_df, 'prep/spatial/3nm_prop_file.csv')

#########3nm final

summary_by_loiczid<- read_csv('~/github/ohiprep/globalprep/spp_ico/v2016/summary/cell_spp_summary_by_loiczid.csv')
region_prop_df<- read_csv('prep/spatial/3nm_prop_file.csv')

region_summary<- summary_by_loiczid %>%
  inner_join(region_prop_df, by= 'loiczid')%>%
  mutate(cell_area_weight_cat     = cellarea * n_cat_spp * proportionArea,
         cell_area_weight_trend   = cellarea * n_tr_spp * proportionArea,
         area_weighted_mean_cat   = weighted_mean_cat   * cell_area_weight_cat,
         area_weighted_mean_trend = weighted_mean_trend * cell_area_weight_trend) %>%
  arrange(loiczid)

region_sums <- region_summary %>%
  group_by(rgn_id) %>%
  summarize(rgn_mean_cat   = sum(area_weighted_mean_cat)/sum(cell_area_weight_cat),
            rgn_mean_trend = sum(area_weighted_mean_trend, na.rm = TRUE)/sum(cell_area_weight_trend))%>%
  mutate(status = ((1 - rgn_mean_cat) - 0.25) / 0.75)
write_csv(region_sums, 'prep/SPP/spp_summary_3nm.csv')


#####Working out what species are in each regions#############
iucn_cells_spp<- read_csv('/home/shares/ohi/git-annex/globalprep/spp_ico/v2016/int/iucn_cells_spp.csv')
am_cells_spp<- read_csv('/home/shares/ohi/git-annex/globalprep/spp_ico/v2016/int/am_cells_spp_prob0.csv')
region_prop_df<- read_csv('circle2016/prep/ICO/rgn_prop_file.csv')
sp_summary<- iucn_cells_spp %>%
  left_join(region_prop_df, by = 'loiczid')
sp_summary2<-filter(sp_summary, rgn_id<10)
sp_summary2<- sp_summary2 %>%
  group_by(rgn_id) %>%
  dplyr::select(sciname, iucn_sid, rgn_id, rgn_name) %>%
  ungroup()
sp_summary3<- sp_summary2 %>% distinct()
sp_summary3<- arrange(sp_summary3, rgn_id)


am_sp_summary<- am_cells_spp %>%
  left_join(region_prop_df, by = 'loiczid')
am_summary<-filter(am_sp_summary, rgn_id<10)
am_summary2<- am_summary %>%
  group_by(rgn_id) %>%
  dplyr::select(am_sid, rgn_id, rgn_name) %>%
  ungroup()
am_summary3<- am_summary2 %>% distinct()
am_summary3<- arrange(am_summary3, rgn_id)
am_summary4<- am_summary3 %>%
  left_join(am_namecheck, by='am_sid')
am_summary5<- am_summary4 %>%
  dplyr::select(am_sid, rgn_id, rgn_name, sciname)

am_spp<-read_csv('circle2016/prep/SPP/am_rgn_summary.csv')
iucn_spp<- read_csv('circle2016/prep/SPP/iucn_rgn_summary.csv')
write_csv(sp_summary3, 'circle2016/prep/SPP/iucn_rgn_summary.csv')
all_spp<- read_csv('/home/shares/ohi/git-annex/globalprep/spp_ico/v2016/int/spp_all_cleaned.csv')
all_spp_rgn<- left_join(all_spp, am_spp, by=c('am_sid', 'sciname'))
all_spp2<- left_join(all_spp_rgn, iucn_spp, by=c('iucn_sid', 'sciname', 'rgn_id', 'rgn_name'))
all_spp3<- all_spp2 %>% drop_na(rgn_id)
write_csv(all_spp3, file.path('circle2016/prep/ICO/spp_summary_final.csv'))
