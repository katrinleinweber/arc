#####3nm###
source('~/github/ohiprep/src/R/common.R')
goal     <- 'lsp'
scenario <- 'v2016'
dir_anx       <- file.path(dir_M, 'git-annex/globalprep')
dir_goal      <- file.path('~/github/ohiprep/globalprep', goal, scenario)
dir_goal_anx  <- file.path(dir_anx, goal, scenario)
dir_data_wdpa <- file.path(dir_anx, '_raw_data/wdpa_mpa', 'd2016')
rast_wdpa_file <- file.path(dir_goal_anx, 'int/wdpa_designated_mol.tif')
rast_wdpa <- raster::raster(rast_wdpa_file)
poly_arc_rgn<- loma

spatial_dir2<- '/home/shares/ohi/git-annex/globalprep/spatial/d2014/data'
eez<- 'regions_gcs'
global_eez<- readOGR(dsn=spatial_dir2, layer=eez, stringsAsFactors = FALSE)
world.map <- global_buffer[global_eez$rgn_name %in% c("Jan Mayen", "Norway", "Canada", "United States", "Russia", "Greenland"),]
p4s_arc<- CRS('+proj=laea +lat_0=90 +lon_0=-150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0') #create p4s of arc map
world.map<- spTransform(world.map, p4s_arc)
loma_shp<- raster::intersect(world.map, loma)
#need to gbuffer width 0 to make one shape - rerun to get global_eez - this was breaking everything.
rast_loma<- rasterize(loma_shp, rast_base2)

rast_3nm_arc<- file.path('circle2016/prep/spatial/rast_loma_3nm.tif')
rast_3nm_arc<- raster::raster(rast_3nm_arc)
raster::plot(rast_3nm_arc)

rast_wdpa_proj<- projectRaster(rast_wdpa, crs = p4s_arc)

ext2 <- extent(poly_arc_rgn); ext2
ext3 <- poly_arc_rgn@bbox; ext3
ext2@xmin <- round(ext2@xmin - 5000, -4); ext2@ymin <- round(ext2@ymin - 5000, -4) #what does the -4 mean?
### expand the extents out to round 10 km edges
ext2@xmax <- round(ext2@xmax + 5000, -4); ext2@ymax <- round(ext2@ymax + 5000, -4)

reso <- 500 ### BC Albers uses meters as units, set resolution to a 0.5-km grid
xcol <- (ext2@xmax - ext2@xmin)/reso ### determine # of columns from extents and resolution
yrow <- (ext2@ymax - ext2@ymin)/reso ### determine # of rows
rast_base2 <- raster(ext2, yrow, xcol, crs = p4s_arc)

rast_base2 ### inspect it: resolution and extents are nice and clean

 #rasterize EEZs with theo rasterbase


#
rast_wdpa_proj<- projectRaster(rast_wdpa, rast_loma, method='ngb') #reproject wdpa into arc CRS
#loma_shp<- gBuffer(loma_shp, byid=FALSE, width=0)
wdpa_by_rgn <- raster::extract(rast_wdpa_proj, loma_shp, weights = FALSE, progress='text')
loma@data$rgn_id<- 1
names(wdpa_by_rgn) <- loma@data$rgn_id


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
              dplyr::select(rgn_id),
            by = 'rgn_id')

knitr::kable(prot_area_df)
### Cross tabulate OHI/EEZ rasters. This givees number of protected cells with year of protection within each region. NA = unprotected cells
rast_wdpa_proj<- crop(rast_wdpa_proj, loma)
rast_df <- raster::crosstab(rast_wdpa_proj, rast_loma, useNA = TRUE, progress = 'text') %>%
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


#####3nm###

rast_wdpa_file <- file.path(dir_goal_anx, 'int/wdpa_designated_mol.tif')
rast_wdpa <- raster::raster(rast_wdpa_file)

rast_3nm_arc<- file.path('prep/spatial/rast_arc_3km.tif')
rast_3nm_arc<- raster::raster(rast_3nm_arc)
raster::plot(rast_3nm_arc)

#rast_wdpa_proj<- projectRaster(rast_wdpa, rast_3nm_arc, method= 'ngb') #reproject wdpa into arc CRS
#writeRaster(rast_wdpa_proj, filename="rast_wdpa_laea.tif")

spatial_dir<- 'prep/spatial'
layer_arc3<- 'poly_arc_3nm'
poly_arc_3nm<- readOGR(dsn= spatial_dir, layer = layer_arc3, stringsAsFactors = FALSE)

wdpa_by_rgn <- raster::extract(rast_wdpa_proj, poly_arc_3nm, weights = FALSE, progress='text')
names(wdpa_by_rgn) <- poly_arc_3nm@data$rgn_id
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
  left_join(poly_arc_3nm@data %>%
              dplyr::select(rgn_id),
            by = 'rgn_id')

knitr::kable(prot_area_df)
### Cross tabulate OHI/EEZ rasters. This givees number of protected cells with year of protection within each region. NA = unprotected cells

rast_df <- raster::crosstab(rast_wdpa_proj, rast_3nm_arc, useNA = TRUE, progress = 'text') %>%
  as.data.frame() %>%
  setNames(c('year', 'rgn_id', 'n_cells')) %>%
  mutate(year   = as.integer(as.character(year)),
         rgn_id = as.integer(as.character(rgn_id))) %>%
  arrange(rgn_id, year)

#### calculate protected area total by region####
lsp_thresh<- 0.30

prot_3nm <- rast_df %>%
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
write_csv(prot_3nm, file.path('~/github/arc/circle2016/prep/spatial/area_protected_3nm.csv'))

##combine
prot_eez<- read.csv('prep/spatial/area_protected_eez.csv')
prot_3nm<- read.csv('prep/spatial/area_protected_3nm.csv')

prot_df <- prot_3nm %>%
  dplyr::select(rgn_id, year,
                lsp_st_3nm = lsp_status,
                a_prot_3nm = a_prot_km2,
                a_tot_3nm  = a_tot_km2) %>%
  full_join(prot_eez %>%
              dplyr::select(rgn_id, year,
                            lsp_st_eez = lsp_status,
                            a_prot_eez = a_prot_km2,
                            a_tot_eez  = a_tot_km2),
            by = c('rgn_id', 'year')) %>%
  mutate(lsp_st_3nm = ifelse(is.na(lsp_st_3nm), 0, lsp_st_3nm),
         lsp_st_eez = ifelse(is.na(lsp_st_eez), 0, lsp_st_eez))%>%
  lsp_status = lsp_st_3nm %>%
  distinct()
write_csv(prot_df, file.path('prep/LSP/area_protected_total.csv'))

prot_df_recent <- prot_df %>%
  filter(year >= 2000)

#set file paths to write.csv to
a_prot_offshore_file <- file.path('prep/LSP/lsp_protected_offshore3nm.csv')
a_prot_eez_file      <- file.path('prep/LSP/lsp_protected_eez.csv')
a_tot_offshore_file  <- file.path('prep/LSP/lsp_a_total_offshore3nm.csv')
a_tot_eez_file       <- file.path('prep/LSP/lsp_a_total_eez.csv')


write_csv(prot_df_recent %>% dplyr::select(rgn_id, year, a_prot_3nm), a_prot_offshore_file)
write_csv(prot_df_recent %>% dplyr::select(rgn_id, year, a_tot_3nm), a_tot_offshore_file)

write_csv(prot_df_recent %>% dplyr::select(rgn_id, year, a_prot_eez), a_prot_eez_file)
write_csv(prot_df_recent %>% dplyr::select(rgn_id, year, a_tot_eez), a_tot_eez_file)
