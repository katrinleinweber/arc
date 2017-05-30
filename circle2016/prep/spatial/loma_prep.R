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

rast_3nm_arc<- file.path('circle2016/prep/spatial/rast_loma_3nm.tif')
rast_3nm_arc<- raster::raster(rast_3nm_arc)
raster::plot(rast_3nm_arc)
rast_wdpa_proj<- projectRaster(rast_wdpa, rast_3nm_arc, method= 'ngb')



wdpa_by_rgn <- raster::extract(rast_wdpa_proj, loma_buff, weights = FALSE, progress='text')
loma_buff@data$rgn_id<- 1
names(wdpa_by_rgn) <- loma_buff@data$rgn_id
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
  #group_by(rgn_id) %>%
  summarize(cells_mpa     = sum(!is.na(year)),
            cells_tot     = n(),
            prot_area_pct = round(cells_mpa/cells_tot, 3) * 100) %>%
  left_join(loma_buff@data %>%
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
write_csv(prot_3nm, file.path('~/github/arc/circle2016/prep/spatial/loma_protected_3nm.csv'))

##combine
prot_eez<- read.csv('prep/spatial/area_protected_eez.csv')
prot_3nm<- read.csv('prep/spatial/area_protected_3nm.csv')

prot_df <- prot_3nm %>%
  dplyr::select(rgn_id, year,
                lsp_st_3nm = lsp_status,
                a_prot_3nm = a_prot_km2,
                a_tot_3nm  = a_tot_km2) %>%
  mutate(lsp_st_3nm = ifelse(is.na(lsp_st_3nm), 0, lsp_st_3nm))%>%
  lsp_status = lsp_st_3nm %>%
  distinct()


write_csv(prot_df, file.path('circle2016/prep/LSP/loma_protected_total.csv'))

prot_df_recent <- prot_df %>%
  filter(year >= 2000)

#set file paths to write.csv to
a_prot_offshore_file <- file.path('prep/LSP/loma_protected_offshore3nm.csv')
a_tot_offshore_file  <- file.path('prep/LSP/loma_a_total_offshore3nm.csv')


write_csv(prot_df_recent %>% dplyr::select(rgn_id, year, a_prot_3nm), a_prot_offshore_file)
write_csv(prot_df_recent %>% dplyr::select(rgn_id, year, a_tot_3nm), a_tot_offshore_file)
