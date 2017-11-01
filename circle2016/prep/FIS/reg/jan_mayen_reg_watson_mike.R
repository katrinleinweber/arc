# using the Reg Watson data 

# set the mazu data_edit share based on operating system
dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

library(sf) #install.packages("sf")
library(dplyr)
library(seaaroundus)

#change the region shapefile to your location

rgns <- st_read(file.path(dir_M, "git-annex/clip-n-ship/arc/spatial"), "arctic_eezs") %>%
  st_transform(crs = 4326)

#grab just the jan mayen polygon
jm_poly <- rgns %>%
  filter(rgn_name == "Jan Mayen")

#grab the geometry for that polygon (this is used to select hte half degree cells that overlap the polygon)
geom <- st_as_text(jm_poly$geometry[[1]])

# get the actual cell id numbers for jan mayen
jm_cells <- getcells(geom)

#read in raw data fro 2014 (from Mazu)

#read in raw data for the year
raw <- readRDS(paste0(file.path(dir_M,'marine_threats/impact_acceleration/stressors/comm_fish/int/catch_data_'),yr,'.rds'))

#filter raw data for data records only in JM cells
jan_m_reg <- raw %>% 
  filter(Seq %in% jm_cells)
head(jan_m_reg)
#look at unique species
unique(jan_m_reg$CommonName)
unique(jan_m_reg$TaxonName)

#get total catch per cell by adding up large scale, small scale, illegal and discards catch. multiply by ocean area since
#catch rate is in tons/km2 and we want tons/cell
jm_catch <- jan_m_reg %>% 
  rowwise() %>% 
  mutate(catch = sum(LSF_CR, SSF_CR, IUU_CR, Discards_CR)*OceanArea)
head(jm_catch)

#ok that worked, now group by common name and summarize for total catch in region.
jm_catch <- jan_m_reg %>% 
  rowwise() %>% 
  mutate(catch = sum(LSF_CR, SSF_CR, IUU_CR, Discards_CR)*OceanArea) %>% 
  group_by(CommonName) %>% 
  summarize(total_catch = sum(catch))