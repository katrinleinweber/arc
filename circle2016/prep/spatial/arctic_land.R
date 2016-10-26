## Getting Arctic Land for Mike

# Jamie Afflerbach

dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

library(rgdal)
library(dplyr)

laeaCRS <- CRS("+init=epsg:3572")

global_shp <- readOGR(dsn = file.path(dir_M,'git-annex/globalprep/spatial/d2014/data'), layer = 'regions_gcs')

arctic_land <- global_shp%>%
                subset(.@data$rgn_nam %in% c("Russia","Canada","Norway", "United States", "Jan Mayen", "Greenland"))%>%
                subset(.@data$rgn_typ %in% c("land", "land-disputed"))

arctic_land_laea <- spTransform(arctic_land, laeaCRS)

plot(arctic_land_laea)

writeOGR(arctic_land_laea,dsn = "arc/spatial",layer = "arctic_land", driver = "ESRI Shapefile", overwrite=T)
