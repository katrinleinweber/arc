#' PrepSpatial.R
#' This function turns a .shp or .geogjson file into a data frame that can be plotted with ggplot2
#'
#' @param mapfile_path is the local filepath of a .shp or .geojson file
#'
#' @return dataframe of spatial information
#' @export
#'
#' @examples PrepSpatial('mapfile_path = 'spatial/regions_gcs.geojson')


##### temporary (until added to ohicore)
library(maptools) # install.packages('maptools')
library(broom) # install.packages('broom')
library(rgdal) # install.packages('rgdal')

PrepSpatial <- function(mapfile_path = 'subcountry2014/spatial/rgn_offshore_gcs.shp') {

  ## identify spatial file type ----

  ## spatial filetype
  fp = mapfile_path %>% normalizePath()
  fp_sans_ext = fp %>% tools::file_path_sans_ext()
  shp_ext = tools::file_ext(fp)

  ## if shapefile, prepare
  if (shp_ext == 'shp') {

    ## Fortify SpatialPolygonsDataFrames into a data.frame for ggplot
    poly_rgn    <- maptools::readShapePoly(fn = fp_sans_ext)

  ## if geojson, prepare
  } else if (shp_ext == 'geojson'){

    poly_rgn = rgdal::readOGR(dsn = mapfile_path, "OGRGeoJSON")

  } else {
    print('Sorry, only .shp or .geojson files are supported at this time.')

  }

  ## tidy
  poly_rgn_df <- broom::tidy(poly_rgn, region = 'rgn_id') %>% # use broom::tidy() instead of ggplot2::fortify()
    dplyr::rename(region_id = id) %>% # during broom::tidy, 'rgn_id' was renamed to 'id'
    mutate(region_id = as.integer(region_id))

  return(poly_rgn_df)

}
