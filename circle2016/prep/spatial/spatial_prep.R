### Sorting Shape Files ####
##Read in Shape file map of arctic
spatial_dir<- 'circle2016/prep/spatial'
layer_arc<- 'arctic_eezs'
poly_arc_rgn<- readOGR(dsn= spatial_dir, layer = layer_arc, stringsAsFactors = FALSE)
poly_arc_ebsa<- readOGR(dsn= spatial_dir, layer = 'EBSA_0511_JC_area_sort') # read in ebsa file
p4s_arc<- CRS('+proj=laea +lat_0=90 +lon_0=-150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0') #create p4s of arc map
poly_arc_ebsa<- spTransform(poly_arc_ebsa, p4s_arc) #reproject ebsa map to same as arctic
