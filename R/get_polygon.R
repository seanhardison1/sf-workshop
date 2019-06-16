#Returns a polygon of the FCWA 

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

get_polygon <- function(fname , save = F){
  #get region for clipping
  kmz <- maptools::getKMLcoordinates(textConnection(system(paste0("unzip -p data/",fname,".kmz"), intern = TRUE)))
  kmz <- data.frame(lat =kmz[[1]][,1],
                     lon = kmz[[1]][,2])
  
  p = Polygon(kmz)
  ps = Polygons(list(p),1)
  sp_kmz = SpatialPolygons(list(ps))
  
  sf_kmz <- as(SP_kmz, "sf") 
  
  #rotate 180 degrees
  sf_poly_geometry <- st_geometry(sf_kmz)
  cntrd = st_centroid(sf_poly_geometry)
  sf_poly_rotated = (sf_poly_geometry - cntrd) * rot(pi*2) + cntrd
  sf_poly_out <- sf_poly_rotated %>% st_cast("POLYGON")
  st_crs(sf_poly_out) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  
  return(sf_poly_out)
}
