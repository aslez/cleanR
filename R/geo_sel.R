geo_sel <- function(shp, year, states) {
  nproj <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96
              +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=us-mi +no_defs'
  xwlk <- subset(ctyXwalk, YEAR = year)
  ymd <- paste0(year, 1031)
  shp_sub1 <- shp[which(shp$STATE_TERR %in% states), ]
  shp_sub2 <- shp_sub1[which(shp_sub1$START_N <= ymd & shp_sub1$END_N >= ymd), ]
  shp_sub2$FIPS <- xwlk$FIPS[match(shp_sub2$ID_NUM, xwlk$ID_NUM)]
  sp::spTransform(shp_sub2, sp::CRS(nproj))
}
