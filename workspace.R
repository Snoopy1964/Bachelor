# adjust codes of missing sub-codes in icd10cm2016 data base -> compare tibble missing.codes
# A099  -> A09
# I1090 -> I10 
# K0888 -> K08
# T14*  -> T14


# tmp.ds <- rbind(ds.2012, ds.2013, ds.2014, ds.2015, ds.2016, ds.2017, ds.2018)
# # convert Schiff from Character to Factor
# tmp.ds[[3]] <- as.factor(tmp.ds[[3]])
# 
# # convert ICD10 to code format of icd.data::icd10cm2016
# tmp.ds <- tmp.ds %>% mutate(ICD10=str_replace(ICD10, "\\.", ""))
# 
# ###tmp.ds <- left_join(tmp.ds, dplyr::filter(icd10.cm2016, str_length(code)==3), by=c("code" = "three_digit"))
# tmp.ds <- left_join(tmp.ds, icd10cm2016[,c("code","three_digit", "major", "sub_chapter", "chapter")], by=c("ICD10" = "code"))
# 
tmp.ds <- ds


ds.na.code    <- dplyr::filter(tmp.ds, is.na(code) )
missing.codes <- count(ds.na.code, ICD10)
ggplot(dplyr::filter(missing.codes, n >= 1000)) + geom_point(mapping=aes(x=ICD10, y=n))

###### joining the data right!

ICD10.codes3 <- as.tibble(dplyr::filter(icd10cm2016[, c("code", "major", "sub_chapter", "chapter")], str_length(code)==3))

View(left_join(tmp.ds, ICD10.codes3, by=c("ICD10.code" = "code")))

ds.tmp <- left_join(tmp.ds, dplyr::filter(icd10cm2016[, c("code", "major", "sub_chapter", "chapter")], str_length(code)==3), by=c("ICD10.code" = "code"))

ds.na.code    <- dplyr::filter(ds.tmp, is.na(major) )
missing.codes <- count(ds.na.code, ICD10)
ggplot(dplyr::filter(missing.codes, n >= 10)) + geom_point(mapping=aes(x=ICD10, y=n))

#############################################################################
# retrieve Route from Tours-tibble by analyzing "Day number"
# - actual version in file extractRoutes.R
#----------------------------------------------------------------------------
Tours.tmp <- Tours %>% dplyr::filter(`Turn or Call` == "T" )
Itinerary.attributes <- c(1,2,3,6,9,11)
Itinerary <- as.tibble(cbind(Tours.tmp[1,Itinerary.attributes], Port.end = Tours.tmp[[1,"Port Name"]], Tour.start = Tours.tmp$Datum[1], Tour.end=Tours.tmp$Datum[1]))
for (i in 2:length(Tours.tmp$Datum)) {
  Itinerary$Tour.end[i-1] <- Tours.tmp$Datum[i]
  Itinerary$Port.end[i-1] <- Tours.tmp[[i,"Port Name"]]
  Itinerary <- rbind(Itinerary, as.tibble(cbind(Tours.tmp[i,Itinerary.attributes], Port.end = Tours.tmp[[i,"Port Name"]],Tour.start = Tours.tmp$Datum[i], Tour.end = Tours.tmp$Datum[i])))
}


#----------------------------------------------------------------------------
# end of retrieve Tour from Tours-tibble by analyzing "Day number"
#############################################################################

#############################################################################
# get (long,lat) for PortNames
#----------------------------------------------------------------------------

ports.tmp <- as_tibble(GNsearch(q=PortNames.tmp[[1]], maxRows=1)[,c(-1,-11)])
for(i in 2:length(PortNames)){
# for(i in 2:10){
  tmp <- GNsearch(q=PortNames[[i]], maxRows=1)
  if( ncol(tmp) == 16) {
    print(sprintf("row: %i, ncol: %i, %s", i, ncol(tmp), PortNames[[i]]))
    ports.tmp <- rbind(ports.tmp, tmp[,c(-1,-11)])
  } else if(ncol(tmp) == 15) {
    print(sprintf("row: %i, ncol: %i, %s", i, ncol(tmp), PortNames[[i]]))
    ports.tmp <- rbind(ports.tmp, tmp[,c(-1)])
  } else if(ncol(tmp) == 14) {
    print(sprintf("row: %i, ncol: %i, %s", i, ncol(tmp), PortNames[[i]]))
    ports.tmp <- rbind(ports.tmp, tmp)
  } else {
    print(sprintf("row: %i, ncol: %i, %s", i, ncol(tmp), PortNames[[i]]))
  }
}
                         
#----------------------------------------------------------------------------
# end of get (long,lat) for PortNames
#############################################################################

#############################################################################
# Plot Ports on map
#---------------------------------------------------------------------------
ds.port.inc <- ds.loc.infect %>% group_by(`Port Name`) %>% summarise(Nr=n()) %>% left_join(Ports,by="Port Name")

ggplot() + 
  geom_polygon(data=map_data("world"), 
               aes(x=long, y=lat, group=group), fill="grey80") + 
  geom_point(data=ds.port.inc,
             aes(x=lng, y=lat, size = Nr ), alpha=1/3) +
  guides(size=guide_legend("Anzahl Infektionen")) + 
  scale_size_continuous(breaks=c(10,100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), range = c(1, 10)) +
  xlim(-150,120) +
  ylim(-20,80)
# same plot with ggmap and google map
world.map <- get_googlemap(center=c(lon=10,lat=52), 
                          zoom=2, 
                          size=c(640,410), 
                          key="", 
                          scale = 2, 
                          language = "de-DE")
ggmap(world.map) + 
  geom_point(data=ds.port.inc,
             aes(x=lng, y=lat, size = Nr ), colour="red", alpha=1/3) +
  guides(size=guide_legend("Anzahl Infektionen")) + 
  scale_size_continuous(breaks=c(10,100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), range = c(1, 10))

# mit density plot
ggmap(world.map) + 
  geom_point(data=ds.port.inc,
             aes(x=lng, y=lat), colour="red", alpha=1/3) +
  stat_density2d(aes(x = lng, y = lat,
                   fill = ..level.., alpha = ..level..),
               size = 1, bins = 10,
               data = select(ds.port.inc, lng, lat, Nr),
               contour = TRUE)
  

#--------------------------------------------
# Schiff in different colours
ds.port.MS.inc <- ds.loc.infect %>% group_by(`Port Name`,Schiff) %>% summarise(Nr=n()) %>% left_join(Ports,by="Port Name")

ggplot() + 
  geom_polygon(data=map_data("world"), 
               aes(x=long, y=lat, group=group), fill="grey80") + 
  geom_point(data=ds.port.MS.inc,
             aes(x=lng, y=lat, size = Nr, colour=Schiff ), alpha=1/3) +
  xlim(-150,120) +
  ylim(-20,80) + 
  guides(size=guide_legend("Anzahl Infektionen"), colour=guide_legend("Schiffe"))

#-------------------------------------------
# some more experiments
ggmap(map.Europe) +
  geom_point(data=ds.port.MS.inc,
             aes(x=lng, y=lat, size = Nr, colour=Schiff ), alpha=1/2) +
  guides(size=guide_legend("Anzahl Infektionen"), colour=guide_legend("Schiffe")) + 
  scale_size_continuous(range = c(5, 10))


#----------------------------------------------------------------------------
# end of Plot Ports on map
#############################################################################
map.Europe <- get_map(location = c(lon=0, lat=50), zoom = 4, maptype="terrain")
ggmap(map.Europe) + geom_point(data=Ports, aes(x=lng, y=lat, size=population)) + xlim(-30,30) + ylim(20,70)

# new stuff will be added here:-)

#
# some interessting stuff concening geo mapping 
# from https://stackoverflow.com/questions/11201997/world-map-with-ggmap
#
# bigmap definition (file )
# Map is 11x8 tiles (2816x2048 px) at zoom 4, aspect 1.4:1
# Bbox is -112.50, -40.98, 135.00, 79.17 (l,b,r,t)
zoom <- 2
map.world <- readPNG("data/bigmap/map-world-high.png")
map.world <- as.raster(apply(map.world, zoom, rgb))# cut map to what I really need
pxymin <- LonLat2XY (-112.50,79,17,zoom+8)$Y # zoom + 8 gives pixels in the big map
pxymax <- LonLat2XY (135.00,-40.98,zoom+8)$Y # this may or may not work with google
map.world <- map.world[pxymin : pxymax,]
# set bounding box
# attr(map.world, "bb") <- data.frame(ll.lat = XY2LonLat(0, pxymax + 1, zoom+8)$lat,
#                                     ll.lon = -40.98,
#                                     ur.lat = round (XY2LonLat (0, pxymin, zoom+9)$lat),
#                                     ur.lon = 79.17)
attr(map.world, "bb") <- data.frame(ll.lat = XY2LonLat(0, pxymax + 1, zoom+8)$lat,
                                    ll.lon = -40.98,
                                    ur.lat = round (XY2LonLat (0, pxymin, zoom+9)$lat),
                                    ur.lon = 180)
class(map.world) <- c("ggmap", "raster")
ggmap(map.world)





