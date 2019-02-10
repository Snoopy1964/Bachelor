#############################################################################
# Plot Ports on map
#---------------------------------------------------------------------------
ds.port.inc <- ds.infect.chapters %>% group_by(`Port Name`) %>% summarise(Nr=n()) %>% left_join(Ports,by="Port Name")

# ggplot() + 
#   geom_polygon(data=map_data("world"), 
#                aes(x=long, y=lat, group=group), fill="grey80") + 
#   geom_point(data=ds.port.inc,
#              aes(x=lng, y=lat, size = Nr ), alpha=1/3) +
#   guides(size=guide_legend("Anzahl Infektionen")) + 
#   scale_size_continuous(breaks=c(10,100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), range = c(1, 10)) +
#   xlim(-150,120) +
#   ylim(-20,80)
# # same plot with ggmap and google map
world.map <- get_googlemap(
  center = c(lon = 15, lat = 52),
  zoom = 2,
  size = c(640, 410),
  key = "AIzaSyDONI14MPhXkFu_j3J5UiC7TMIuSOeAiB8",
  scale = 2,
  language = "de-DE"
)
# style_str <- str_c(
#   "element:labels%7Cvisibility:off&",
#   "style=feature:administrative.land_parcel%7Cvisibility:off&",
#   "style=feature:administrative.neighborhood%7Cvisibility:off&",
#   "style=feature:poi%7Celement:labels.text%7Cvisibility:off&",
#   "style=feature:poi.business%7Cvisibility:off&",
#   "style=feature:road%7Celement:labels.icon%7Cvisibility:off&",
#   "style=feature:road.arterial%7Cvisibility:off&",
#   "style=feature:road.highway%7Celement:labels%7Cvisibility:off&",
#   "style=feature:road.local%7Cvisibility:off&",
#   "style=feature:transit%7Cvisibility:off&",
#   "style=feature:water%7Celement:labels%7Cvisibility:on"
# )
# world.map <- get_googlemap(
#   center = c(lon = 15, lat = 52),
#   zoom = 2,
#   size = c(640, 410),
#   key = "AIzaSyDONI14MPhXkFu_j3J5UiC7TMIuSOeAiB8",
#   scale = 2,
#   language = "de-DE"
# )
# Mittelmeer.map <-
#   get_googlemap(
#     center = c(lon = 15, lat = 42),
#     zoom = 4,
#     size = c(640, 410),
#     key = "AIzaSyDONI14MPhXkFu_j3J5UiC7TMIuSOeAiB8",
#     scale = 2,
#     style = style_str,
#     language = "de-DE"
#   )
# ggmap(Mittelmeer.map) + 
#   geom_point(data=Ports,
#              aes(x=lng, y=lat, color="blue")
#   )
# 


ggmap(world.map) + 
  geom_point(data=ds.loc,
             aes(x=lng, y=lat), colour="red", alpha=1/3)

ggmap(world.map) + 
  geom_point(data=ds.loc,
             aes(x=lng, y=lat, size = Nr ), colour="red", alpha=1/3) +
  guides(size=guide_legend("Anzahl Infektionen")) + 
  scale_size_continuous(breaks=c(10,100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), range = c(1, 10))

# mit density plot
ggmap(map.Welt) + 
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

########################################################
# Desease Map's
#-------------------------------------------------------

# A09 - Gastroenteritis
ds.port.infect <- ds %>% 
  group_by(`Port Name`,lng,lat) %>% 
  dplyr::filter(Code.ID == "A09") %>%
  summarize(Anzahl = n())

ggmap(world.map) + 
  geom_point(data=ds.port.infect,
             mapping=aes(x    =lng, 
                         y    =lat, 
                         size = Anzahl)
  )


#-------------------------------------------------------
# End of Desease Map's
########################################################
# TimeSeries Experiments
#-------------------------------------------------------

ds.MS1 %>% 
  group_by(Code.ID, Code.Titel, Year, Month) %>%
  # summarize(nr_cases = n())   %>%
  # ggplot(aes(x = Month, y = nr_cases, colour = Code.Titel)) +
  ggplot(aes(x = Week, colour = Code.Titel)) +
  # geom_freqpoly(stat = 'count', alpha = 1, binwidth = 7) +
  geom_histogram(stat = 'count', alpha = 1, binwidth = 1) +
  facet_wrap(~Code.Titel, ncol = 1) +
  theme(legend.position="top",
        legend.direction="vertical")

ggds

ggplot(ds.infect.codes.high, aes(x = Month, colour = Code.Titel)) +
  geom_freqpoly(position = 'identity',
                alpha = 1,
                binwidth = 1) +
  # facet_wrap( ~ Code.Titel, ncol = 1) +
  facet_grid(  Schiff ~ Code.ID) +
  theme(legend.position = "top",
        legend.direction = "vertical")

ds <- ds.MS1 %>% 
  group_by(Week, Code.ID) %>%
  summarize(nr = n())


ds %>%
  ggplot(aes(x=Week, y=nr)) +
  geom_line()                        +
  facet_wrap( ~ Code.ID, ncol = 1)
  


########################################################
# just test anything
########################################################

x.Axis.Label <- ifelse(str_length(icd10.chapters[["Kapitel.Titel"]]) >= 57, 
                       str_c(str_sub(icd10.chapters[["Kapitel.Titel"]], 1, 57), "...", sep=""), 
                       str_c(icd10.chapters[["Kapitel.Titel"]]))
ggds.loc + 
  geom_col(mapping=aes(x=reorder(Kapitel.Titel, Anzahl), y=Anzahl)) + 
  coord_flip() + 
  scale_x_discrete(breaks = x.Axis, labels=x.Axis.Label)


# Fehler mit codierung M/W
graph <- ggplot(ds.loc, aes(x = Datum)) +
  geom_histogram(position = 'identity', alpha = 0.8, binwidth = 7) +
  facet_grid( Schiff ~ Geschlecht ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13)
  )
graph


#######################################################
# Lon/Lat Mittelmeer boundery

# ggmap(Mittelmeer.map) +
#   geom_polygon(
#     data = Mittelmeer.region,
#     mapping = aes(x = lng, y = lat),
#     color = "red",
#     alpha = 1 / 5
#   ) +
#   geom_point(data = Ports.Mittelmeer,
#              mapping = aes(x = lng, y = lat)) +
#   geom_label_repel(
#     data = Ports.Mittelmeer,
#     mapping = aes(x = lng, y = lat, label = geoName),
#     size = 2
#   )


                                              

ggmap(map.Welt) + 
  geom_polygon(aes(long, lat, group = group, fill = region), data = regions, alpha = 0.5) +
  geom_point(aes(lng, lat), data = Ports, color = "red") +
  theme(#legend.title     = "ICD10 Code",
    legend.position  = "bottom",
    legend.direction = "vertical")


# check read parameters
regions <- 
  geojson_read("maps/Regions.json", parse=TRUE, what = "sp")

# Punkt: 27.94921875, 78.76779175784321


#####################################################################
#
# Relative Häufigkeiten
#
#####################################################################
ds.A09.region.nr <- ds.infect.codes.region.nr       %>% 
  dplyr::filter(Code.ID=="A09") %>% 
  arrange(desc(Anzahl))    

write_delim(ds.A09.region.nr, "data/Results/ds.A09.region-aggrgiert.csv", delim=";")


ggplot(ds.A09.region.nr) + 
  geom_bar(aes(x=reorder(Region, Anzahl), y=Anzahl), stat="identity") + 
  coord_flip()


ds.A09.ship.nr <- ds.infect.codes.ship.nr       %>% 
  dplyr::filter(Code.ID=="A09") %>% 
  arrange(desc(Anzahl))    
write_delim(ds.A09.ship.nr, "data/Results/ds.A09.ship-aggrgiert.csv", delim=";")


ggplot(ds.A09.ship.nr) + 
  geom_bar(aes(x=Schiff, y=relFreq), stat="identity") + 
  coord_flip()

ds.tmp <- ds.infect.codes.ship.region %>% 
  dplyr::filter(Code.ID == "A09")     %>% 
  group_by(Region,Schiff)             %>% 
  summarize(Anzahl = sum(Anzahl), PaxNr = mean(PaxNr), CrewNr=mean(CrewNr)) %>% 
  mutate(relFreq=Anzahl/(PaxNr+CrewNr))

ggplot(ds.tmp, aes(x=Schiff, y=Region, size=relFreq)) + geom_point(color="blue", alpha=1/3)

################################################
#
# nächster Versuch basierend auf ds.infect.codes
#
################################################

ds.A09 <- ds.infect.codes %>%
  dplyr::filter(Code.ID == "A09")







#####################################################################
#
# Algorithm to get Passengers per days
#
#####################################################################

