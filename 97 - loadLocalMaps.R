######################################################################################
#
# Load local maps (ggmap format)
# downloaded with geo_googlemaps() with scirpt "98 - downloadGoogleMaps.R"
#
#
####################################################################################

if(!exists("map.Welt"))                  load("maps/map.Welt.Rdata")
if(!exists("map.Mittelmeer"))            load("maps/map.Mittelmeer.Rdata")
if(!exists("map.Orient.Asien"))          load("maps/map.Orient.Asien.Rdata")
if(!exists("map.Mittelamerika.Karibik")) load("maps/map.Mittelamerika.Karibik.Rdata")
