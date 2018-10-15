######################################################################################
#
# - Load local maps (ggmap format)
#   downloaded with geo_googlemaps() with script "98 - downloadGoogleMaps.R"
# - Read geojson definition of Regions, maually created with http://geojson.io
#
#
####################################################################################

if(!exists("map.Welt"))                  load("maps/map.Welt.Rdata")
if(!exists("map.Mittelmeer"))            load("maps/map.Mittelmeer.Rdata")
if(!exists("map.Orient.Asien"))          load("maps/map.Orient.Asien.Rdata")
if(!exists("map.Mittelamerika.Karibik")) load("maps/map.Mittelamerika.Karibik.Rdata")

# - geojsonio Region definiton from http://geojson.io/#map=4/53.59/-1.54
regions.geojson <- 
  geojson_read("maps/Regions.json", what = "sp")

regions <- regions.geojson %>%
  fortify() %>%
  as.tibble() %>%
  mutate(
    region = ifelse(id == 0, "Nordamerika", NA),
    region = ifelse(id == 1, 
                    "Mittelamerika & Karibik", 
                    ifelse(!is.na(region), region, NA)),
    region = ifelse(id == 2, 
                    "Ostsee & Baltikum", 
                    ifelse(!is.na(region), region, NA)),
    region = ifelse(id == 3, 
                    "Nordland & Großbritanien", 
                    ifelse(!is.na(region), region, NA)),
    region = ifelse(id == 4, 
                    "Westeuropa", 
                    ifelse(!is.na(region), region, NA)),
    region = ifelse(id == 5, 
                    "Südeuropa & Mittelmeer", 
                    ifelse(!is.na(region), region, NA)),
    region = ifelse(id == 6, 
                    "Orient", 
                    ifelse(!is.na(region), region, NA)),
    region = ifelse(id == 7, 
                    "Asien", 
                    ifelse(!is.na(region), region, NA)),
    region = ifelse(id == 8, 
                    "Kanaren", 
                    ifelse(!is.na(region), region, NA))
  )


Regions <- tribble(~Region,                       ~lng, ~lat, 
                   "Nordamerika",              -69.6,   39.8, 
                   "Mittelamerika & Karibik",  -74.2,   14.6, 
                   "Ostsee & Baltikum",         18.8,   57.7, 
                   "Nordland & Großbritanien",   0.552, 66.594, 
                   "Westeuropa",                -2.18,  45.37, 
                   "Südeuropa & Mittelmeer",    17.1,   37.3, 
                   "Orient",                    57.9,   17.5, 
                   "Asien",                    104.1,   12.8, 
                   "Kanaren",                  -20.0,   28.5)

