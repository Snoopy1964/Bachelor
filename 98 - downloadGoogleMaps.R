##########################################################################
#
# Download von google static maps und speicern als .R Bbjecte
#
# - Welt
# - Mittelmeer
# - Mittelamerika
# - Nordamerika
# - Baltische See
# - Orient/Asien
# - ...
#
##########################################################################

# Style String von https://mapstyle.withgoogle.com/
#   Achtung: geo_googlemap() fügt bei ersten element IMMER ein style= ein!
#   Daher muss von der kopierten URL das erste "style=" im sytle string
#   per Hand gelöscht werden.
#
style_str <- str_c(
  "element:labels%7Cvisibility:off&",
  "style=feature:administrative.land_parcel%7Cvisibility:off&",
  "style=feature:administrative.neighborhood%7Cvisibility:off&",
  "style=feature:poi%7Celement:labels.text%7Cvisibility:off&",
  "style=feature:poi.business%7Cvisibility:off&",
  "style=feature:road%7Celement:labels.icon%7Cvisibility:off&",
  "style=feature:road.arterial%7Cvisibility:off&",
  "style=feature:road.highway%7Celement:labels%7Cvisibility:off&",
  "style=feature:road.local%7Cvisibility:off&",
  "style=feature:transit%7Cvisibility:off&",
  "style=feature:water%7Celement:labels%7Cvisibility:on"
)

# Weltkarte für Übersicht
map.Welt <-
  get_googlemap(
    center = c(lon = 15, lat = 52),
    zoom = 2,
    size = c(640, 410),
    key = "AIzaSyDONI14MPhXkFu_j3J5UiC7TMIuSOeAiB8",
    style = style_str,
    scale = 2,
    language = "de-DE"
  )
save(map.Welt, file = "maps/map.Welt.RData")

# Mittelmeer für Südeuropa/Westeuropa
map.Mittelmeer <-
  get_googlemap(
    center = c(lon = 15, lat = 38),
    zoom = 4,
    size = c(640, 280),
    key = "AIzaSyDONI14MPhXkFu_j3J5UiC7TMIuSOeAiB8",
    scale = 2,
    style = style_str,
    language = "de-DE"
  )
save(map.Mittelmeer, file = "maps/map.Mittelmeer.RData")

# Orient/Asien
# center=-10.219302517967389,71.29898122990312

map.Orient.Asien <-
  get_googlemap(
    center = c(lon = 75, lat = 10),
    zoom = 3,
    size = c(640, 300),
    key = "AIzaSyDONI14MPhXkFu_j3J5UiC7TMIuSOeAiB8",
    scale = 2,
    style = style_str,
    language = "de-DE"
  )
save(map.Orient.Asien, file = "maps/map.Orient.Asien.RData")

# Mittelamerika & Karibik
map.Mittelamerika.Karibik <-
  get_googlemap(
    center = c(lon = -75, lat = 15),
    zoom = 4,
    size = c(400,300),
    key = "AIzaSyDONI14MPhXkFu_j3J5UiC7TMIuSOeAiB8",
    scale = 2,
    style = style_str,
    language = "de-DE"
  )
save(map.Mittelamerika.Karibik, file = "maps/map.Mittelamerika.Karibik.RData")








ggmap(map.Mittelmeer) +
  geom_point(data = Ports,
             aes(x = lng, y = lat),
             color = "blue")
