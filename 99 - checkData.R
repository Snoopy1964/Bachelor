##############################################################################
#
# check data set ds.loc
# ("00 - readData.R" muss ausgeführt worden sein) -> ds.loc
#
# (1) Checke Zeilen auf nicht gültige (NA) Einträge 
#
#
# ToDo's (beim Anschauen der Daten gefunden):
# - Checks für
#   - Region ist NA
#     Tour MS3-2018-01 ist in ds.loc auf MS4 gemappt!!!!!!!
#
#
##############################################################################

# (1) Checke Zeilen auf nicht gültige (NA) Einträge 
View(ds.loc.na <- ds.loc[!complete.cases(ds.loc),])

cat("\n\nkeine Klassifizierung nach ICD10-WHO möglich, aber nach ICD10-GM teilweise!\n")
print(
  ds.all %>% group_by(Code.ID, Code.Titel) %>% dplyr::filter(is.na(Gruppen.ID)) %>% summarize("Anzahl NAs" = n())
)

# (2) checke Regionen durch plots der Häfen mit Selektion auf Region
Regions <- unique(ds.loc[["Region"]])

# temp -> ToDo
Regions <- Regions[!is.na(Regions)]

# ggds <- ggplot(
#   ds.loc %>% 
#     select(`Port Name`, geoName, lng, lat, Region) %>%
#     dplyr::filter(Region == Regions[1])
# )

for (r in Regions) {
  print(r)
  print(
    ggmap(world.map) +
      geom_point(
        data = ds.loc %>% dplyr::filter(Region == r),
        aes(x = lng, y = lat),
        colour = "red",
        alpha = 1 / 3
      ) +
      ggtitle(r)
  )
}
