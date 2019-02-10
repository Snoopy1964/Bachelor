##############################################################################
#
# check data set cases
# ("00 - readData.R" muss ausgeführt worden sein) -> cases
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
View(cases.na <- cases[!complete.cases(cases),])

cat("\n\nkeine Klassifizierung nach ICD10-WHO möglich, aber nach ICD10-GM teilweise!\n")
print(
  cases %>% group_by(Code.ID, Code.Titel) %>% dplyr::filter(is.na(Gruppen.ID)) %>% summarize("Anzahl NAs" = n())
)

# (2) checke Regionen durch plots der Häfen mit Selektion auf Region
Regions <- unique(cases[["Region"]])

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
    ggmap(map.Welt) +
      geom_point(
        data = ds.loc %>% dplyr::filter(Region == r),
        aes(x = lng, y = lat),
        colour = "red",
        alpha = 1 / 3
      ) +
      ggtitle(r)
  )
}

#--------------------------------------------------------------------
#-------------------------------------------------------
# Ports - worldwide
#-------------------------------------------------------
ds.dd <- dplyr::filter(Ports.Region, Region == "Asien")
# ggmap(map.Mittelmeer) +
ggmap(map.Welt) +
  geom_point(data = ds.dd,
             mapping = aes(x = lng, y = lat)) +
  geom_text(
    data = Regions,
    mapping = aes(x = lng, y = lat, label = Region),
    size = 4 ) + 
  geom_polygon(aes(long, lat, group = group, fill = region), data = regions, alpha = 1/3) + 
  geom_label_repel(
    data = ds.dd,
    mapping = aes(x = lng, y = lat, label = `Port Name`),
    size = 2
  ) + 
  geom_polygon(aes(long, lat, group = group, fill = region), data = regions, alpha = 1/3) + 
  scale_fill_discrete(name="Region") +
  theme(#legend.title     = element_text("ICD10 Code"),
    legend.position  = "top",
    legend.direction = "horizontal")
