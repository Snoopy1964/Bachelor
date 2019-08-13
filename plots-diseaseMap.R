#
# Disease-Maps
#
# ToDo:
# - Regionen mit rot füllen, je nach relativer Häufigkeit 
#   ggplot2::cut_interval() genauer anschauen (https://gis.stackexchange.com/questions/178597/how-to-create-a-continuous-scale-with-distinct-custom-color-and-value-breaks-wit)
#
#
#

source('97 - loadLocalMaps.R')

#----------------------------------------------------------------------
# set some defaults
#----------------------------------------------------------------------
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))

#-------------------------------------------------------
# Desease Map's
# - A09
# - B01
# - B02
# - J11
# - A16
#-------------------------------------------------------

# A09 - Gastroenteritis
# Disease Map für Regionen
ds.tmp <- ts.region %>% 
  dplyr::filter(Code.ID == "K52")                  %>%
  mutate(
    Nr.Cases = Nr.Cases.Pax + Nr.Cases.Crew, 
    relFreq = Nr.Cases/((Pd.Crew+Pd.Pax)/Nr.Days)) %>%
  left_join(Regions, by="Region")

ggmap(map.Welt) + 
  geom_polygon(data = regions, aes(long, lat, group = group, fill = region), alpha = 0.5) +
  # scale_fill_brewer(palette = "Reds") +
  geom_point(data    = ds.tmp,
             mapping = aes(x    =lng, 
                           y    =lat, 
                           size = relFreq),
             alpha=4/8
  )  + 
  geom_text_repel(
    data = ds.tmp,
    mapping = aes(x = lng, y = lat, label = sprintf("%.4f", relFreq)),
    nudge_y = 1,
    nudge_x = 10,
    size = 3
  ) +
  ggtitle(str_c("Disease Map ","Gastroenteritis (A09)")) + 
  theme(#legend.title     = element_text("ICD10 Code"),
    legend.position  = "right",
    legend.direction = "vertical")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\diseaseMap.A09.region.all.png", 
       width = 297, height =210, units = "mm" )


# A16, B01, B02, J11
# Disease Map für Regionen
ds.tmp <- ts.region %>% 
  dplyr::filter(Code.ID %in% c("A16", "B01", "B02", "J11")) %>%
  mutate(
    Nr.Cases = Nr.Cases.Pax + Nr.Cases.Crew, 
    relFreq = Nr.Cases/((Pd.Crew+Pd.Pax)/Nr.Days))          %>%
  left_join(Regions, by="Region")


ggmap(map.Welt) + 
  geom_polygon(data = regions, aes(long, lat, group = group, fill = region), alpha = 0.5) +
  geom_point(data=ds.tmp,
             mapping=aes(x    =lng, 
                         y    =lat, 
                         size = relFreq),
             alpha = 4/8
  )   + 
  facet_wrap( ~ Code.ID )                    +
  geom_text_repel(
    data = ds.tmp %>% dplyr::filter(relFreq != 0),
    mapping = aes(x = lng, y = lat, label = sprintf("%.4f", relFreq)),
    nudge_y = 2,
    nudge_x = 20,
    size = 3
  ) +
  ggtitle("Disease Map Infektionskrankheiten II", subtitle =  "aggregiert 2015-2017") + 
  theme(#legend.title     = "ICD10 Code",
    legend.position  = "right",
    legend.direction = "vertical")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\diseaseMap.A16+B01+B02+J11.region.all.png", 
       width = 297, height =210, units = "mm" )

