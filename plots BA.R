#
# Plots für Bacherarbeit
#
# ToDo:
# - Zentrale Tabelle der Fahrpläne stimmt nicht
#
#
#
#
#
#

source('97 - loadLocalMaps.R')

#----------------------------------------------------------------------
# Statistik
#----------------------------------------------------------------------

# ToDo: !!!!!!!!!!!!!!!!!!!!!! Hier stimmt was nicht!!!!!!!!!!!!!!!!!!!!!
# zentrale Tabelle Fahrplan der Schiffe im gesamten Zeitraum
cat("\n\nTimetable: zentrale Tabelle Fahrplan der Schiffe im gesamten Zeitraum",
      "\n--------------------------------------------------------------------------\n")
print(Timetable        %>% 
      group_by(Schiff, TourNr) %>%
      summarize(min(Datum),max(Datum), NrDays = n()) %>%
      summarize(NrTours = n(), NrDays=sum(NrDays))
        
)

# zentrale Tabelle Fahrplan der Schiffe im Analyse Zeitraum 2015 - 2017
cat("\n\nTimetable: zentrale Tabelle Fahrplan der Schiffe im Analyse Zeitraum 2015 - 2017",
      "\n-------------------------------------------------------------------------------------\n")
print(Timetable                                                   %>% 
      dplyr::filter(Datum >= "2015-01-01" & Datum < "2018-01-01") %>% 
      group_by(Schiff)                                            %>% 
      summarize(min(Datum),max(Datum), NrDays = n())
)



# #----------------------------------------------------------------------
# # Plots
# #----------------------------------------------------------------------
# 
# 
# # Anzahl der Cases (Fälle) gruppiert nach Kapitel
# cases.chapter <- ds.loc %>% group_by(chapter) %>% summarize(case = n()) %>% arrange(desc(case))
# ggcc <- ggplot(cases.chapter, mapping = aes(x=chapter, y=case)) 
# ggcc + geom_point()
# 
# 
# # Anzahl der Cases (Fälle) gruppiert nach ICD10 code
# cases.ICD10.code <- ds.loc %>% group_by(ICD10.code,major) %>% summarize(case = n()) %>% arrange(desc(case))
# 
########################################################
#-------------------------------------------------------
# Ports - worldwide
#-------------------------------------------------------
ggmap(map.Welt) +
  geom_point(data = Ports,
             mapping = aes(x = lng, y = lat)) +
  geom_text(
    data = Regions,
    mapping = aes(x = lng, y = lat, label = Region),
    size = 4 ) + 
  geom_polygon(aes(long, lat, group = group, fill = region), data = regions, alpha = 1/3) + 
  geom_label_repel(
    data = Ports,
    mapping = aes(x = lng, y = lat, label = `Port Name`),
    size = 2
  ) + 
  geom_polygon(aes(long, lat, group = group, fill = region), data = regions, alpha = 1/3) + 
  scale_fill_discrete(name="Region") +
  theme(#legend.title     = element_text("ICD10 Code"),
    legend.position  = "top",
    legend.direction = "horizontal")

#-------------------------------------------------------
# Regions
#-------------------------------------------------------
gg.region <- ggmap(map.Welt) +
  geom_point(data = Ports,
             mapping = aes(x = lng, y = lat), color = "skyblue3") + 
  geom_polygon(aes(long, lat, group = group, fill = region), data = regions, alpha = 1/3) +
  geom_text(
    data = Regions,
    mapping = aes(x = lng, y = lat, label = Region),
    hjust = 0, nudge_x = 5,
    size  = 4 )

ds.tmp <- ds.infect.codes.ship.region %>%
  group_by(Region) %>%
  summarize(
    Anzahl = sum(Anzahl),
    PaxNr  = sum(PaxNr)
  )

gg.region +
  geom_point(data=ds.tmp, mapping=aes(x=lng, y=lat, size=Anzahl), alpha = 1/3) +
  scale_size(range=c(4,10)) +
  theme(legend.position="top")

# ds.tmp <- ds.infect.codes.region.nr %>%
#   group_by(Region) %>%
#   summarize(Anzahl = sum(Anzahl)) %>%
#   left_join(Regions, by="Region")
# 
# gg.region +
#   geom_point(data=ds.tmp, mapping=aes(x=lng, y=lat, size=Anzahl), alpha = 1/3) + 
#   scale_size(range=c(4,10)) +
#   theme(legend.position="top")

#-------------------------------------------------------
# Summary infect cases / total number of (Passengers plus Crew)
#-------------------------------------------------------

ds.infect.codes.ship.nr %>%
  ggplot(aes(x=Code.ID, y=relFreq, fill=Schiff)) +
  geom_bar(stat="identity", position="dodge")

ds.infect.codes.region.nr %>%
  ggplot(aes(x=Code.ID, y=relFreq, fill=Region)) +
  geom_bar(stat="identity", position="dodge")


#-------------------------------------------------------
# Desease Map's
# - A09
# - B01
# - B02
# - J11
# - J00, J06 & J40
# - A16
#-------------------------------------------------------

# A09 - Gastroenteritis
ds.port.infect <- ds.infect.codes %>% 
  group_by(`Port Name`,lng,lat, Code.ID, Code.Titel)  %>% 
  dplyr::filter(Code.ID %in% infect.codes) %>%
  summarize(Anzahl = n())

ggmap(map.Welt) + 
  geom_point(data=ds.port.infect %>% 
               dplyr::filter(Code.ID %in% c("A09", "J00", "J06")),
             mapping=aes(x    =lng, 
                         y    =lat, 
                         size = Anzahl,
                         colour = Code.Titel),
             alpha=1/2
  ) +
  ggtitle(str_c("Disease Map ","Infektionskrankheiten I")) + 
  facet_grid( Code.ID ~ . )                    +
  theme(#legend.title     = element_text("ICD10 Code"),
        legend.position  = "bottom",
        legend.direction = "vertical")

ggmap(map.Welt) + 
  geom_point(data=ds.port.infect %>% 
               dplyr::filter(Code.ID %in% c("A16", "B01", "B02")),
             mapping=aes(x    =lng, 
                         y    =lat, 
                         size = Anzahl,
                         colour = Code.Titel),
             alpha=.75
  ) +
  ggtitle(str_c("Disease Map ","Infektionskrankheiten II")) + 
  facet_grid( Code.ID ~ . )                    +
  theme(#legend.title     = "ICD10 Code",
        legend.position  = "bottom",
        legend.direction = "vertical")

#-------------------------------------------------------
# TimeSeries Analysis
# - A09
# - B01
# - B02
# - J11
# - J00, J06 & J40
# - A16
#-------------------------------------------------------

# Codes with high frequency
ds.infect.codes.high <- ds.infect.codes %>%
  dplyr::filter(Code.ID %in% c("A09", "J00", "J06"))

ggds <- ggplot(ds.infect.codes.high, aes(x = Datum, fill = Code.Titel)) +
 geom_histogram(position = 'identity', alpha = 1, binwidth = 7) +
  facet_grid( Schiff ~ Code.ID ) +
  theme(legend.position="top",
        legend.direction="vertical")
ggds

ggplot(ds.infect.codes.high, aes(x = Month, colour = Code.Titel)) +
  geom_freqpoly(position = 'identity',
                alpha = 1,
                binwidth = 1) +
  facet_grid(  Schiff ~ Code.ID) +
  theme(legend.position = "top",
        legend.direction = "vertical")

ggplot(ds.infect.codes.high, aes(x = Week, colour = Code.Titel)) +
  geom_freqpoly(position = 'identity',
                alpha = 1,
                binwidth = 1) +
  facet_grid(  Schiff ~ Code.ID) +
  theme(legend.position = "top",
        legend.direction = "vertical")

ggplot(ds.infect.codes.high, aes(x = Datum, colour = Code.Titel)) +
  geom_freqpoly(position = 'identity',
                alpha = 1,
                binwidth = 1) +
  facet_grid(  Schiff ~ Code.ID) +
  theme(legend.position = "top",
        legend.direction = "vertical")

# Codes with low frequency
ds.infect.codes.low <- ds.infect.codes %>%
  dplyr::filter(Code.ID %in% c("A16", "B01", "B02"))

ggds <- ggplot(ds.infect.codes.low, aes(x = Datum, fill = Code.Titel)) +
  geom_histogram(position = 'identity', alpha = 1, binwidth = 7) +
  facet_grid( Schiff ~ Code.ID ) +
  theme(legend.position="bottom",
        legend.direction="vertical")
ggds


