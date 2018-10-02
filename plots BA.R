#
# Plots für Bacherarbeit
#
#

source('97 - loadLocalMaps.R')

#----------------------------------------------------------------------
# Statistik
#----------------------------------------------------------------------

# zentrale Tabelle Fahrplan der Schiffe im gesamten Zeitraum
cat("\n\nTimetable: zentrale Tabelle Fahrplan der Schiffe im gesamten Zeitraum",
      "\n--------------------------------------------------------------------------\n")
print(Timetable        %>% 
      group_by(Schiff) %>%
      summarize(min(Datum),max(Datum), NrTours = n())
)

# zentrale Tabelle Fahrplan der Schiffe im Analyse Zeitraum 2015 - 2017
cat("\n\nTimetable: zentrale Tabelle Fahrplan der Schiffe im Analyse Zeitraum 2015 - 2017",
      "\n-------------------------------------------------------------------------------------\n")
print(Timetable                                                   %>% 
      dplyr::filter(Datum >= "2015-01-01" & Datum < "2018-01-01") %>% 
      group_by(Schiff)                                            %>% 
      summarize(min(Datum),max(Datum), NrTours = n())
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
  # geom_polygon(
  #   data = Mittelmeer.region,
  #   mapping = aes(x = lng, y = lat),
  #   color = "red",
  #   alpha = 1 / 5
  # ) +
  geom_point(data = Ports,
             mapping = aes(x = lng, y = lat)) +
  geom_label_repel(
    data = Ports,
    mapping = aes(x = lng, y = lat, label = geoName),
    size = 3
  )



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


