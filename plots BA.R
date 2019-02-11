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
# zentrale Tabelle Fahrplan der Schiffe im Analyse Zeitraum 2015 - 2017
cat("\n\nTimetable: zentrale Tabelle Fahrplan der Schiffe im Analyse Zeitraum 2015 - 2017",
    "\n-------------------------------------------------------------------------------------\n")
print(
  timetable.day                                                 %>% 
    dplyr::filter(Datum >= "2015-01-01" & Datum < "2018-01-01") %>% 
    group_by(Schiff)                                            %>% 
    summarize(min(Datum),max(Datum), NrDays = n())
)

# zentrale Tabelle Fahrplan der Schiffe im gesamten Zeitraum
cat("\n\nTimetable: zentrale Tabelle Fahrplan der Schiffe im gesamten Zeitraum",
    "\n--------------------------------------------------------------------------\n")
print(
  timetable.day                                                 %>% 
    group_by(Schiff, Region)                                    %>%
    summarize(min(Datum),max(Datum), NrDays = n())
  
)


#----------------------------------------------------------------------
# Plots
#----------------------------------------------------------------------
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

ds.tmp <- cases.infect.codes.ship.region %>%
  group_by(Region) %>%
  summarize(
    Anzahl = sum(Anzahl),
    PaxNr  = sum(PaxNr),
    lng    = unique(lng),
    lat    = unique(lat)
  )

gg.region +
  geom_point(data=ds.tmp, mapping=aes(x=lng, y=lat, size=Anzahl), alpha = 1/3) +
  scale_size(range=c(4,10)) +
  theme(legend.position="top")


#------------------------------------------------------------------------
# Overview frequencies all Cases in time period [2015-01-01, 2017-12-31] 
# grouped by chapters
#------------------------------------------------------------------------

cases.nr <- cases                     %>% 
  group_by(Kapitel.ID, Kapitel.Titel) %>% 
  summarise(Anzahl = n())             %>% 
  arrange(desc(Anzahl))

# - plotte die Verteilung der icd10-Kapitel (ranked nach Anzahl)
gg.cases <- ggplot(cases.nr)

# Verkürze die Titel der x-Achsen (hier bei y, da coord_flip())
x.Axis       <- icd10.chapters[["Kapitel.Titel"]]
x.Axis.Label <- ifelse(str_length(icd10.chapters[["Kapitel.Titel"]]) >= 56, 
                       str_c(str_sub(icd10.chapters[["Kapitel.Titel"]], 1, 56), "...", sep=""), 
                       str_c(icd10.chapters[["Kapitel.Titel"]]))

# print(
gg.cases +
  geom_bar(
    mapping = aes(x = reorder(Kapitel.Titel, Anzahl), 
                  y = Anzahl
    ),
    stat = "identity")                            +
  scale_x_discrete(breaks = x.Axis, labels = x.Axis.Label) +
  ggtitle("Verteilung der Cases bzgl. ICD10-Kapitel")    +
  labs(x = "ICD10-Kapitel",
       y = "Anzahl Cases")                              +
  # scale_y_log10() +
  theme(
    plot.title  = element_text(hjust = 0.5,
                               vjust = 7),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )             +
  coord_flip()
#)


#print(
gg.cases +
  geom_bar(
    mapping = aes(x = reorder(Kapitel.Titel,-Anzahl), 
                  y = Anzahl
    ),
    stat = "identity")                            +
  scale_x_discrete(breaks = x.Axis, labels = x.Axis.Label) +
  ggtitle("Verteilung der Cases bzgl. ICD10-Kapitel")    +
  labs(x = "ICD10-Kapitel",
       y = "Anzahl Cases")   +
  theme(
    axis.text.x = element_text(
      angle = 60,
      hjust = 1,
      vjust = 1
    ),
    plot.title  = element_text(hjust = 0.5,
                               vjust = 7),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )
# )

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
        legend.position  = "right",
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
  dplyr::filter(Code.ID %in% c("A16", "B01", "B02", "J11"))

ggds <- ggplot(ds.infect.codes.low, aes(x = Datum, fill = Code.Titel)) +
  geom_histogram(position = 'identity', alpha = 1, binwidth = 7) +
  facet_grid( Schiff ~ Code.ID ) +
  theme(legend.position="bottom",
        legend.direction="vertical")
ggds


