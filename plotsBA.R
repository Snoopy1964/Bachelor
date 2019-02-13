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
# set some defaults
#----------------------------------------------------------------------
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))

#----------------------------------------------------------------------
# Statistik
#----------------------------------------------------------------------

# zentrale Tabelle Fahrplan der Schiffe im Analyse Zeitraum 2015 - 2017
cat("\n\nTimetable: zentrale Tabelle Fahrplan der Schiffe im Analyse Zeitraum 2015 - 2017",
    "\nAggregiert pro Schiff",
    "\n-------------------------------------------------------------------------------------\n")
print(
  timetable.day                                         %>% 
    group_by(Schiff)                                    %>% 
    summarize(min(Datum),max(Datum), NrDays = nrDays(Day), NrPaxAvg = sum(PaxNr)/NrDays)
)

# zentrale Tabelle Fahrplan der Schiffe im Analyse Zeitraum 2015 - 2017
cat("\n\nTimetable: zentrale Tabelle Fahrplan der Schiffe im gesamten Zeitraum",
    "\nAggregiert pro Region",
    "\n--------------------------------------------------------------------------\n")
print(
  timetable.day                                         %>% 
    group_by(Region)                                    %>%
    summarize(min(Datum),max(Datum), NrDays = nrDays(Day), NrPaxAvg = sum(PaxNr)/NrDays)
  
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

# ds.tmp <- cases.infect.codes.ship.region %>%
#   group_by(Region) %>%
#   summarize(
#     Anzahl = sum(Anzahl),
#     PaxNr  = sum(PaxNr),
#     lng    = unique(lng),
#     lat    = unique(lat)
#   )
# 
# gg.region +
#   geom_point(data=ds.tmp, mapping=aes(x=lng, y=lat, size=Anzahl), alpha = 1/3) +
#   scale_size(range=c(4,10)) +
#   theme(legend.position="top")

#-------------------------------------------------------
# - Gesamtpassagierzahlen pro Tag (Summe aller Schiffe)
#-------------------------------------------------------
gg <- personNr.ship.day %>% 
  group_by(Day, Datum)  %>% 
  summarize(PaxNr = sum(PaxNr)/nrDays(Day), CrewNr = sum(CrewNr)/nrDays(Day)) %>%
  ggplot()

gg +
  geom_smooth(aes(x=Datum, y=PaxNr)) +
  geom_line(aes(x=Datum, y=PaxNr), color="black")   +
  geom_line(aes(x=Datum, y=CrewNr), color="red") +
  ylim(0, 16000) 

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\personNr.day.png")


# +
#   geom_rect(aes(xmin = as.Date("2015-01-01"),
#                 ymin = 0, 
#                 xmax = as.Date("2015-05-23"), 
#                 ymax = 16000), fill="blue", transient = TRUE, opacity=1/10)


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
# Desease Map's
# - A09
# - B01
# - B02
# - J11
# - A16
#-------------------------------------------------------

# A09 - Gastroenteritis
# Disease Map für Regionen
ds.tmp <- ds.region %>% 
  dplyr::filter(Code.ID == "A09")                  %>%
  mutate(
    Nr.Cases = Nr.Cases.Pax + Nr.Cases.Crew, 
    relFreq = Nr.Cases/((Pd.Crew+Pd.Pax)/Nr.Days)) %>%
  left_join(Regions, by="Region")

ggmap(map.Welt) + 
  geom_polygon(data = regions, aes(long, lat, group = group, fill = region), alpha = 0.5) +
  geom_point(data    = ds.tmp,
             mapping = aes(x    =lng, 
                           y    =lat, 
                           # size = relFreq.Pax,
                           # colour = Code.Titel),
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
  # facet_grid( Code.ID ~ . )                    +
  theme(#legend.title     = element_text("ICD10 Code"),
    legend.position  = "right",
    legend.direction = "vertical")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\diseaseMap.A09.region.all.png", 
       width = 297, height =210, units = "mm" )


# A16, B01, B02, J11
# Disease Map für Regionen
ds.tmp <- ds.region %>% 
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
    data = ds.tmp,
    mapping = aes(x = lng, y = lat, label = sprintf("%.4f", relFreq)),
    nudge_y = 2,
    nudge_x = 20,
    size = 3
  ) +
  ggtitle(str_c("Disease Map ","Infektionskrankheiten II")) + 
  theme(#legend.title     = "ICD10 Code",
    legend.position  = "right",
    legend.direction = "vertical")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\diseaseMap.A16+B01+B02+J11.region.all.png", 
       width = 297, height =210, units = "mm" )

#-------------------------------------------------------
# TimeSeries Analysis - EPI-Curve
# - A09
# - other
#   - B01
#   - B02
#   - J11
#   - A16
#-------------------------------------------------------

#---------------------------------------------------
# A09 - aggregiert über Schiffe und Regionen
#---------------------------------------------------
# Summe aller cases in einer Woche inkl. relativen Häufigkeiten
ds.A09.tmp <- ds                        %>%
  dplyr::filter(Code.ID == "A09")       %>%
  group_by(Week)                        %>%
  summarize(
    Nr.Cases.Crew = sum(Crew), 
    Nr.Cases.Pax  = sum(Pax), 
    Nr.Cases      = Nr.Cases.Crew+Nr.Cases.Pax,
    Nr.Days       = nrDays(Day),
    Pd.Pax        = sum(PaxNr),
    Pd.Crew       = sum(CrewNr),
    Pd.Pers       = Pd.Pax+Pd.Crew,
    relFreq.Pax   = Nr.Cases.Pax/(Pd.Pax/Nr.Days),
    relFreq.Crew  = Nr.Cases.Crew/(Pd.Crew/Nr.Days),
    relFreq.Pers  = Nr.Cases/(Pd.Pers/Nr.Days)
  )                 

gg <- ggplot(ds.A09.tmp, aes(x=Week))

# Plot absulte Anzahl Cases
gg +
  geom_step(aes(y=Nr.Cases), stat = "identity", alpha = 1/2) +
  geom_smooth(aes(x=Week, y=Nr.Cases), span = 0.3) +
  geom_smooth(aes(x=Week, y=Nr.Cases.Pax), color="red", span = 0.3) +
  geom_smooth(aes(x=Week, y=Nr.Cases.Crew), color="green", span = 0.3) +
  ggtitle("Anzahl Fälle Gastroenteritis (A09) gesamt" ) +
  xlab("Woche ab 1.1.2015 bis 31.12.2017") +
  ylab("Anzahl Fälle A09 pro Woche")
# geom_smooth(aes(x=Week, y=Nr.Cases.Crew), span = 0.3, color = "red") +
# geom_smooth(aes(x=Week, y=Nr.Cases.Pax), span = 0.3, color = "green") #+

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.absolut.all.all.png", 
       width = 297, height =210, units = "mm" )

# Plotte relative Häufigkeiten/Woche Personen/Crew/Pax
gg +
  geom_step(aes(y=relFreq.Pers), stat = "identity", alpha = 1/2) +
  geom_smooth(aes(x=Week, y=relFreq.Pers), span = 0.3) +
  geom_smooth(aes(x=Week, y=relFreq.Pax), span = 0.3, color="red") +
  geom_smooth(aes(x=Week, y=relFreq.Crew), span = 0.3, color="green") +
  ggtitle("relative Häufigkeit Gastroenteritis (A09) gesamt", subtitle = "normiert auf Anzahl Personentage/Anzahl Tage" ) +
  xlab("Wochen im Analysezeitraum 1.1.2015 bis 31.12.2017") +
  ylab("relative Häufigkeit A09 pro Woche")
# geom_smooth(aes(x=Week, y=Nr.Cases.Crew), span = 0.3, color = "red") +
# geom_smooth(aes(x=Week, y=Nr.Cases.Pax), span = 0.3, color = "green") #+

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.relativ.all.all.png", 
       width = 297, height =210, units = "mm" )

#----------------------------------------------------------------------
# Summe aller cases in einer Woche pro Schiff, aggregiert über Regionen
#----------------------------------------------------------------------
ds.A09.tmp <- ds                        %>%
  dplyr::filter(Code.ID == "A09")       %>%
  group_by(Week, Schiff)                %>%
  summarize(
    Nr.Cases.Crew = sum(Crew), 
    Nr.Cases.Pax  = sum(Pax), 
    Nr.Cases      = Nr.Cases.Crew+Nr.Cases.Pax,
    Nr.Days       = nrDays(Day),
    Pd.Pax        = sum(PaxNr),
    Pd.Crew       = sum(CrewNr),
    Pd.Pers       = Pd.Pax+Pd.Crew,
    relFreq.Pax   = Nr.Cases.Pax/(Pd.Pax/Nr.Days),
    relFreq.Crew  = Nr.Cases.Crew/(Pd.Crew/Nr.Days),
    relFreq.Pers  = Nr.Cases/(Pd.Pers/Nr.Days)
  )                 

gg <- ggplot(ds.A09.tmp, aes(x=Week))

# Plotte absolute Anzahl Fälle
gg +
  geom_step(aes(y=Nr.Cases), alpha = 1/2) +
  geom_smooth(aes(x=Week, y=Nr.Cases), span = 6/12, fullrange = FALSE) +
  facet_wrap( ~ Schiff, scales = NULL ) +
  ggtitle("Anzahl Fälle Gastroenteritis (A09) pro Schiff" ) +
  xlab("Woche ab 1.1.2015 bis 31.12.2017") +
  ylab("Anzahl Fälle A09 pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.absolut.schiff.all.png", 
       width = 297, height =210, units = "mm" )

# Plotte relative Häufigkeiten/Woche Personen/Crew/Pax
gg +
  facet_wrap( ~ Schiff, scales = "free_x" ) +
  geom_step(aes(y=relFreq.Pers), stat = "identity", alpha = 1/2) +
  geom_smooth(aes(x=Week, y=relFreq.Pers), span = 0.3) +
  # geom_smooth(aes(x=Week, y=relFreq.Pax), span = 0.3, color="red") +
  # geom_smooth(aes(x=Week, y=relFreq.Crew), span = 0.3, color="green") +
  ggtitle("relative Häufigkeit Gastroenteritis (A09) pro Schiff", subtitle = "normiert auf Anzahl Personentage/Anzahl Tage" ) +
  xlab("Wochen im Analysezeitraum 1.1.2015 bis 31.12.2017") +
  ylab("relative Häufigkeit A09 pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.relativ.schiff.all.png", 
       width = 297, height =210, units = "mm" )


#----------------------------------------------------------------------
# Summe aller cases in einer Woche pro Region, aggregiert über Schiffe
#----------------------------------------------------------------------
ds.A09.tmp <- ds                        %>%
  dplyr::filter(Code.ID == "A09")       %>%
  group_by(Week, Region)                %>%
  summarize(
    Nr.Cases.Crew = sum(Crew), 
    Nr.Cases.Pax  = sum(Pax), 
    Nr.Cases      = Nr.Cases.Crew+Nr.Cases.Pax,
    Nr.Days       = nrDays(Day),
    Pd.Pax        = sum(PaxNr),
    Pd.Crew       = sum(CrewNr),
    Pd.Pers       = Pd.Pax+Pd.Crew,
    relFreq.Pax   = Nr.Cases.Pax/(Pd.Pax/Nr.Days),
    relFreq.Crew  = Nr.Cases.Crew/(Pd.Crew/Nr.Days),
    relFreq.Pers  = Nr.Cases/(Pd.Pers/Nr.Days)
  )                 

gg <- ggplot(ds.A09.tmp, aes(x=Week))

# Plotte absolute Anzahl Fälle
gg +
  geom_bar(aes(y=Nr.Cases), stat="identity", alpha = 3/4) +
  # geom_smooth(aes(x=Week, y=Nr.Cases), span = 6/12, fullrange = FALSE) +
  facet_wrap( ~ Region, scales = NULL ) +
  ggtitle("Anzahl Fälle Gastroenteritis (A09) pro Region" ) +
  xlab("Woche ab 1.1.2015 bis 31.12.2017") +
  ylab("Anzahl Fälle A09 pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.absolut.region.all.png", 
       width = 297, height =210, units = "mm" )

# Plotte relative Häufigkeiten/Woche Personen
gg +
  facet_wrap( ~ Region, scales = NULL ) +
  geom_bar(aes(y=relFreq.Pers), stat = "identity", alpha = 3/4) +
  # geom_smooth(aes(x=Week, y=relFreq.Pers), span = 0.3) +
  # geom_smooth(aes(x=Week, y=relFreq.Pax), span = 0.3, color="red") +
  # geom_smooth(aes(x=Week, y=relFreq.Crew), span = 0.3, color="green") +
  ggtitle("relative Häufigkeit Gastroenteritis (A09) pro Region", subtitle = "normiert auf Summe Personentage/Anzahl Tage" ) +
  xlab("Wochen im Analysezeitraum 1.1.2015 bis 31.12.2017") +
  ylab("relative Häufigkeit A09 pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.relativ.region.all.png", 
       width = 297, height =210, units = "mm" )

############################################################################################# 
#-------------------------------------------------------
# # Summary infect cases / total number of (Passengers plus Crew)
# #-------------------------------------------------------
# 
# ds.infect.codes.ship.nr %>%
#   ggplot(aes(x=Code.ID, y=relFreq, fill=Schiff)) +
#   geom_bar(stat="identity", position="dodge")
# 
# ds.infect.codes.region.nr %>%
#   ggplot(aes(x=Code.ID, y=relFreq, fill=Region)) +
#   geom_bar(stat="identity", position="dodge")
# 


# # Codes with high frequency
# ds.infect.codes.high <- ds.infect.codes %>%
#   dplyr::filter(Code.ID %in% c("A09", "J00", "J06"))
# 
# ggds <- ggplot(ds.infect.codes.high, aes(x = Datum, fill = Code.Titel)) +
#   geom_histogram(position = 'identity', alpha = 1, binwidth = 7) +
#   facet_grid( Schiff ~ Code.ID ) +
#   theme(legend.position="top",
#         legend.direction="vertical")
# ggds
# 
# ggplot(ds.infect.codes.high, aes(x = Month, colour = Code.Titel)) +
#   geom_freqpoly(position = 'identity',
#                 alpha = 1,
#                 binwidth = 1) +
#   facet_grid(  Schiff ~ Code.ID) +
#   theme(legend.position = "top",
#         legend.direction = "vertical")
# 
# ggplot(ds.infect.codes.high, aes(x = Week, colour = Code.Titel)) +
#   geom_freqpoly(position = 'identity',
#                 alpha = 1,
#                 binwidth = 1) +
#   facet_grid(  Schiff ~ Code.ID) +
#   theme(legend.position = "top",
#         legend.direction = "vertical")
# 
# ggplot(ds.infect.codes.high, aes(x = Datum, colour = Code.Titel)) +
#   geom_freqpoly(position = 'identity',
#                 alpha = 1,
#                 binwidth = 1) +
#   facet_grid(  Schiff ~ Code.ID) +
#   theme(legend.position = "top",
#         legend.direction = "vertical")
# 
# # Codes with low frequency
# ds.infect.codes.low <- ds.infect.codes %>%
#   dplyr::filter(Code.ID %in% c("A16", "B01", "B02", "J11"))
# 
# ggds <- ggplot(ds.infect.codes.low, aes(x = Datum, fill = Code.Titel)) +
#   geom_histogram(position = 'identity', alpha = 1, binwidth = 7) +
#   facet_grid( Schiff ~ Code.ID ) +
#   theme(legend.position="bottom",
#         legend.direction="vertical")
# ggds


