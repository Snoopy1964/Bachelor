#########################################################################
# Daten Analyse
# - Deskriptive Rangfolge Kapitel 01 und 10 ICD-10
#
#
#
# - Passagierzahlen bestimmen 
#   - pro Tag aus Timetable, aus Datum Woche und Monat fortlaufend durchnummeriert 
#     (zur späteren Gruppierung der Daten) 
#   - pro Woche durch Aggregation der PersonenTage durch Gruppierung nach Woche / Anzahl Tage (n())
#   - pro MOnat durch Aggregation der PersonenTage durch Gruppierung nach Monat / Anzahl Tage (n())
#
#
#
#
#########################################################################

#------------------------------------------------------------------------
# Overview frequencies all Cases in time period [2015-01-01, 2017-12-31] 
# grouped by chapters
#------------------------------------------------------------------------

ds.loc.nr <- ds.loc                   %>% 
  group_by(Kapitel.ID, Kapitel.Titel) %>% 
  summarise(Anzahl = n())             %>% 
  arrange(desc(Anzahl))

# - plotte die Verteilung der icd10-Kapitel (ranked nach Anzahl)
ggds.loc <- ggplot(ds.loc.nr)

# Verkürze die Titel der x-Achsen (hier bei y, da coord_flip())
x.Axis       <- icd10.chapters[["Kapitel.Titel"]]
x.Axis.Label <- ifelse(str_length(icd10.chapters[["Kapitel.Titel"]]) >= 56, 
                       str_c(str_sub(icd10.chapters[["Kapitel.Titel"]], 1, 56), "...", sep=""), 
                       str_c(icd10.chapters[["Kapitel.Titel"]]))

# print(
  ggds.loc +
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
  ggds.loc +
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

# # - Gesamtanzahl/durchschnittliche Anzahl Passagiere/Schiff im Analysezeitraum
# PaxNr.Schiff <- ToursPax %>% 
#   dplyr::filter(EndDate >= "2015-01-01" & StartDate <= "2017-12-31") %>% 
#   group_by(Schiff) %>%  
#   summarize(PaxGesamt = sum(PaxNr), PaxMean = mean(PaxNr), PaxMedian = median(PaxNr), PaxMad = mad(PaxNr)) %>% 
#   left_join(select(Ships, "Schiff", "MaxPaxNr", "CrewNr"), by="Schiff")
# 
# # - Passagierzahlen/Tag/Schiff im Analysezeitraum
# PaxNr.Schiff.Day <- ds.loc %>% 
#   group_by(Datum, Day, Schiff) %>% 
#   summarize(Anzahl.Infects = n(), PaxNr = unique(PaxNr), PaxNr = PaxNr) %>%
#   mutate(Rate.Infects = Anzahl.Infects / PaxNr)


# - Passagierzahlen pro Tag (PersonenTage)
PersonNr.Ship.Day <- select(Timetable, Datum, Schiff, TourNr, `Port Name`, LocDesc) %>% 
  dplyr::filter(Datum >= "2015-01-01" & Datum < "2018-01-01")                    %>%
  left_join(select(ToursPax, TourNr, DurationDays, Route, PaxNr), by="TourNr")   %>%
  left_join(select(Ports, `Port Name`, Region), by="Port Name")                  %>%
  # Crew Zahlen aus Ships
  left_join(select(Ships, Schiff, CrewNr), by="Schiff")                          %>%
  # berechne die fortlaufende Anzahl der Tage, der Wochen und der Monate ab dem 1.1.2015
  mutate( Year  = year(Datum), 
          Month = (year(Datum) - year(min(Datum)))*12 + month(Datum),
          Day   = as.numeric(Datum - min(Datum)) + 1,
          Week  = trunc(Day/7) + 1
  )

# - Passagierzahlen pro Woche, aus PersonDays
PaxNr.Ship.Week <- PersonNr.Ship.Day %>% 
  group_by(Schiff, Week) %>% 
  summarise(PaxNr = sum(PaxNr)/n(), nr.Days = n())

# - Passagierzahlen pro Monat, aus PersonDays
PaxNr.Ship.Month <- PersonNr.Ship.Day %>% 
  group_by(Schiff, Month) %>% 
  summarise(PaxNr = sum(PaxNr)/n(), nr.Days = n())

# - Passagierzahlen pro Shiff über den gesamten Analysezeitraum
#   Hier muss beachtet werden, dass z.B. die MS6 erst im April 2017 in DIest gestellt wurde
PaxNr.Ship <- PersonNr.Ship.Day %>% 
  group_by(Schiff) %>% 
  summarise(PaxNr = sum(PaxNr)/n(), nr.Days = n())

PaxNr.Ship.Month %>%
  ggplot(aes(x=Month, y=PaxNr, group=Schiff, color=Schiff)) + 
  geom_line() +
  facet_wrap(~ Schiff, ncol = 2)

# - Gesamtpassagierzahlen pro Tag (Summe aller Schiffe)
gg <- PersonNr.Ship.Day %>% 
  # dplyr::filter(Datum >= "2015-01-01" & Datum < "2015-07-01") %>%
  group_by(Day, Datum)  %>% 
  summarize(PaxNr = sum(PaxNr), CrewNr = sum(CrewNr)) %>%
  ggplot()

gg +
  geom_smooth(aes(x=Datum, y=PaxNr)) +
  geom_line(aes(x=Datum, y=PaxNr, color="black"))   +
  geom_line(aes(x=Datum, y=CrewNr, color="red")) +
  ylim(0, 16000) 
# +
#   geom_rect(aes(xmin = as.Date("2015-01-01"),
#                 ymin = 0, 
#                 xmax = as.Date("2015-05-23"), 
#                 ymax = 16000), fill="blue", transient = TRUE, opacity=1/10)



# - Gesamtanzahl Passagiere/Region im Analysezeitraum
PaxNr.Region.Ship <- PersonNr.Ship.Day %>% 
  group_by(Region, Schiff) %>% 
  summarize(PaxNr = sum(PaxNr)/n(), nr.Days = n())

#-----------------------------------------------------------------
# Overview frequencies all Cases grouped by infect groups
# - chapters 01 & 10
# - time period: [2015-01-01, 2017-12-31]
#-----------------------------------------------------------------

ds.infect.chapters.nr <- ds.infect.chapters  %>% 
  group_by(Gruppen.ID, Gruppen.Titel)        %>% 
  summarise(Anzahl = n())                    %>% 
  arrange(desc(Anzahl))

ds.infect.groups.nr <- ds.infect.chapters  %>% 
  group_by(Code.ID, Code.Titel)        %>% 
  summarise(Anzahl = n())                    %>% 
  arrange(desc(Anzahl))

#-----------------------------------------------------------------
# Overview frequencies all Cases grouped by selected infect codes
# - codes: A09, B01, B02, J11, J00, J06, J40, A16
# - time period: [2015-01-01, 2017-12-31]
#-----------------------------------------------------------------

ds.infect.codes.nr <- ds.infect.codes       %>% 
  group_by(Code.ID, Code.Titel)             %>% 
  summarise(Anzahl = n())                   %>% 
  arrange(desc(Anzahl))

# - same grouped by ships
ds.infect.codes.ship.nr <- ds.infect.codes  %>%
  group_by(Schiff, Code.ID, Code.Titel)     %>%
  summarize(Anzahl = n())                   %>%
  # calculate relative frequencies per ship
  # (Nr divided by total number of passengers
  # in analysis time period)
  left_join(PaxNr.Ship, by = "Schiff")    %>%
  mutate(relFreq = Anzahl / PaxNr)

# - same grouped by Region
ds.infect.codes.region.nr <- ds.infect.codes  %>%
  group_by(Region, Code.ID, Code.Titel)       %>%
  summarize(Anzahl = n())                     %>%
  left_join(
    summarize(PaxNr.Region.Ship, PaxNr = sum(PaxNr)), 
    by="Region"
    )                                         %>%
  # calculate relative frequencies per ship
  # (Nr divided by total number of passengers
  # in analysis time period)
  mutate(relFreq = Anzahl / PaxNr)

# Anzahl der Infect Codes pro Shiff, pro Region (inkl. lon/lat) 
ds.infect.codes.ship.region <- ds.infect.codes           %>%
  group_by(Code.ID, Region, Schiff, PaxStatus)           %>%
  summarize(Anzahl = n())                                %>%
  left_join(PaxNr.Region.Ship, by=c("Region", "Schiff")) %>%
  left_join(Regions, by="Region")                        %>%
  left_join(
    select(Ships, Schiff, CrewNr, MaxPaxNr), 
    by="Schiff"
  )

