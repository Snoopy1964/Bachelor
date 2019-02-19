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


# Passagier-, Crew- und Personenanzahl pro Tag (Personen Tage), aus timetable.day
personNr.ship.day <- select(timetable.day, Datum, Schiff, Region, PaxNr, CrewNr, PersNr, Year, Month, Day, Week)
  
# - Passagierzahlen pro Woche, aus PersonDays
personNr.ship.week <- personNr.ship.day %>% 
  group_by(Schiff, Week) %>% 
  # Bestimmung der PersonenTage. Für alle Analysen sind diese plus die Anzahl Tage zu verwenden
  summarise(Pax.Pd = sum(PaxNr), Crew.Pd = sum(CrewNr), nr.Days = nrDays(Day)) %>%
  mutate(Pax.Nr = Pax.Pd/nr.Days, Crew.Nr = Crew.Pd/nr.Days, Pers.Nr = (Pax.Pd + Crew.Pd)/nr.Days)

# - Passagierzahlen pro Monat, aus PersonDays
personNr.ship.month <- personNr.ship.day %>% 
  group_by(Schiff, Month) %>% 
  # Bestimmung der PersonenTage. Für alle Analysen sind diese plus die Anzahl Tage zu verwenden
  summarise(Pax.Pd = sum(PaxNr), Crew.Pd = sum(CrewNr), nr.Days = nrDays(Day)) %>%
  # Daten nur zur Info, müssen immer neu bestimmt werden
  mutate(Pax.Nr = Pax.Pd/nr.Days, Crew.Nr = Crew.Pd/nr.Days, Pers.Nr = (Pax.Pd + Crew.Pd)/nr.Days)

# - Passagierzahlen pro Jahr, aus PersonDays
personNr.ship.year <- personNr.ship.day %>% 
  group_by(Schiff, Year) %>% 
  # Bestimmung der PersonenTage. Für alle Analysen sind diese plus die Anzahl Tage zu verwenden
  summarise(Pax.Pd = sum(PaxNr), Crew.Pd = sum(CrewNr), nr.Days = nrDays(Day)) %>%
  mutate(Pax.Nr = Pax.Pd/nr.Days, Crew.Nr = Crew.Pd/nr.Days, Pers.Nr = (Pax.Pd + Crew.Pd)/nr.Days)

# - Passagierzahlen pro Shiff über den gesamten Analysezeitraum
#   Hier muss beachtet werden, dass z.B. die MS6 erst im April 2017 in DIest gestellt wurde
personNr.ship <- personNr.ship.day %>% 
  group_by(Schiff) %>% 
  # Bestimmung der PersonenTage. Für alle Analysen sind diese plus die Anzahl Tage zu verwenden
  summarise(Pax.Pd = sum(PaxNr), Crew.Pd = sum(CrewNr), nr.Days = nrDays(Day)) %>%
  mutate(Pax.Nr = Pax.Pd/nr.Days, Crew.Nr = Crew.Pd/nr.Days, Pers.Nr = (Pax.Pd + Crew.Pd)/nr.Days)

# - Passagierzahlen pro Region über den gesamten Analysezeitraum
#   Hier muss beachtet werden, dass z.B. die MS6 erst im April 2017 in DIest gestellt wurde
personNr.region <- personNr.ship.day %>% 
  group_by(Region) %>% 
  # Bestimmung der PersonenTage. Für alle Analysen sind diese plus die Anzahl Tage zu verwenden
  summarise(Pax.Pd = sum(PaxNr), Crew.Pd = sum(CrewNr), nr.Days = nrDays(Day)) %>%
  mutate(Pax.Nr = Pax.Pd/nr.Days, Crew.Nr = Crew.Pd/nr.Days, Pers.Nr = (Pax.Pd + Crew.Pd)/nr.Days)

# - Passagierzahlen pro Schiff un pro Region über den gesamten Analysezeitraum
#   Hier muss beachtet werden, dass z.B. die MS6 erst im April 2017 in DIest gestellt wurde
personNr.ship.region <- personNr.ship.day %>% 
  group_by(Schiff, Region) %>% 
  # Bestimmung der PersonenTage. Für alle Analysen sind diese plus die Anzahl Tage zu verwenden
  summarise(Pax.Pd = sum(PaxNr), Crew.Pd = sum(CrewNr), nr.Days = nrDays(Day)) %>%
  mutate(Pax.Nr = Pax.Pd/nr.Days, Crew.Nr = Crew.Pd/nr.Days, Pers.Nr = (Pax.Pd + Crew.Pd)/nr.Days)


#-----------------------------------------------------------------
# Overview frequencies all Cases grouped by selected infect codes
# - codes: A09, A16, B01, B02, J11
# - time period: [2015-01-01, 2017-12-31]
#-----------------------------------------------------------------

# A09
ts.A09.day <- ts %>% 
  dplyr::filter(Code.ID == "A09") %>% 
  group_by(Schiff, Region, `Port Name`, Datum, Day) %>% 
  summarize(
    Nr.Days=nrDays(Day), 
    Pd.Crew=sum(CrewNr), 
    Pd.Pax=sum(PaxNr), 
    Nr.Cases.Crew=sum(Crew), 
    Nr.Cases.Pax=sum(Pax), 
    relFreq.Crew=Nr.Cases.Crew/(Pd.Crew/Nr.Days), 
    relFreq.Pax=Nr.Cases.Pax/(Pd.Pax/Nr.Days)
    )

# relative Häufigkeit pro Infektions Code und pro Region im gesamten Zeitraum
ts.region <- ts %>% 
  group_by(Region, Code.ID) %>% 
  summarize(
    Nr.Days=nrDays(Day), 
    Pd.Crew=sum(CrewNr), 
    Pd.Pax=sum(PaxNr), 
    Nr.Cases.Crew=sum(Crew), 
    Nr.Cases.Pax=sum(Pax), 
    relFreq.Crew=Nr.Cases.Crew/(Pd.Crew/Nr.Days), 
    relFreq.Pax=Nr.Cases.Pax/(Pd.Pax/Nr.Days)
  )


# cases.infect.codes.nr <- cases.infect.codes       %>% 
#   group_by(Code.ID, Code.Titel)             %>% 
#   summarise(Anzahl = n())                   %>% 
#   arrange(desc(Anzahl))
# 
# # - same grouped by ships
# cases.infect.codes.ship.nr <- cases.infect.codes  %>%
#   group_by(Schiff, Code.ID, Code.Titel)     %>%
#   summarize(Anzahl = n())                   %>%
#   # calculate relative frequencies per ship
#   # (Nr divided by total number of passengers
#   # in analysis time period)
#   left_join(paxNr.ship, by = "Schiff")    %>%
#   mutate(relFreq = Anzahl / PaxNr)
# 
# # - same grouped by Region
# cases.infect.codes.region.nr <- cases.infect.codes  %>%
#   group_by(Region, Code.ID, Code.Titel)       %>%
#   summarize(Anzahl = n())                     %>%
#   left_join(
#     summarize(paxNr.region.ship, PaxNr = sum(PaxNr)), 
#     by="Region"
#     )                                         %>%
#   # calculate relative frequencies per ship
#   # (Nr divided by total number of passengers
#   # in analysis time period)
#   mutate(relFreq = Anzahl / PaxNr)
# 
# # Anzahl der Infect Codes pro Shiff, pro Region (inkl. lon/lat) 
# cases.infect.codes.ship.region <- cases.infect.codes             %>%
#   group_by(Code.ID, Region, Schiff, PaxStatus, Geschlecht) %>%
#   summarize(Anzahl = n())                                  %>%
#   left_join(paxNr.region.ship, by=c("Region", "Schiff"))   %>%
#   left_join(Regions, by="Region")                          %>%
#   left_join(
#     select(Ships, Schiff, CrewNr, MaxPaxNr), 
#     by=c("Schiff","CrewNr")
#   )                                                        %>%
#   mutate(relFreq.mc=)
# 
# # Anzahl der Infec
# 
