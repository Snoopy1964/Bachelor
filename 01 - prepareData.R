##################################################################################################
# Daten für Analyse vorbereiten
#
# - Analysezeitraum 01.01.2015-31.12.2017 -> ds (tibble)
#   - Randeffekte minimieren 
#   - Touren sind erst ab 2014 Oktober verfügbar 
#   - Passagierzahlen sind ebenso erst in diesem Zeitraum valide
#   - Geschlecht falsch codiert F (female) statt W (weiblich), und 0 -> NA
#
# Data samples:
#-------------------------------------------------------------------------------------------------
# cases................: all cases in time period [2015-01-01, 2017-12-31]
# cases.infect.chapters: all infect cases (chapter 01 & 10) in time period [2015-01-01, 2017-12-31]
# cases.infect.codes...: selected infect.codes for analysis in time period [2015-01-01, 2017-12-31]
#
#
##################################################################################################

# - Analysezeitraum 01.01.2015-31.12.2017
cases <- cases %>% 
  # - in icd10-GM ist I84 nicht definiert
  dplyr::filter(!is.na(Gruppen.ID))


# all infect cases (chapter 01 & 10)
infect.chapters <- c(
  "01",   # Kapitel: 01 - Bestimmte infektiöse und parasitäre Krankheiten, Codes A00 - B99
  "10"    # Kapitel: 10 - Krankheiten des Atmungssystems                 , Codes J00 - J99
)

cases.infect.chapters <- cases %>% dplyr::filter(Kapitel.ID %in% infect.chapters )
# save data as csv-file
write_delim(cases.infect.chapters, "data/Results/ds.infect.cases.csv", delim = ";")


# selected infect.codes for analysis
infect.codes <- c(
  "A09", # (Gastroenteritis)
  "B01", # (Varizellen)
  "B02", # (zoster)
  "J11", # (Grippe)
  "A16"  # (Tuberkulose)
  #----------------------------------------------------------------------
  # 2019.02.09 - rausgenommen, da sonst zu viel Daten für Bachelor Arbeit
  #----------------------------------------------------------------------
  # "J00", # (Akute Rhinopharyngitis [Erkältungsschnupfen])
  # "J06", # (Akute Infektionen an mehreren oder nicht näher bezeichneten Lokalisationen der oberen Atemwege)
  # "J40"  # (Bronchitis, nicht als akut oder chronisch bezeichnet)
)

cases.infect.codes <- cases %>% dplyr::filter(Code.ID %in% infect.codes )

# filter auf infect codes und Aggregation auf Tages Ebene
cases.day <- cases.infect.codes               %>%
  group_by(Datum, Schiff, Code.ID, PaxStatus) %>%
  summarize(Anzahl = n())                     %>%
  # Tidying data (https://r4ds.had.co.nz/tidy-data.html#spreading), 
  # PaxStatus splitted die Observations in 2 Zeilen (Anzahl Crew, Anzahl Passagiere)
  # -> diese müssen zusammengeführt werden in eine Zeile
  spread(PaxStatus, value = Anzahl, fill = 0, sep=NULL)

# (5) Verknüpfe Schiffsfahrplan mit Cases
#     - Join timetable.day auf cases.day
# ist nur als Sicherung hier - nicht mehr verwenden!
ds.falsch <- timetable.day                          %>%
  left_join(cases.day, by=c("Datum", "Schiff"))  %>%
  mutate(
    Crew = if_else(is.na(Crew), 0, Crew), 
    Pax = if_else(is.na(Pax), 0, Pax))

# (6) Bilde eine fortluafende TimeSeries (pro Tag) für jeden Infect (A0, A16, B0, B16, J11)
#     - Join timetable.day auf cases.day.A09

ts.A09 <- timetable.day                          %>%
  left_join(cases.day %>% dplyr::filter(Code.ID == "A09"), by=c("Datum", "Schiff"))  %>%
  mutate(
    Crew    = if_else(is.na(Crew), 0, Crew), 
    Pax     = if_else(is.na(Pax), 0, Pax),
    Code.ID = "A09"
  )

ts.A16 <- timetable.day                          %>%
  left_join(cases.day %>% dplyr::filter(Code.ID == "A16"), by=c("Datum", "Schiff"))  %>%
  mutate(
    Crew    = if_else(is.na(Crew), 0, Crew), 
    Pax     = if_else(is.na(Pax), 0, Pax),
    Code.ID = "A16"
  )

ts.B01 <- timetable.day                          %>%
  left_join(cases.day %>% dplyr::filter(Code.ID == "B01"), by=c("Datum", "Schiff"))  %>%
  mutate(
    Crew    = if_else(is.na(Crew), 0, Crew), 
    Pax     = if_else(is.na(Pax), 0, Pax),
    Code.ID = "B01"
  )

ts.B02 <- timetable.day                          %>%
  left_join(cases.day %>% dplyr::filter(Code.ID == "B02"), by=c("Datum", "Schiff"))  %>%
  mutate(
    Crew    = if_else(is.na(Crew), 0, Crew), 
    Pax     = if_else(is.na(Pax), 0, Pax),
    Code.ID = "B02"
  )

ts.J11 <- timetable.day                          %>%
  left_join(cases.day %>% dplyr::filter(Code.ID == "J11"), by=c("Datum", "Schiff"))  %>%
  mutate(
    Crew    = if_else(is.na(Crew), 0, Crew), 
    Pax     = if_else(is.na(Pax), 0, Pax),
    Code.ID = "J11"
  )

ts <- rbind(ts.A09,ts.A16, ts.B01, ts.B02, ts.J11)

rm(list = ls(pattern="ts\\.[ABJ]{0-9}{0-9}"))

