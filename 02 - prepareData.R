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
# ds.loc............: all cases in time period [2015-01-01, 2017-12-31]
# ds.infect.chapters: all infect cases (chapter 01 & 10) in time period [2015-01-01, 2017-12-31]
# ds.infect.codes...: selected infect.codes for analysis in time period [2015-01-01, 2017-12-31]
#
#
##################################################################################################

# - Analysezeitraum 01.01.2015-31.12.2017
ds.loc <- ds.loc %>% 
  # - Analysezeitraum 01.01.2015-31.12.2017
  dplyr::filter(Datum >= "2015-01-01" & Datum < "2018-01-01")  %>%
  # - in icd10-GM ist I84 nicht definiert
  dplyr::filter(!is.na(Gruppen.ID)) %>%
  # - auf der MS4 gibt es am 4. und 5.6.2015 Fälle. Es gibt jedoch keine gültige
  #   Tour, vermutlich, da die MS4 hier in der Werft war. Aus diesem Grund
  #   gibt es keine Passagierzahlen -> der Wert is NA -> setze diese auf 0
  mutate(PaxNr = ifelse(is.na(PaxNr), 0, PaxNr)) %>%
  # berechne die Anzahl der Tage, der Wochen und der Monate ab dem 1.1.2015
  mutate( Year  = year(Datum), 
          Month = (year(Datum) - year(min(Datum)))*12 + month(Datum),
          Day   = as.numeric(Datum - min(Datum)) + 1,
          Week  = trunc(Day/7) + 1
  )


# all infect cases (chapter 01 & 10)
infect.chapters <- c(
  "01",   # Kapitel: 01 - Bestimmte infektiöse und parasitäre Krankheiten, Codes A00 - B99
  "10"    # Kapitel: 10 - Krankheiten des Atmungssystems                 , Codes J00 - J99
)

ds.infect.chapters <- ds.loc %>% dplyr::filter(Kapitel.ID %in% infect.chapters )


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

ds.infect.codes <- ds.loc %>% dplyr::filter(Code.ID %in% infect.codes )

# remove unnecessary data
if (exists("ds.all")) rm("ds.all")
if (exists("Cases"))  rm("Cases")

# save data as csv-file
# write.csv2 is mixing up with "," and 1000 separater "."
# write.csv2(ds.infect.chapters, file="data/Results/ds.infect.cases.csv", fileEncoding = "UTF-8")
write_delim(ds.infect.chapters, "data/Results/ds.infect.cases.csv", delim = ";")




