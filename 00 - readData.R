##########################################################################################
#
# Definitionen
#
# - Region:
#     frei definiertes Polygon, welches den Fahrgebieten der Schiffe entspricht.
#     Die Region ist somit eine Gruppierung von angefahrenen Häfen. An den Häfen ist 
#     die GeoPosition festgelegt. Alle Häfen werden einer Region zugeordnet. Dieser 
#     wiederum wird ein zentraler Punkt (geschätzt) als Mittelpunkt des Polygons 
#     einer Region als GeoPosition zugeordnet
#
# - Route: (nicht verwendet in der Arbeit, da zu wenig Statistik/Route)
#     entspricht eine Abfolge von Häfen, enthält keine konkreten Zeitpunkte, 
#     kann von verschiedenen Schiffen abgefahren werden. 
#
# - Tour (Trip):  (nicht verwendet in der Arbeit, da selbst für Trips zu wenig Statistik/Trip)
#     Eine Tour (ein Trip) entspricht einer buchbaren Reise eines Schiffes auf einer bestimmten Route
#     zu einem bestimmten Zeitpunkt  
#     mit Start, Ende und Passagierwechsel. 
#
# - Inzidenz für Schiffe:
#     Häufigkeit der Neuerkrankungen bezogen auf die Anzahl der Gäste plus der Crew pro Woche (oder pro Tour?)
#
#------------------------------------------------------------------------
# ToDo's:
#
#------------------------------------------------------------------------
# Descriptive Statistik
# - sortierte Liste der ICD10.codes in geom bar und pie chart (zum Vergleich)
# - Top Ten der selektierten Infektionskrankheiten (Kapitel I und X)
# - sampled.codes.infect (nur für selektierte Krankheiten)
# - Unterscheidung Gast/Crew inkl. Altersverteilung und Geschlecht
# - Krankheitsfälle pro Region pro Schiff
#
# Regionale Betrachtung (Häfen)
# - pro code 1 desease map
#
# Saisonale Betrachtung
# - Zeitreihenanalyse (calendar weeks) plot einzelne Linien pro Krankheit pro Woche pro Schiff pro Route(?)
#
#------------------------------------------------------------------------
# Read Data
# (1) Bilde DB der ICD10 codes (WHO GM) 2018 von
#     www.dimdi.de (deutsche Version)  -> "01 - readICD10-GM-2018.R"
#
# (2) Einlesen der Daten
#     - Cases (incidents)              aus File "data/Cases.csv" 
#     - Schiffsfahrplan                aus File "Timetable.csv
#     - Ports                          aus File "Ports.csv"
#     - Touren (inkl. Passagierzahlen) aus File "ToursPax.csv"
#
# (3) Verknüpfen der Cases mit Daten mit ICD10 codes zu einem Datensatz "cases"
#     - Join von ICD10 Chapters, Groups und Codes (nur 3 digit codes!)
#     - filtere alle Daten ohne GeoInformation (longitude, latitude) heraus,
#       da die Arbeit sich auf regionale Infektionsrisiken beschränkt
#
# (4) Verknüpfe GeoDaten, Passagierzahlen etc. zu Schiffsfahrplan (timetable.day)
#     (Wo befindet sich welches Schiff an welchem Tag in welcher Region)
#     - Join von Port und Tour Number zu "Timetable"
#     - Join der GeoInformationen
#     - Join der Route und der Region, sowie der Passagierzahlen (Pax)
#     - Join der Crew-Zahlen, abgeschätzt durch max. Capa bei Vollbesetzung
#
#------------------------------------------------
# some useful functions 
#------------------------------------------------
nrDays <- function(x) {return(length(unique(x)))}

#------------------------------------------------
# Crew Zahlen pro Schiff
#
# MS1:  850
# MS2:  850
# MS3: 1040
# MS4: 1040
# MS5: 1040
# MS6: 1040
#
#--------------------------------
#                                    capacity for passenger needs to added
Ships <- tribble(~Schiff, ~CrewNr, ~MaxPaxNr, ~ComissioningYear, ~DecommisioningYear,
                 "MS1"  ,     850,      1924,              2009,                2018,
                 "MS2"  ,     850,      1912,              2011,                  NA,
                 "MS3"  ,    1040,      2506,              2014,                  NA,
                 "MS4"  ,    1040,      2506,              2015,                  NA,
                 "MS5"  ,    1040,      2534,              2016,                  NA,
                 "MS6"  ,    1040,      2534,              2017,                  NA) %>%
  mutate(Schiff = as.factor(Schiff),
         TotalMaxNr = CrewNr + MaxPaxNr)

#
#--------------------------------
# Load maps
#
# source("97 - loadLocalMaps.R", encoding = "UTF-8")
source("97 - loadLocalMaps.R")


##########################################################################################
#
# (1) Bilde DB der ICD10 codes (WHO) 2016
#     - icd10.chapters (tibble)
#     - icd10.groups   (tibble)
#     - icd10.codes    (tibble)
#
##########################################################################################

# Alternativ WHO version, aber dort fehlen D90, I84, O09 und T89
# source('01 - readICD10-WHO.R')
source('96 - readICD10-GM-2018.R')


##########################################################################################
#
# (2) Einlesen der Daten
#     - Cases (incidents)              aus File "data/Cases.csv" 
#     - Schiffsfahrplan                aus File "Timetable.csv
#     - Ports                          aus File "Ports.csv"
#     - Touren (inkl. Passagierzahlen) aus File "ToursPax.csv"
#
##########################################################################################

# Cases (incidents) 
Cases.raw <- read_delim("data/Cases.csv",";", 
                    escape_double = FALSE, 
                    col_types = cols(
                      Datum = col_date(format = "%Y-%m-%d"),
                      Schiff = col_factor(levels = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6"))
                    ), 
                    trim_ws = TRUE)
Cases.raw <- Cases.raw %>% mutate(PaxStatus = if_else(PaxStatus == "0", "Crew", "Pax", "NA"))
# Schiffsfahrplan
Timetable.raw <- read_delim(
  "data/Timetable.csv",
  ";",
  escape_double = FALSE,
  col_types = cols(
    Datum  = col_date(format = "%Y-%m-%d"),
    Schiff = col_factor(levels = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6")),
    `Turn or Call` = col_factor(levels = c("T", "C", "D"))
  ),
  trim_ws = TRUE
)

# Ports 
Ports <- read_delim("data/Ports-corrected.csv",
                    ";",
                    escape_double = FALSE,
                    trim_ws = TRUE)
# remove unnessecary columns (like adminName1, fcode, etc.)
Ports <- Ports %>% select("Port Name", "lng", "lat", "geoName", "countryCode", "countryName", "Region")

# Touren (inkl. Passagierzahlen)
ToursPax <- read_delim(
  "data/ToursPax.csv",
  ";",
  escape_double = FALSE,
  col_types = cols(
    EndDate = col_date(format = "%d.%m.%Y"),
    Schiff = col_factor(levels = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6")),
    StartDate = col_date(format = "%d.%m.%Y")
  ),
  trim_ws = TRUE
)
# - Region wurde aus der TourPax Datei extrahiert. Diese enthält jedoch alle Transits
#   Transits sind jedoch noch nicht berücksichtigt, daher erst einmal herausnehmen!
ToursPax <- select(ToursPax, -Region)

##########################################################################################
#
# (3) Verknüpfen der Cases mit Daten aus den Files zu einem Datensatz "cases"
#     - Join von ICD10 Chapters, Groups und Codes (nur 3 digit codes!)
#     - filtere alle Daten im Analysezeitraum 01.01.2015 - 31.12.2017
#
##########################################################################################

cases <- Cases.raw %>%
  #     - Konvertiere ICD10 zum 3 digit code von icd10.codes
  mutate(Code.ID = str_sub(ICD10, start=1, end=3))     %>%
  #  - Join von ICD10 Chapters, Groups und Codes (nur 3 digit codes!)
  left_join(icd10.codes, by=c("Code.ID"))              %>%
  # filtere alle Daten im Analysezeitraum 01.01.2015 - 31.12.2017
  dplyr::filter(Datum >= "2015-01-01" & Datum < "2018-01-01")


##########################################################################################
#
# (4) Verknüpfe GeoDaten, Passagierzahlen etc. zu Schiffsfahrplan (Timetable)
#     (Wo befindet sich welches Schiff an welchem Tag in welcher Region)
#     - Join von Port und Tour Number zu "Timetable"
#     - Join der GeoInformationen
#     - Join der Route und der Region, sowie der Passagierzahlen (Pax)
#     - Join der Crew-Zahlen, abgeschätzt durch max. Capa bei Vollbesetzung
#
##########################################################################################

# - Passagierzahlen pro Tag (PersonenTage)
timetable.day <- select(Timetable.raw, Datum, Schiff, TourNr, `Port Name`, LocDesc) %>% 
  dplyr::filter(Datum >= "2015-01-01" & Datum < "2018-01-01")                       %>%
  left_join(select(Ports, `Port Name`, lng, lat, Region), by="Port Name")           %>%
  left_join(select(ToursPax, TourNr, PaxNr), by="TourNr")                           %>%
  # Crew Zahlen aus Ships
  left_join(select(Ships, Schiff, CrewNr), by="Schiff")                             %>%
  # Berechne die Gesamtanzahl der Personen an Board (PersNr)
  mutate(PersNr = PaxNr + CrewNr)                                                   %>%
  # sortiere Spalten
  select(Datum, Schiff, Region, `Port Name`, lng, lat, PaxNr, CrewNr,PersNr)        %>%
  # berechne die fortlaufende Anzahl der Tage, der Wochen und der Monate ab dem 1.1.2015
  mutate( Year  = year(Datum), 
          Month = (year(Datum) - year(min(Datum)))*12 + month(Datum),
          Day   = as.numeric(Datum - min(Datum)) + 1,
          Week  = trunc(Day/7) + 1
  )

##########################################################################################
#
#     Clean-Up Data
#
##########################################################################################

if (exists("Timetable.raw"))  rm("Timetable.raw")
if (exists("Cases.raw"))      rm("Cases.raw")
# if (exists("icd10.chapters")) rm("icd10.chapters")
# if (exists("icd10.codes"))    rm("icd10.codes")
# if (exists("icd10.groups"))   rm("icd10.groups")
