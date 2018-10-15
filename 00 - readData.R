##########################################################################################
#
# Definitionen
# - Route: 
#     entspricht eine Abfolge von Häfen, enthält keine konkreten Zeitpunkte, 
#     kann von verschiedenen Schiffen abgefahren werden. 
#
# - Tour (Trip): 
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
# - Krankheitsfälle pro Tour pro Schiff
#
# Regionale Betrachtung (Häfen)
# - pro code 1 desease map
#
# Saisonale Betrachtung
# - Zeitreihenanalyse (calendar weeks) plot einzelne Linien pro Krankheit pro Woche pro Schiff pro Route(?)
#
#------------------------------------------------------------------------
# Read Data
# (1) Bilde DB der ICD10 codes (WHO) 2016 von
#     www.dimdi.de (deutsche Version)  -> "01 - readICD10.R"
# (2) Einlesen der Daten
#     - Cases (incidents)              aus File "data/Cases.csv" 
#     - Schiffsfahrplan                aus File "Timetable.csv
#     - Ports                          aus File "Ports.csv"
#     - Touren (inkl. Passagierzahlen) aus File "ToursPax.csv"
# (3) Verknüpfen der Cases mit Daten aus den Files zu einem Datensatz "ds.all"
#     - Join von ICD10 Chapters, Groups und Codes (nur 3 digit codes!)
#     - Join von Port und Tour Number zu "ds.all"
#     - Join der GeoInformationen
#     - Join der Route und der Region, sowie der Passagierzahlen (Pax)
#     - Join der Crew-Zahlen, abgeschätzt durch max. Capa bei Vollbesetzung
#
# (4) filtere alle Daten ohne GeoInformation (longitude, latitude) heraus,
#     da die Arbeit sich auf regionale Infektionsrisiken beschränkt
#
#
#--------------------------------
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
source("97 - loadLocalMaps.R", encoding = "UTF-8")


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
source('01 - readICD10-GM-2018.R')


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
Cases <- read_delim("data/Cases.csv",";", 
                    escape_double = FALSE, 
                    col_types = cols(
                      Datum = col_date(format = "%Y-%m-%d"),
                      Schiff = col_factor(levels = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6"))
                    ), 
                    trim_ws = TRUE)

# Schiffsfahrplan
Timetable <- read_delim(
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
# (3) Verknüpfen der Cases mit Daten aus den Files zu einem Datensatz "ds.all"
#     - Join von ICD10 Chapters, Groups und Codes (nur 3 digit codes!)
#     - Join von Port und Tour Number 
#     - Join der GeoInformationen
#     - Join der Route und der Region, sowie der Passagierzahlen (Pax)
#     - Join der Crew-Zahlen, abgeschätzt durch max. Capa bei Vollbesetzung
#
##########################################################################################

ds.all <- Cases %>%
  #     - Konvertiere ICD10 zum 3 digit code von icd10.codes
  mutate(Code.ID = str_sub(ICD10, start=1, end=3))     %>%
  #  - Join von ICD10 Chapters, Groups und Codes (nur 3 digit codes!)
  left_join(icd10.codes, by=c("Code.ID"))              %>%
  #  - Join von Port und Tour Number
  left_join(Timetable[,c("Datum", "Schiff", "TourNr", "Port Name")], by=c("Datum", "Schiff")) %>%
  #  - Join der GeoInformationen
  left_join(Ports, by="Port Name")                     %>%
  #  - Join der Route und der Region, sowie der Passagierzahlen (Pax)
  # left_join(select(ToursPax, -Year, -Region), by=c("TourNr", "Schiff"))        %>%
  left_join(select(ToursPax, -Year), by=c("TourNr", "Schiff"))        %>%
  #  - Join der Crew-Zahlen, abgeschätzt durch max. Capa bei Vollbesetzung
  left_join(select(Ships,"Schiff","CrewNr"),by="Schiff")    %>%
  #  - Berechne die Gesamtanzahl der Personen an Board (TotalNr)
  mutate(TotalNr = PaxNr + CrewNr)

##########################################################################################
#
# (4) filtere alle Daten ohne GeoInformation (longitude, latitude) heraus,
#     da die Arbeit sich auf regionale Infektionsrisiken beschränkt
#
##########################################################################################

ds.loc <- ds.all %>% dplyr::filter(!is.na(lng))

