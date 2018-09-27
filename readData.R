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
# - Do we need a region for Routes, e.g. Karibik, Mittelmehr, Nordroute, etc.
#   Wenn ja: welche Regionen?
# - Tours.timetable: check NA's in 2017-04-24 | MS1 | Valletta / Malta | 14.51472 | 35.89972 | MT | Valletta | NA
# - add commisioning (Inbetriebnahme) and decommisioning (Außerbetriebnahme) times to Ships
# - (done) ds: add Port, Tour, Route, PaxNr and CrewNr -> calculate TotalNr = CrewNr + PaxNr
# - (done) Tours.timetable: add Tours, Routes and PaxNr
# - (done) get Route information for Tours
# - (done) write consistency check for Tour.Timetable -> 
#          for each ship check continous Date
# - (done) use ICD10 codes from www.dimdi.de (German version) instead of icd.data package
#          -> readICD10.R (called within readCases.R)
# - (done) Tour.timetable: LocDesc ist falsch (at Sea für MS1, 2016-04-22 steht At Sea an einem Turn-over Tag) 
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
# (2) read Trips          -> readTrips.R
# (3) read Tour.timetable -> readTours.R
# (4) read Cases (Incidents)
#
#!!!!!!!!!!!!!!! to be adapted, at the moment this is a pure brain dump of useful information and todo's 
# addtional information
#
# (2) read Passenger number (Pax) per Trip
#     key of the table is combined of the form
#     <TravelNumberTrip> - <TravelNumberDesc> of mapping trips
# (3) join tables

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
Ships <- tribble(~Schiff, ~CrewNr, ~MaxPaxNr,
                 "MS1"  ,     850,      1924,
                 "MS2"  ,     850,      1912,
                 "MS3"  ,    1040,      2506,
                 "MS4"  ,    1040,      2506,
                 "MS5"  ,    1040,      2534,
                 "MS6"  ,    1040,      2534) %>%
  mutate(Schiff = as.factor(Schiff),
         TotalMaxNr = CrewNr + MaxPaxNr)
#
#
#
##########################################################################################

# (1) read Trips -> readTrips.R
source('readTrips.R')


# (2) read timetable of Tours and extract Tours -> readTours.R
source('readTours.R')

##########################################################################################
#
# (3) Read Cases (Incidents)
#
##########################################################################################
source('readCases.R')
