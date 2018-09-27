##########################################################################################
#
# Read Tours (combined file from combineTours.R) -> this file contains the raw data for 
# Port Names, which are very messy. The Names need to be manually corrected (at the moment)
# to be used with geoname::GNSearch().
# In addition 
# - Some entries for "Wetdock" needs to be corrected, e.g. MS1, 2016-05-10 - 2016-05-13 
# - Tours for MS3 2017-09-01 ends not at 2017-09-06, but on 2017-09-05
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Use Tours_corrected.csv only
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Tours_corrected.csv contains the full timetable of the tours including information per day
# 
#   (1) read timetable of tours
#   (2) perform data cleansing
#       - remove entries with `Day number` is NA -> missing information
#       - assign entries with `Port Name "at Sea", "Shipyard", etc. the last known port
#   (3) add geo location information for timetable
#   (4) extract Tours from timetable 
#       - select "Turn days" (attribute `Turn or Call` = T)
#       - "tidyfy" data by ... (see below)
#   (5) correlate Tours to TripNumbers of Trips
#       Trips file is buggy: e.g. contains unknown routes, missing StartDate information, etc.
#       - try to combine information from Tours and Trips
#   (6) add Tour and Route to Tour.timetable
#
# ToDo's:
#   - 
#
##########################################################################################

#
#----------------------------------------------------------------------
fileName <- "data/Tours_corrected.csv"
#----------------------------------------------------------------------
#   (1) read timetable of tours
#----------------------------------------------------------------------
Tours.tmp <- read_delim(fileName, ";",
                    escape_double = FALSE, 
                    col_types = cols(Datum  = col_date(format = "%d.%m.%Y"),
                                     Schiff = col_factor(levels = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6"))),
                    trim_ws = TRUE)

# Read Ports (extracted from Tours_corrected.csv)
Ports <- read_delim("data/Ports.csv", 
                    ";",
                    escape_double = FALSE, 
                    col_types = cols(lat = col_double(),
                                     lng = col_double()), trim_ws = TRUE)
# remove unnessecary columns (like adminName1, fcode, etc.)
Ports <- Ports %>% select("Port Name", "lng", "lat", "name", "countryCode", "countryName")

#   (2) perform data cleansing
#       - remove entries with `Day number` is NA -> missing information
#       - assign entries with `Port Name "at Sea", "Shipyard", etc. the last known port
#   (3) add geo location information for timetable

Repairs <- c("Drydock", 
             "Wetdock",
             "Shipyard", 
             "Test, 5 days")
NoPorts <- Repairs %>% append("At Sea")

Tour.timetable <- Tours.tmp                 %>% 
  # remove rows with NA entry in "Day number"
  dplyr::filter(!is.na(`Day number`))       %>% 
  # duplicate "Port Name" as "LocDesc" (Location Description) 
  mutate(LocDesc = `Port Name`)             %>%
  # replace "Drydock", "at Sea", "Shipyard", and "Test, 5 days" with previous "Port Name" (replace to NA, and then use fill())
  mutate(`Port Name` = ifelse(`Port Name` %in% NoPorts, NA, 
                              `Port Name`)) %>%
  
  fill(`Port Name`)                         %>%
  # set "Dock or Anchor" to "S" (Shipyard), if value is NA
  mutate(`Dock or Anchor` = ifelse(is.na(`Dock or Anchor`), "S", `Dock or Anchor`)) %>% 
  # for Repairs replace first entry of LocDesc
  mutate(LocDesc = ifelse(lead(LocDesc) %in% Repairs & `Turn or Call` == "T", lead(LocDesc), LocDesc )) %>%
  # join with Ports to get (long, lat) and countryCodes
  left_join(select(Ports,`Port Name`,long=lng,lat,name,countryCode), by="Port Name") %>%
  # order by "Datum" (original name "CALL DATE") and switch columns "Schiff" and "Datum" because of primary key condition
  select(Datum,Schiff,`Port Name`, long,lat,countryCode, name, LocDesc, `Day number`,`Dock or Anchor`, `Turn or Call`)
  # %>%
  # # filter for data in 2015, 2016 and 2017 only
  # dplyr::filter(Datum >= "2015-01-01" & Datum < "2018-01-01")


#-------------------------------------------------------------
#   (4) extract Tours from timetable 
#       - select "Turn days" (attribute `Turn or Call` = T)
#       - "tidyfy" data by 
#           - getting tour start and end
#           - extracting the number of days
#           - getting start and end port
Tour.attributes <- c(1,2,3,6,9,11)
Tours.tmp <- Tour.timetable             %>% 
  dplyr::filter(`Turn or Call` == "T" ) %>%
  arrange(Schiff, Datum)                %>%
  # calculate duration of Route and start/end dates/ports
  separate(`Day number`, into=c("dayNr.end.tmp", "dayNr.start.tmp"), sep="/", fill="left", remove=FALSE) %>%
  mutate(DurationDays = as.integer(lead(dayNr.end.tmp)), 
         StartDate    = Datum, 
         EndDate      = lead(Datum),
         StartPort    = `Port Name`,
         EndPort      = lead(`Port Name`),
         StartName    = name,
         EndName      = lead(name))                  %>%
  # some entries had wrong `Day number` attributes on Turn-over days (e.g. 0 instead of 14/0). These has been corrected in Tours_corrected file
  # in that case the duration could also be calculated based on Datum: Datum(i+1) - Datum(i) - see below.
  select(Schiff, StartDate, EndDate, StartPort, EndPort, StartName, EndName, DurationDays)  %>% 
  # remove all entries with EndDate = NA -> these are the last available Tours in Dataset without a follow-up Tour
  dplyr::filter(!is.na(EndDate))

#-------------------------------------------------------------
#   (5) correlate Tours to TripNumbers of Trips
#       Trips file is buggy: e.g. contains unknown routes, missing StartDate information, etc.
#       - try to combine information from Tours and Trips
#-------------------------------------------------------------
Tours <- Tours.tmp                                                    %>%
  left_join(Trips, by = c("Schiff", "StartDate", "EndDate"))          %>%
  # # filter data to be in 2015, 2016 and 2017
  # dplyr::filter(StartDate >= "2015-01-01" & StartDate < "2018-01-01") %>%
  # select columns
  select("TourNr", "Schiff", "Year", "StartDate", "EndDate", "DurationDays", "Route", "PaxNr" )                             %>%
  # sort bei Schiff and StartDate
  arrange(Schiff, StartDate)                         


#-------------------------------------------------------------
#   (6) add Tour and Route to Tour.timetable
#-------------------------------------------------------------
Tour.timetable <- Tour.timetable %>% 
  left_join(Tours, by=c("Schiff", "Datum" = "StartDate")) %>% 
  fill(TourNr,Year,EndDate, DurationDays, Route, PaxNr)   %>%
  dplyr::filter(Datum >= "2015-01-01" & Datum < "2018-01-01")

#-------------------------------------------------------------
# clean up memory
#-------------------------------------------------------------
rm("fileName")
rm(Tours.tmp)
# if(exists("Trips"))     rm("Trips")
# if(exists("Trips.Pax")) rm("Trips.Pax")



