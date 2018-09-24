##########################################################################################
#
# Read Tours (combined file from combineTours.R) -> this file contains the raw data for 
# Port Names, which are very messy. The Names need to be manually corrected (at the moment)
# to be used with geoname::GNSearch().
# In addition Some entries for "Wetdock" needs to be corrected, e.g. MS1, 2016-05-10 - 2016-05-13  
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
#   (6) extract Routes from Trips
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

#   (2) perform data cleansing
#       - remove entries with `Day number` is NA -> missing information
#       - assign entries with `Port Name "at Sea", "Shipyard", etc. the last known port
#   (3) add geo location information for timetable

Repairs <- c("Drydock", 
             "Wetdock",
             "Shipyard", 
             "Test, 5 days")
noPorts <- Repairs %>% append("At Sea")

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
  # for NoPorts replace first entry of LocDesc
  mutate(LocDesc = ifelse(lead(LocDesc) %in% NoPorts & `Turn or Call` == "T", lead(LocDesc), LocDesc)) %>%
  # join with Ports to get (long, lat) and countryCodes
  left_join(select(Ports,`Port Name`,long=lng,lat,name,countryCode), by="Port Name") %>%
  # order by "Datum" (original name "CALL DATE") and switch columns "Schiff" and "Datum" because of primary key condition
  select(Datum,Schiff,`Port Name`, long,lat,countryCode, name, LocDesc, `Day number`,`Dock or Anchor`, `Turn or Call`)

#-------------------------------------------------------------
#   (4) extract Tours from timetable 
#       - select "Turn days" (attribute `Turn or Call` = T)
#       - "tidyfy" data by 
#           - getting tour start and end
#           - extracting the number of days
#           - getting start and end port
Tour.attributes <- c(1,2,3,6,9,11)
Tours <- Tour.timetable             %>% 
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
  # mutate(duratio.Days = ifelse(is.na(dayNr.end.tmp), lead(Datum)-Datum,dayNr.end.tmp)) %>%
  select(Schiff, StartDate, EndDate, StartPort, EndPort, StartName, EndName, DurationDays) %>% 
  # remove all entries where duration.Days is NA -> these are the last available Tours in Dataset without a follow-up Tour
  dplyr::filter(!is.na(DurationDays))


#-------------------------------------------------------------
#   (5) correlate Tours to TripNumbers of Trips
#       Trips file is buggy: e.g. contains unknown routes, missing StartDate information, etc.
#       - try to combine information from Tours and Trips
Tours.tmp <- Tours %>%
  full_join(Trips, by = c("Schiff", "StartDate", "EndDate")) %>%
  arrange(Schiff, StartDate)                                 %>%
  # replace unknown "TripDescription" with combination "StartName/EndName"
  mutate(TripDescription = ifelse(is.na(TripDescription),str_c(StartName,EndName,sep="/"),TripDescription))


#----------------------------------------------------------------------
# clean up memory
#----------------------------------------------------------------------
rm("fileName")
# rm(Tours.tmp)


