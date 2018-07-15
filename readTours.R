##########################################################################################
#
# Read Tours (combined file from combineTours.R) -> this file contains the raw data for 
# Port Names, which are very messy. The Names need to be manually corrected (at the moment)
# to be used with geoname::GNSearch().
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Please use Tours_corrected.csv only
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
##########################################################################################

#
#----------------------------------------------------------------------
fileName <- "data/Tours_corrected.csv"
#----------------------------------------------------------------------
Tours <- read_delim(fileName, ";",
                    escape_double = FALSE, 
                    col_types = cols(Datum  = col_date(format = "%d.%m.%Y"),
                                     Schiff = col_factor(levels = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6"))),
                    trim_ws = TRUE)

# Read Ports
Ports <- read_delim("data/Ports.csv", 
                    ";",
                    escape_double = FALSE, 
                    col_types = cols(lat = col_double(),
                                     lng = col_double()), trim_ws = TRUE)


# split "Port Name" into "Port" and "Country"
Tours <- Tours %>% 
  separate("Port Name", into = c("Port", "Country"), sep=" / ", remove=TRUE, extra="drop", fill="left") %>%
# remove all rows with no Country -> these are the missing/invalid data entries
  dplyr::filter(is.na(Country)==FALSE) %>% 
# the remaining missing "Port Name" values will be filled by the last known "Port Name" value
  mutate(Port = na.locf(Port)) 


# order by "Datum" (original name "CALL DATE") and switch columns "Schiff" and "Datum" because of primary key condition
Tours <- Tours[, c(2,1,3:9)] 
# Tours <- arrange(Tours, Datum, Schiff)

Tours[[1]] <- as.Date(Tours[[1]])

# prepare Tours for join with Trips 
Tours <- Tours %>% 
# identify the start of a new Trip -> Turn or Call = T
  mutate(TripStart = ifelse(`Turn or Call`== "T", TRUE, FALSE) ) %>%
# convert Datum from Type Datetime to Date (readxl is always converting to Datetime)
  mutate(Datum = as.Date(Datum))                                 %>%
# convert Schiff to factor
  mutate(Schiff = as.factor(Schiff))                             %>%
  left_join(select(Trips, TripNumber, Schiff, StartDate, TripDescription), by=c("Datum" = "StartDate", "Schiff" = "Schiff")) %>%
# fill missing values of TripNumbers for all days from previous value
  mutate(TripNumber = na.locf(TripNumber))


# clean up memory
rm("fileName")


