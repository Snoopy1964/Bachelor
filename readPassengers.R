######################################################################
#
# read Passenger Numbers (Pax) for Cruises
# There are inconsistent Tripnumbers of the form MSS172123. 
# These incosistenecies have been manually corrected and stored in 
# the file data/Passengers_corrected.csv
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Use Passengers_corrected.csv only
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 
# - extract TripNumber, Schiff and Year from TripDescription 
#
######################################################################

Trip.tmp <- read_delim("data/Passengers_corrected.csv", 
                       ";", 
                       escape_double = FALSE, 
                       col_types = cols(PaxNr = col_integer(),
                                        TripDescription = col_character()),
                       trim_ws = TRUE)


Trips.Pax <- Trip.tmp %>%
  separate(TripDescription, into = c("TripNum.tmp", "Route"), sep = " - ", remove=TRUE) %>%
  separate(TripNum.tmp, c("Schiff", "Year", "SeqNr"), sep=c(3,5), remove=TRUE) %>%
  mutate(Year = as.numeric(Year)+2000 )              %>%
  mutate(Schiff = str_replace(Schiff, "MSD", "MS3")) %>%
  mutate(Schiff = str_replace(Schiff, "MSF", "MS5")) %>%
  mutate(Schiff = str_replace(Schiff, "MSS", "MS6")) %>%
  mutate(Schiff = str_replace(Schiff, "MSV", "MS4")) %>%
  mutate(Schiff = str_replace(Schiff, "MSZ", "MS2")) %>%
  mutate(Schiff = str_replace(Schiff, "PRE", "MS1")) %>%
  # build factor for Schiff  
  mutate(Schiff = as.factor(Schiff))                 %>%
  # build TripNumber
  mutate(TripNumber = str_c(Schiff,Year,SeqNr, sep="-")) %>%
  # sort bei Shiff, Year, and SeqNr
  arrange(Schiff, Year, SeqNr)                       %>%
  # sort columns
  select(TripNumber,Schiff, Year, SeqNr, Route, PaxNr)

rm(Trip.tmp)