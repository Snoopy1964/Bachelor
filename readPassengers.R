######################################################################
#
# read Passenger Numbers (Pax) for Cruises
# - extract TripNumber, Schiff and Year from TripDescription 
#
######################################################################

Trip.tmp <- read_delim("data/Passengers.csv", 
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
  mutate(TripNumber = str_c(Schiff,Year,SeqNr, sep="-")) 

# sort columns
Trips.Pax <- Trips.Pax[,c(6,1:5)]

names(Trips.Pax)[6] <- "PaxNr"

rm(Trip.tmp)