################################################################################
#
# (1) read Passenger Numbers for Trips
#     
# (2) read (de-duplicated) mapping trips
#     mapping contains
#     - TravelNumberTrip -> TripNumber
#     - TravelNumberDesc -> TripDescription
#       typically in the form <harbor-start>/<harbor-end> of trip, but
#       sometimes it's just a description like "Karibik trifft Mittelmeer II" 
#       or "Kurzreise Lissabon & Ibizza"
#     - DateStart
#     - DateEnd
#     
#----------------------------------------------------------------------

# (1) read Passenger Numbers for Trips
source('C:/Users/user/R-Workspace/Bachelor/readPassengers.R')

# (2) read (de-duplicated) mapping trips
Trip.tmp <- read_csv("data/mapping_cruises_start_end.csv", 
                     col_types = cols(TravelNumberMonthDateEnd   = col_date(format = "%Y-%m-%d"), 
                                      TravelNumberMonthDateStart = col_date(format = "%Y-%m-%d")))
names(Trip.tmp) <- c("TripNumber", "TripDescription", "StartDate", "EndDate")
Trip.tmp <- 
  Trip.tmp %>% 
  
  separate(TripNumber, c("Schiff", "Year", "SeqNr"), sep=c(3,5), remove=TRUE) %>%
  mutate(Year = as.numeric(Year)+2000 )              %>%
  mutate(Schiff = str_replace(Schiff, "MSD", "MS3")) %>%
  mutate(Schiff = str_replace(Schiff, "MSF", "MS5")) %>%
  mutate(Schiff = str_replace(Schiff, "MSS", "MS6")) %>%
  mutate(Schiff = str_replace(Schiff, "MSV", "MS4")) %>%
  mutate(Schiff = str_replace(Schiff, "MSZ", "MS2")) %>%
  mutate(Schiff = str_replace(Schiff, "PRE", "MS1")) %>%
# build factor for Schiff  
  mutate(Schiff = as.factor(Schiff))                 %>%
# Join passenger numbers and route
  full_join(Trips.Pax[,c("Schiff", "Year", "SeqNr", "Route", "PaxNr")], by=c("Schiff", "Year", "SeqNr")) %>%
# build TripNumber
  mutate(TripNumber = str_c(Schiff,Year,SeqNr, sep="-")) 
  
# reorder columns
 Trips <- Trip.tmp[,c(9,1:3,5:6,4,7:8)]



# clean up memory
rm(Trip.tmp)
  






