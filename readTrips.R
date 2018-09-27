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
source('readPassengers.R')

# (2) read (de-duplicated) mapping trips
# Trip.tmp <- read_csv("data/mapping_cruises_start_end.csv",
#                      col_types = cols(TravelNumberMonthDateEnd   = col_date(format = "%Y-%m-%d"),
#                                       TravelNumberMonthDateStart = col_date(format = "%Y-%m-%d")))
Trip.tmp <- read_delim(
  "data/mapping_cruises_start_end.csv", 
  ";",
  escape_double = FALSE,
  col_types = cols(TravelNumberMonthDateEnd   = col_date(format = "%Y-%m-%d"),
                   TravelNumberMonthDateStart = col_date(format = "%Y-%m-%d")),
  trim_ws = TRUE)
# Example
# # A tibble: 738 x 4
# TravelNumberTrip TravelNumberDesc          TravelNumberMonthDateStart TravelNumberMonthDateEnd
# <chr>           <chr>                      <date>                     <date>                  
# 1 MSD1402       Hamburg/Palma de Mallorca  2014-06-13                 2014-06-22              
# 2 MSD1403       Palma de Mallorca/Valletta 2014-06-22                 2014-06-29              
# 3 MSD1404       Valletta/Valletta          2014-06-29                 2014-07-06  
# 4 .......

names(Trip.tmp) <- c("TripNumber", "Trip.Route", "StartDate", "EndDate")

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
  arrange(Schiff, StartDate)                         %>%
  mutate(Trip.duration = as.numeric(EndDate - StartDate) )

# check for inconsistencies
# - EndDate of a trip needs to be equal to the StartDate of the next trip for one Schiff. 
#   Otherwise a trip is missing.
#   Exception: this was the last known Tour for a ship
#
Trip.check <- Trip.tmp %>% dplyr::filter(EndDate != lead(StartDate) & Schiff == lead(Schiff))
if(length(Trip.check[["Schiff"]]) > 0 ) { 
  print(Trip.check)
  View(Trip.tmp)
} else {
  rm(Trip.check)
}


Trips <- Trip.tmp                                                                                        %>%
  # Join passenger numbers and route
  full_join(Trips.Pax[,c("Schiff", "Year", "SeqNr", "Route", "PaxNr")], by=c("Schiff", "Year", "SeqNr")) %>%
  # build TripNumber
  mutate(TourNr = str_c(Schiff,Year,SeqNr, sep="-"), SeqNr = NULL)                                       %>%
  arrange(Schiff, StartDate)                                                                             %>%
  # if Trip.Route is set, but Route is NA -> Test after construction/repair -> no passengers on board 
  mutate(Route = ifelse(is.na(Route),"Repair", Route), PaxNr = ifelse(is.na(PaxNr), 0, PaxNr))           %>%
  # reorder columns
  select(TourNr, Schiff, Year,StartDate, EndDate,Trip.duration, Route, PaxNr)

Routes <- unique(Trips[["Route"]])

# clean up memory
rm(Trip.tmp)
  






