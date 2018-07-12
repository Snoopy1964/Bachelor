######################################################################
#
# read Passenger Numbers (Pax) for Cruises
#
#
#
#
######################################################################


Trip.Pax.MS1 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "PRE", range = "A5:B159")
Trip.Pax.MS2 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "MSZ", range = "A5:B182")
Trip.Pax.MS3 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "MSD", range = "A5:B166")
Trip.Pax.MS4 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "MSV", range = "A5:B116")
Trip.Pax.MS5 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "MSF", range = "A5:B68")
Trip.Pax.MS6 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "MSS", range = "A5:B30")

Trip.tmp <- rbind(Trip.Pax.MS1, Trip.Pax.MS2, Trip.Pax.MS3, Trip.Pax.MS4, Trip.Pax.MS5, Trip.Pax.MS6)
#
# correct data set manually:
# row 477 contains 2 times " - " (MSD1728 - SCHALLWELLEN - FESTIVAL AUF SEE")
# this leads to a conversion error, when " - " is used as separator
#
Trip.tmp[[477,1]] <- "MSD1728 - SCHALLWELLEN, FESTIVAL AUF SEE"


Trips.Pax <- Trip.tmp %>%
  separate(X__1, into = c("TripNum.tmp", "Route"), sep = " - ", remove=TRUE) %>%
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

names(Trips.Pax)[6] <- "Pax.Count"







rm(list = ls(pattern=glob2rx("Trip.Pax.*")))
rm(Trip.tmp)