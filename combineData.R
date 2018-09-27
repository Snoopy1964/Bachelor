##########################################################################################
#
# combine yearly/seasonal data from Excel and write to single csv files (separated by ";")
# - Read Passeneger numbers
# - Read Trips (and de-duplicate them!)
# - Read Tours
# - Read Incidents
#
# this Script needs to be executed only once (or when the input excel changes)
# main purpose is to have an easy to read input data and to easyly correct data entries 
# (e.g.some of the port names of the Tours cannot be used in geonames to get (long,lat)
# an example is "Southampton (London) / England" -> "Southampton / England"
#
######################################################################
#
# read Passenger Numbers (Pax) for Cruises
#
######################################################################

Trip.Pax.MS1 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "PRE", range = "A5:B159")
Trip.Pax.MS2 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "MSZ", range = "A5:B182")
Trip.Pax.MS3 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "MSD", range = "A5:B166")
Trip.Pax.MS4 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "MSV", range = "A5:B115")
Trip.Pax.MS5 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "MSF", range = "A5:B67")
Trip.Pax.MS6 <- read_excel("data/Passagierzahlen/Pax pro Reise.xlsx", sheet = "MSS", range = "A5:B29")

Trip.tmp <- rbind(Trip.Pax.MS1, Trip.Pax.MS2, Trip.Pax.MS3, Trip.Pax.MS4, Trip.Pax.MS5, Trip.Pax.MS6)
#
# correct data set manually:
# row 477 contains 2 times " - " (MSD1728 - SCHALLWELLEN - FESTIVAL AUF SEE")
# this leads to a conversion error, when " - " is used as separator
#
Trip.tmp[[477,1]] <- "MSD1728 - SCHALLWELLEN, FESTIVAL AUF SEE"

# adjust names
names(Trip.tmp) <- c("TripDescription", "PaxNr")

# write to csv
write.csv2(Trip.tmp, "./data/Passengers.csv", row.names=FALSE, fileEncoding = "UTF-8")

# remove temp. objects
rm(list = ls(pattern = glob2rx("Trip.*")))

######################################################################
# Read Trips,
# de-duplicate and write back to csv
#----------------------------------------------------------------------
#     mapping contains
#     - TravelNumberTrip 
#       Every trip is splitted at the end of the month and needs to be combined
#     - TravelNumberDesc
#       typically in the form <harbor-start>/<harbor-end> of trip, but
#       sometimes it's just a description like "Karibik trifft Mittelmeer II" 
#       or "Kurzreise Lissabon & Ibizza"
#     - DateStart
#     - DateEnd
#----------------------------------------------------------------------
fileName.Mapping      <- "data/Passagierzahlen/Mapping Cruise Start End.xlsx"
fileName.deDuplecated <- "data/mapping_cruises_start_end.csv"
#----------------------------------------------------------------------

Trip.tmp <- read_excel(fileName.Mapping,
                       range = "A1:D946", 
                       col_types = c("text", "text", "date", "date"))

Trip.old <- Trip.tmp[[1,1]]
for (i in 2:length(Trip.tmp$TravelNumberTrip)) {
  Trip.new <- Trip.tmp[[i,1]]
  if(Trip.old == Trip.new){
    # print(sprintf("i: %3i, old: %s, new: %s",i, Trip.old, Trip.new))
    # print(Trip.tmp[i-1,])
    # print(Trip.tmp[i,])
    Trip.tmp[[i  ,3]] <- Trip.tmp[[i-1,3]]   # set start date from former row
    Trip.tmp[[i-1,4]] <- Trip.tmp[[i  ,4]]   # set former end date to current row
    # print("#----------------")
    # print(Trip.tmp[i-1,])
    # print(Trip.tmp[i,])
  }
  Trip.old <- Trip.new
}
Trip.Mapping <- unique(Trip.tmp)
# convert Datum from Type Datetime to Date (readxl is always converting to Datetime)
Trip.Mapping[[3]] <- as.Date(Trip.Mapping[[3]])
Trip.Mapping[[4]] <- as.Date(Trip.Mapping[[4]])


# write_excel_csv(Trip.Mapping, fileName.deDuplecated)
write.csv2(Trip.Mapping, 
           fileName.deDuplecated, 
           quote        = FALSE,
           row.names    = FALSE,
           fileEncoding ="UTF-8")


# clean up memory
rm(list = ls(pattern = glob2rx("*.tmp")))
rm(list = ls(pattern = glob2rx("fileName.*")))
rm("Trip.old", "Trip.new", "i", "Trip.Mapping")

##########################################################################################
#
# Read Tours and convert to single csv
#
##########################################################################################
#
# 2014-2015
#   Load data only from 1.4.2014 - 31.3.2015 
#                   row        8 - 373
#   Special treatments
#   - Excel contains Itinerary-Number in columns B,K and T, 
#     so don't read it, because all ohter xls do not contain this information
#   - all excels have column "PORT NAME /n name of ship", adjust it to "Port Name"
#
#----------------------------------------------------------------------
fileName.2014_2015 <- "data/Tours/ALL - Schedule Season 2014-2015.xlsx"
#----------------------------------------------------------------------

Tours.2014_2015.MS1 <- read_excel(fileName.2014_2015,
                                  range = "D8:J373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))
# construct names vector
tmp.names <- names(Tours.2014_2015.MS1)
tmp.names[1] <- "Datum"
tmp.names[4] <- "Port Name"

#---------------------------------------------------------

Tours.2014_2015.MS2 <- read_excel(fileName.2014_2015,
                                  range = "M34:S373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2014_2015.MS3 <- read_excel(fileName.2014_2015,
                                  range = "V81:AB373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
# adjust names
#---------------------------------------------------------
names(Tours.2014_2015.MS1) <- tmp.names
names(Tours.2014_2015.MS2) <- tmp.names
names(Tours.2014_2015.MS3) <- tmp.names


#----------------------------------------------------------------------
#2015-2016
#   Load data only from 1.4.2015 - 31.03.2016 (Schaltjahr) 
#                   row        8 - 374
#
#----------------------------------------------------------------------
fileName.2015_2016 <- "data/Tours/ALL - Schedule Season 2015-2016.xlsx"
#----------------------------------------------------------------------

Tours.2015_2016.MS1 <- read_excel(fileName.2015_2016,
                                  # adjust because of missing data in the 2016-2017 file 
                                  range = "C9:I380", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2015_2016.MS2 <- read_excel(fileName.2015_2016,
                                  range = "K9:Q378", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2015_2016.MS3 <- read_excel(fileName.2015_2016,
                                  range = "S9:Y389", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2015_2016.MS4 <- read_excel(fileName.2015_2016,
                                  range = "AA73:AG376", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
# adjust names
#---------------------------------------------------------
names(Tours.2015_2016.MS1) <- tmp.names
names(Tours.2015_2016.MS2) <- tmp.names
names(Tours.2015_2016.MS3) <- tmp.names
names(Tours.2015_2016.MS4) <- tmp.names

#----------------------------------------------------------------------
# 2016-2017
#   Load data only from 1.4.2016 - 31.3.2017 
#                   row        8 - 373
#
#----------------------------------------------------------------------
fileName.2016_2017 <- "data/Tours/ALL - Schedule Season 2016-2017.xlsx"
#----------------------------------------------------------------------

Tours.2016_2017.MS1 <- read_excel(fileName.2016_2017,
                                  range = "C15:I373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2016_2017.MS2 <- read_excel(fileName.2016_2017,
                                  range = "K13:Q373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2016_2017.MS3 <- read_excel(fileName.2016_2017,
                                  range = "S24:Y373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2016_2017.MS4 <- read_excel(fileName.2016_2017,
                                  range = "AA11:AG373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2016_2017.MS5 <- read_excel(fileName.2016_2017,
                                  range = "AI115:AO373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
# adjust names
#---------------------------------------------------------
names(Tours.2016_2017.MS1) <- tmp.names
names(Tours.2016_2017.MS2) <- tmp.names
names(Tours.2016_2017.MS3) <- tmp.names
names(Tours.2016_2017.MS4) <- tmp.names
names(Tours.2016_2017.MS5) <- tmp.names

#----------------------------------------------------------------------
# 2017-2018
#   Last year, take all availbale days
#   Special treatments
#     Arrival and departure times are named "Published Arr" and "Published Dep" instead of Arr and Dep
#
#----------------------------------------------------------------------
fileName.2017_2018 <- "data/Tours/ALL - Schedule Season 2017-2018.xlsx"
#----------------------------------------------------------------------

Tours.2017_2018.MS1 <- read_excel(fileName.2017_2018,
                                  range = "C9:I384", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2017_2018.MS2 <- read_excel(fileName.2017_2018,
                                  range = "K9:Q378", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2017_2018.MS3 <- read_excel(fileName.2017_2018,
                                  range = "S9:Y396", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2017_2018.MS4 <- read_excel(fileName.2017_2018,
                                  range = "AA9:AG381", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2017_2018.MS5 <- read_excel(fileName.2017_2018,
                                  range = "AI9:AO383", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
Tours.2017_2018.MS6 <- read_excel(fileName.2017_2018,
                                  range = "AQ47:AW391", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"),
                                  col_names = FALSE)

#---------------------------------------------------------
# adjust names
#---------------------------------------------------------
names(Tours.2017_2018.MS1) <- tmp.names
names(Tours.2017_2018.MS2) <- tmp.names
names(Tours.2017_2018.MS3) <- tmp.names
names(Tours.2017_2018.MS4) <- tmp.names
names(Tours.2017_2018.MS5) <- tmp.names
names(Tours.2017_2018.MS6) <- tmp.names

#-----------------------------------------------------------------------------------------
# concatenate ships
#-----------------------------------------------------------------------------------------
# tmp.ms1 <- ls(pattern = glob2rx("Tours.*.MS1"))
# tmp.ms2 <- ls(pattern = glob2rx("Tours.*.MS2"))
# tmp.ms3 <- ls(pattern = glob2rx("Tours.*.MS3"))
# tmp.ms4 <- ls(pattern = glob2rx("Tours.*.MS4"))
# tmp.ms5 <- ls(pattern = glob2rx("Tours.*.MS5"))
# tmp.ms6 <- ls(pattern = glob2rx("Tours.*.MS6"))
# 
# Tours.MS1 <- tmp.ms1[1] 
# for (season in 2:length(tmp.ms1)) {
#   Tours.MS1 <- union(Tours.MS1, tmp.ms1[season])
# }
Tours.MS1 <- rbind(Tours.2014_2015.MS1,Tours.2015_2016.MS1, Tours.2016_2017.MS1, Tours.2017_2018.MS1)
Tours.MS1 <- cbind(Schiff=rep("MS1", length(Tours.MS1[1])), Tours.MS1)

Tours.MS2 <- rbind(Tours.2014_2015.MS2,Tours.2015_2016.MS2, Tours.2016_2017.MS2, Tours.2017_2018.MS2)
Tours.MS2 <- cbind(Schiff=rep("MS2", length(Tours.MS2[1])), Tours.MS2)

Tours.MS3 <- rbind(Tours.2014_2015.MS3,Tours.2015_2016.MS3, Tours.2016_2017.MS3, Tours.2017_2018.MS3)
Tours.MS3 <- cbind(Schiff=rep("MS3", length(Tours.MS3[1])), Tours.MS3)

Tours.MS4 <- rbind(Tours.2015_2016.MS4, Tours.2016_2017.MS4, Tours.2017_2018.MS4)
Tours.MS4 <- cbind(Schiff=rep("MS4", length(Tours.MS4[1])), Tours.MS4)

Tours.MS5 <- rbind(Tours.2016_2017.MS5, Tours.2017_2018.MS5)
Tours.MS5 <- cbind(Schiff=rep("MS5", length(Tours.MS5[1])), Tours.MS5)

Tours.MS6 <- Tours.2017_2018.MS6
Tours.MS6 <- cbind(Schiff=rep("MS6", length(Tours.MS6[1])), Tours.MS6)

# concatenate all Tours into one Tibble
Tours <- as_tibble(rbind(Tours.MS1, Tours.MS2, Tours.MS3, Tours.MS4, Tours.MS5, Tours.MS6))

# convert Datum <dttm> into <Date>
Tours <- Tours %>% mutate(Datum = format.Date(as.Date(Datum), "%d.%m.%Y"))

# save combined file to csv
write.csv2(Tours, "./data/TourTimetable.csv", 
           row.names=FALSE, 
           quote = FALSE,
           fileEncoding = "UTF-8")

# clean up memory
rm(list = ls(pattern = glob2rx("Tours.*")))
rm(list = ls(pattern = glob2rx("fileName.*")))
rm(tmp.names, Tours)

##########################################################################################
#
# Read Cases (Incidents)
#
##########################################################################################
ds.2012 <- read_delim("data/Incidents/2012.csv",";", 
                      escape_double = FALSE, 
                      col_types = cols(Crew = col_logical(),
                                       Datum = col_date(format = "%d.%m.%Y")), trim_ws = TRUE)
ds.2013 <- read_delim("data/Incidents/2013.csv",";", 
                      escape_double = FALSE, 
                      col_types = cols(Crew = col_logical(),
                                       Datum = col_date(format = "%d.%m.%Y")), trim_ws = TRUE)
ds.2014 <- read_delim("data/Incidents/2014.csv",";", 
                      escape_double = FALSE, 
                      col_types = cols(Crew = col_logical(),
                                       Datum = col_date(format = "%d.%m.%Y")), trim_ws = TRUE)
ds.2015 <- read_delim("data/Incidents/2015.csv",";", 
                      escape_double = FALSE, 
                      col_types = cols(Crew = col_logical(),
                                       Datum = col_date(format = "%d.%m.%Y")), trim_ws = TRUE)
ds.2016 <- read_delim("data/Incidents/2016.csv",";", 
                      escape_double = FALSE, 
                      col_types = cols(Crew = col_logical(),
                                       Datum = col_date(format = "%d.%m.%Y")), trim_ws = TRUE)
ds.2017 <- read_delim("data/Incidents/2017.csv",";", 
                      escape_double = FALSE, 
                      col_types = cols(Crew = col_logical(),
                                       Datum = col_date(format = "%d.%m.%Y")), trim_ws = TRUE)
ds.2018 <- read_delim("data/Incidents/2018.csv",";", 
                      escape_double = FALSE, 
                      col_types = cols(Crew = col_logical(),
                                       Datum = col_date(format = "%d.%m.%Y")), trim_ws = TRUE)

ds.tmp <- rbind(ds.2012, ds.2013, ds.2014, ds.2015, ds.2016, ds.2017, ds.2018)

# save combined file to csv
write.csv2(ds.tmp, "./data/ds.csv", row.names=FALSE, fileEncoding = "UTF-8")

# clean up memory
rm(list = ls(pattern = glob2rx("ds.*")))
