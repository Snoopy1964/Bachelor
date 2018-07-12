##########################################################################################
#
# Read Tours
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
                                  range = "M8:S373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2014_2015.MS3 <- read_excel(fileName.2014_2015,
                                  range = "V8:AB373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
# adjust names
#---------------------------------------------------------
names(Tours.2014_2015.MS1) <- tmp.names
names(Tours.2014_2015.MS2) <- tmp.names
names(Tours.2014_2015.MS3) <- tmp.names


#----------------------------------------------------------------------
#2015-2016
#   Load data only from 1.4.2015 - 31.3.2016 (Schaltjahr) 
#                   row        8 - 374
#
#----------------------------------------------------------------------
fileName.2015_2016 <- "data/Tours/ALL - Schedule Season 2015-2016.xlsx"
#----------------------------------------------------------------------

Tours.2015_2016.MS1 <- read_excel(fileName.2015_2016,
                                  range = "C8:I374", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2015_2016.MS2 <- read_excel(fileName.2015_2016,
                                  range = "K8:Q374", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2015_2016.MS3 <- read_excel(fileName.2015_2016,
                                  range = "S8:Y374", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2015_2016.MS4 <- read_excel(fileName.2015_2016,
                                  range = "AA8:AG374", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

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
                                  range = "C8:I373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2016_2017.MS2 <- read_excel(fileName.2016_2017,
                                  range = "K8:Q373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2016_2017.MS3 <- read_excel(fileName.2016_2017,
                                  range = "S8:Y373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2016_2017.MS4 <- read_excel(fileName.2016_2017,
                                  range = "AA8:AG373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2016_2017.MS5 <- read_excel(fileName.2016_2017,
                                  range = "AI8:AO373", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

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
                                  range = "C8:I434", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2017_2018.MS2 <- read_excel(fileName.2017_2018,
                                  range = "K8:Q434", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2017_2018.MS3 <- read_excel(fileName.2017_2018,
                                  range = "S8:Y434", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2017_2018.MS4 <- read_excel(fileName.2017_2018,
                                  range = "AA8:AG434", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2017_2018.MS5 <- read_excel(fileName.2017_2018,
                                  range = "AI8:AO434", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

#---------------------------------------------------------
Tours.2017_2018.MS6 <- read_excel(fileName.2017_2018,
                                  range = "AQ8:AW434", 
                                  col_types = c("date", "text", "text",
                                                "text", "text", "text", "text"))

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

# extract Port Names
PortNames <- unique(Tours, `Port Name`)

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
#rm(list = ls(pattern = glob2rx("Tours.*")))
rm(list = ls(pattern = glob2rx("fileName.*")))
rm(tmp.names)

