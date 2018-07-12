##########################################################################################
#
# Read Data
# (1) read Trips -> readTrips.R
# (2) read Tours -> readTours.R
# (3) read Incidents
#
#!!!!!!!!!!!!!!! to be adapted, at the moment this is a pure brain dump of useful information and todo's 
# addtional information
#
# (2) read Passenger number (Pax) per Trip
#     key of the table is combined of the form
#     <TravelNumberTrip> - <TravelNumberDesc> of mapping trips
# (3) join tables

#--------------------------------
# Crew Zahlen pro Schiff
#
# MS1:  850
# MS2:  850
# MS3: 1040
# MS4: 1040
# MS5: 1040
# MS6: 1040
#
#--------------------------------

#
#
#
##########################################################################################

# (1) read Trips -> readTrips.R
source('readTrips.R')


# (2) read Tours -> readTours.R
source('readTours.R')

##########################################################################################
#
# (3) Read Incidents
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

tmp.ds <- rbind(ds.2012, ds.2013, ds.2014, ds.2015, ds.2016, ds.2017, ds.2018)


# convert ICD10 to code format of icd.data::icd10cm2016
tmp.ds <- tmp.ds %>% 
  mutate(ICD10=str_replace(ICD10, "\\.", ""))         %>% 
  mutate(ICD10.code = factor(str_sub(ICD10, start=1, end=3), levels=levels(icd10cm2016$three_digit))) %>%
# transform "Mein Schiff 1" to "MS1", etc.  
  mutate(Schiff = str_replace(Schiff, "ein Schiff ", "S"))
  
# convert Schiff from Character to Factor 
tmp.ds[[3]] <- as.factor(tmp.ds[[3]])

###tmp.icd10cm2016 <- dplyr::filter(icd10.cm2016, str_length(code)==3)

tmp.ds <- left_join(tmp.ds, dplyr::filter(icd10cm2016[, c("code","three_digit", "major", "sub_chapter", "chapter")], str_length(code)==3), by=c("ICD10.code" = "three_digit"))

# add Harbor and Country from Tours
ds.all <- left_join(tmp.ds,Tours[,c("Datum", "Schiff", "Port","Country", "TripNumber")], by=c("Datum", "Schiff"))


# only for 2014 - 2017 all data is available
ds <- ds.all %>% dplyr::filter(year(Datum) %in% c(2014, 2015, 2016, 2017))

# clean up temporary objects
rm(list = ls(patter=glob2rx("ds.20*")))
rm(tmp.ds)
