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
tmp.ds <- read_delim("data/ds.csv",";", 
                      escape_double = FALSE, 
                      col_types = cols(Crew = col_logical(),
                                       Datum = col_date(format = "%Y-%m-%d")), trim_ws = TRUE)

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
ds.all <- left_join(tmp.ds,Tours[,c("Datum", "Schiff", "Port Name","countryCode", "long", "lat")], by=c("Datum", "Schiff"))


# location information is only available for Dates >= min(Tours$Datum) and Dates <= max(Tours$Datum)
ds.loc <- ds.all %>% dplyr::filter(!is.na(long))

# filter for infections
infect.codes <- c(
  "B01", # (Varizellen)
  "B02", # (zoster)
  "A09", # (Gastroenterologie)
  "J00", # (Streptokokken)
  "J02", # (Streptokokken)
  "J03", # (Streptokokken)
  "J11", # (Grippe)
  "A52", # (Syphilis 1)
  "A53", # (Syphilis 2)
  "A54", # (Syphilis 3)
  "A16"  # (Tuberkulose)
)

 ds.loc.infect <- ds.loc %>% dplyr::filter(ICD10.code %in% infect.codes)


# clean up temporary objects
rm(list = ls(patter=glob2rx("ds.20*")))
rm(tmp.ds)
