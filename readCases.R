##################################################################
#
# prepare case tables for analysis
#
# (1) read ICD10 WHO 2016 codes (German version) from www.dimdi.de (downloaded in data)
# (2) read cases from incidents file
# (3) join icd10.code information
#
#
#
##################################################################

# (1) read ICD10 WHO 2016 codes from www.dimdi.de
source('./readICD10.R')

# (2) read cases from incidents file
tmp.ds <- read_delim("data/ds.csv",";", 
                     escape_double = FALSE, 
                     col_types = cols(Crew  = col_logical(),
                                      Datum = col_date(format = "%Y-%m-%d")), 
                     trim_ws = TRUE)

# (3) join icd10.code information
# convert ICD10 to code format of icd10.codes
tmp.ds <- tmp.ds %>% 
  mutate(Code.ID = str_sub(ICD10, start=1, end=3))     %>%
  # transform "Mein Schiff 1" to "MS1", etc. and convert to Factors 
  mutate(Schiff = as.factor(str_replace(Schiff, "ein Schiff ", "S")))  %>%
  # join icd10.codes
  left_join(icd10.codes, by=c("Code.ID"))

# add Port, Country, TourNr, Route and PaxNr from Tours
ds.all <- tmp.ds %>%
  left_join(Tour.timetable[,c("Datum", "Schiff", "Port Name","countryCode", "long", "lat", "TourNr", "Route", "PaxNr")], by=c("Datum", "Schiff")) %>%
  left_join(select(Ships,"Schiff","CrewNr"),by="Schiff")    %>%
  # calculate total number of persons on board (TotalNr)
  mutate(TotalNr = PaxNr + CrewNr)


# location information is only available for Dates >= min(Tours$Datum) and Dates <= max(Tours$Datum)
ds.loc <- ds.all %>% dplyr::filter(!is.na(long))

# filter for infections
infect.codes <- c(
  "01",   # Kapitel: 01 - Bestimmte infektiöse und parasitäre Krankheiten, Codes A00 - B99
  "10"    # Kapitel: 10 - Krankheiten des Atmungssystems                 , Codes J00 - J99
)

sample.infect.codes <- c(
  "B01", # (Varizellen)
  "B02", # (zoster)
  "A09", # (Gastroenterologie)
  #  "J00", # (Akute Rhinopharyngitis [Erkältungsschnupfen])
  #  "J02", # (Akute Pharyngitis [Halsentzündung])
  #  "J03", # (Akute Tonsillitis [Mandelentzündung])
  "J11", # (Grippe)
  #  "A52", # (Syphilis 1)
  #  "A53", # (Syphilis 2)
  #  "A54", # (Syphilis 3)
  "A16"  # (Tuberkulose)
)

ds.loc.infect <- ds.loc %>% dplyr::filter(Kapitel.ID %in% infect.codes)


# clean up temporary objects
rm(list = ls(patter=glob2rx("ds.20*")))
rm(tmp.ds)
