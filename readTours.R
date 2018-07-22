##########################################################################################
#
# Read Tours (combined file from combineTours.R) -> this file contains the raw data for 
# Port Names, which are very messy. The Names need to be manually corrected (at the moment)
# to be used with geoname::GNSearch().
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Please use Tours_corrected.csv only
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
##########################################################################################

#
#----------------------------------------------------------------------
fileName <- "data/Tours_corrected.csv"
#----------------------------------------------------------------------
Tours.tmp <- read_delim(fileName, ";",
                    escape_double = FALSE, 
                    col_types = cols(Datum  = col_date(format = "%d.%m.%Y"),
                                     Schiff = col_factor(levels = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6"))),
                    trim_ws = TRUE)

# Read Ports (extracted from Tours_corrected.csv)
Ports <- read_delim("data/Ports.csv", 
                    ";",
                    escape_double = FALSE, 
                    col_types = cols(lat = col_double(),
                                     lng = col_double()), trim_ws = TRUE)


Tours <- Tours.tmp                          %>% 
  # remove rows with NA entry in "Day number"
  dplyr::filter(!is.na(`Day number`))       %>% 
  # duplicate "Port Name" as "LocDesc" (Location Description) 
  mutate(LocDesc = `Port Name`)             %>%
  # replace "Drydock", "at Sea", "Shipyard", and "Test, 5 days" with previous "Port Name" (replace to NA, and then use fill())
  mutate(`Port Name` = ifelse(`Port Name` %in% c("Drydock", 
                                                 "At Sea", 
                                                 "Shipyard", 
                                                 "Test, 5 days"), NA, 
                              `Port Name`)) %>%
  fill(`Port Name`)                         %>%
  # set "Dock or Anchor" to "S" (Shipyard), if value is NA
  mutate(`Dock or Anchor` = ifelse(is.na(`Dock or Anchor`), "S", `Dock or Anchor`)) %>% 
  # join with Ports to get (long, lat) and countryCodes
  left_join(select(Ports,`Port Name`,long=lng,lat,name,countryCode), by="Port Name") %>%
  # order by "Datum" (original name "CALL DATE") and switch columns "Schiff" and "Datum" because of primary key condition
  select(Datum,Schiff,`Port Name`, long,lat,countryCode, name, LocDesc, `Day number`,`Dock or Anchor`, `Turn or Call`)


#----------------------------------------------------------------------
# clean up memory
#----------------------------------------------------------------------
rm("fileName")
rm(Tours.tmp)


