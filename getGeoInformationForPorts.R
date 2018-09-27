#####################################################################################
#
# get geo information for the "Port Names" in the Tour files
#   this script has to be executed only, if Tours_corrected.csv will be touched
#
# - load Tours from Tours_corrected.csv
#   the Tours_corrected.csv is higly manual corrected. The original Tours Excel data
#   is concerning the Port Names a hell, since different people seems to had 
#   different ideas, how the Port Names should look like. Especially the Countries are 
#   "somewhat strange". In addition there are doubled entries just by adding 
#   "line breaks" like "\r\n\r".
#   All these facts result in empty responses of the geonames service api
#   (http://api.geonames.org/searchJSON?q="xxxxx")
#
# - extract geo information with geonames::GNSearch
#
# - write to csv file Ports.csv
#
# !!!!!!!!!!!!! be patient, geo coding takes time !!!!!!!!!!!!!!!!!!!
#
#####################################################################################
#
# - load Tours from Tours_corrected.csv
#----------------------------------------------------------------------
fileName <- "data/Tours_corrected.csv"
#----------------------------------------------------------------------
Tours <- read_delim(fileName, ";",
                    escape_double = FALSE, 
                    col_types = cols(Datum  = col_date(format = "%d.%m.%Y"),
                                     Schiff = col_factor(levels = c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6"))),
                    trim_ws = TRUE)


# extract Port Names
PortNames <- unique(Tours[["Port Name"]])
# Remove                 NA, At Sea, Dry Dock, Wetdock, Shipyard, "Test, 5 days"
PortNames <- PortNames[c(-1,     -3,       -4,    -109,     -117,         -143)]

# - extract geo information with geonames::GNSearch
ports.tmp <- as_tibble(GNsearch(q=PortNames[[1]], maxRows=1)[,c(-1,-11)])
for(i in 2:length(PortNames)){
  # for(i in 2:10){
  tmp <- GNsearch(q=PortNames[[i]], maxRows=1)
  if( ncol(tmp) == 16) {
    ports.tmp <- rbind(ports.tmp, tmp[,c(-1,-11)])
  } else if(ncol(tmp) == 15) {
    ports.tmp <- rbind(ports.tmp, tmp[,c(-1)])
  } else if(ncol(tmp) == 14) {
    ports.tmp <- rbind(ports.tmp, tmp)
  } else {
    print(sprintf("row: %i, ncol: %i, %s", i, ncol(tmp), PortNames[[i]]))
  }
  print(sprintf("row: %i, ncol: %i, %s", i, ncol(tmp), PortNames[[i]]))
}
Ports <- as_tibble(cbind(`Port Name` = PortNames, ports.tmp))[,c(1,2,14,9,8,11,3:7,10,12:13,15)]

# - write to csv file Ports.csv
write.csv2(Ports, "./data/Ports.csv", row.names=FALSE, fileEncoding = "UTF-8")

# clean up memory
rm(fileName, Tours, PortNames, ports.tmp, Ports, i)

