################################################################################
#
# extract Routes from Tours/Trips
#     
#   a Route  
#     - is independent from the ship
#     - is an ordered list of ports in a region
#     - has a start and an end port
#     - has a fixed number of days
#     - is independent from start and end date
#     - is independent of number of passenegers 
#     - different lists of ports -> different routes
#       simplifying assumption: 
#         Route is defined by: port of start, port of and and number of days
# 
#   The route is extracted from the Tours file     
#     (1) select "Turn days" (attribute `Turn or Call` = T)
#     (2) "tidyfy" data by 
#           - getting tour start and end
#           - extracting the number of days
#           - getting start and end port
#     
#     (3) select distinct records for port.Start, port.End, duration in days
# 
#     
#----------------------------------------------------------------------
#
#     (1) select "Turn days" (attribute `Turn or Call` = T)
#     (2) "tidyfy" data by 
#           - getting tour start and end
#           - extracting the number of days
#           - getting start and end port
Tour.attributes <- c(1,2,3,6,9,11)
Tours.tmp <- Tour.timetable             %>% 
  dplyr::filter(`Turn or Call` == "T" ) %>%
  arrange(Schiff, Datum)                %>%
  # calculate duration of Route and start/end dates/ports
  separate(`Day number`, into=c("dayNr.end.tmp", "dayNr.start.tmp"), sep="/", fill="left", remove=FALSE) %>%
  mutate(duration.Days = lead(dayNr.end.tmp), 
         start.Date    = Datum, 
         end.Date      = lead(Datum),
         start.Port    = `Port Name`,
         end.Port      = lead(`Port Name`),
         start.Name    = name,
         end.Name      = lead(name))                  %>%
  # some entries had wrong `Day number` attributes on Turn-over days (e.g. 0 instead of 14/0). These has been corrected in Tours_corrected file
  # in that case the duration could also be calculated based on Datum: Datum(i+1) - Datum(i) - see below.
  # mutate(duratio.Days = ifelse(is.na(dayNr.end.tmp), lead(Datum)-Datum,dayNr.end.tmp)) %>%
  select(Schiff, start.Port, end.Port, start.Name, end.Name, duration.Days, start.Date, end.Date) %>% 
  # remove all entries where duration.Days is NA -> these are the last available Tours in Dataset without a follow-up Tour
  dplyr::filter(!is.na(duration.Days))


#     (3) select distinct records for port.Start, port.End, duration in days
Routes <- Tours.tmp %>%
  distinct(start.Port, end.Port, start.Name, end.Name, duration.Days) %>%
  mutate(Route.ID = paste("R",rownames(.), sep="-"))                        %>%
  select(Route.ID,duration.Days,start.Name, end.Name, start.Port, end.Port)

  
#     (4) join Tours with Routes by port.Start, port.End, duration.Days
Tours <- Tours.tmp %>%
  left_join(select(Routes, -start.Name, -end.Name), by=c("start.Port", "end.Port", "duration.Days")) %>%
  select(Schiff, start.Date, end.Date, Route.ID, duration.Days, start.Port, end.Port, start.Name, end.Name)

#----------------------------------------------------------------------
# clean up memory
#----------------------------------------------------------------------
rm(Tour.attributes)
rm(Tours.tmp)


  

