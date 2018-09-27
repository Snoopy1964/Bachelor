#
# Plots für Bacherarbeit
#
#


#----------------------------------------------------------------------
# Statistik
#----------------------------------------------------------------------

# zentrale Tabelle Fahrplan der Schiffe im gesamten Zeitraum
cat("\n\nTour.timetable: zentrale Tabelle Fahrplan der Schiffe im gesamten Zeitraum",
      "\n--------------------------------------------------------------------------\n")
print(Tour.timetable.all     %>% 
        group_by(Schiff) %>%
        summarize(min(Datum),max(Datum), NrTours = n())
      )

# zentrale Tabelle Fahrplan der Schiffe im Analyse Zeitraum 2015 - 2017
cat("\n\nTour.timetable: zentrale Tabelle Fahrplan der Schiffe im Analyse Zeitraum 2015 - 2017",
      "\n-------------------------------------------------------------------------------------\n")
print(Tour.timetable                                                %>% 
        dplyr::filter(Datum >= "2015-01-01" & Datum < "2018-01-01") %>% 
        group_by(Schiff)                                            %>% 
        summarize(min(Datum),max(Datum), NrTours = n())
      )
# kombinierte Tabelle Trips aus "Mapping Cruise Start End.xlsx und "Pax pro Reise.xlsx"
cat('\n\nTrips: kombinierte Tabelle Trips aus "Mapping Cruise Start End.xlsx" und "Pax pro Reise.xlsx"',
      '\n---------------------------------------------------------------------------------------------\n')
print(Trips %>% summarize(min(StartDate), max(StartDate), min(EndDate), max(EndDate)))



# #----------------------------------------------------------------------
# # Plots
# #----------------------------------------------------------------------
# 
# 
# # Anzahl der Cases (Fälle) gruppiert nach Kapitel
# cases.chapter <- ds.loc %>% group_by(chapter) %>% summarize(case = n()) %>% arrange(desc(case))
# ggcc <- ggplot(cases.chapter, mapping = aes(x=chapter, y=case)) 
# ggcc + geom_point()
# 
# 
# # Anzahl der Cases (Fälle) gruppiert nach ICD10 code
# cases.ICD10.code <- ds.loc %>% group_by(ICD10.code,major) %>% summarize(case = n()) %>% arrange(desc(case))
# 



