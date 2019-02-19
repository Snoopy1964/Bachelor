#----------------------------------------------------------------------
# Infektionskrankheiten
# - A16: Tuberkulose
# - B01: Varizellen (Windpocken)
# - B02: Herpes Zoster (Gürtelrose)
# - J11: Grippe (Viren nicht nachgewiesen)
# 
# Aufgrund zu geringer Statistik macht hier keine Aufsplittung in Regionen Sinn
# - Correlationsanalyse B01, B02
#   Ansteckung Crew von Windpocken Kinder, bzw. Gürtelrose ältere Menschen
# - Vergleich Grippe mit offiziellen Grippewellen
#
# ToDos:
# - Plot "Anzahl Fälle ... gesamt"
#   - Unterscheidung Passagiere und Crew
#   - y-Achse: "ganze Zahlen"
#
#
#----------------------------------------------------------------------
# set some defaults
#----------------------------------------------------------------------
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))

#======================================================================
# Infektionskrankheiten - Übersicht
#----------------------------------------------------------------------
# aggregiert über Schiffe und Regionen
#----------------------------------------------------------------------
# Summe aller cases in einer Woche 
# Relative Häufigkeiten machen hier keinen Sinn, da zu wenig Statistik

ds.other.tmp <- ts                                    %>%
  dplyr::filter(Code.ID %in% c("A16", "B01", "J11", "B02"))  %>%
  group_by(Month, Code.ID)                                    %>%
  summarize(
    Nr.Cases.Crew = sum(Crew), 
    Nr.Cases.Pax  = sum(Pax), 
    Nr.Cases      = Nr.Cases.Crew+Nr.Cases.Pax,
    Nr.Days       = nrDays(Day),
    Pd.Crew       = sum(CrewNr),
    Pd.Pax        = sum(PaxNr),
    relFreq       = Nr.Cases*Nr.Days/(Pd.Crew + Pd.Pax)
  )                 



# Plot absulte Anzahl Cases
ds.tmp <- ds.other.tmp %>% select(Month, Code.ID, Nr.Cases.Crew, Nr.Cases.Pax) %>% melt(id.vars = c("Month", "Code.ID"))
ggplot(ds.other.tmp, aes(x=Month)) +
  facet_wrap(~ Code.ID) +
  geom_bar(
    data = ds.tmp,
    aes(
      x = Month,
      y = value, 
      fill = variable), 
    stat = "identity", 
    alpha = 3/4, 
    position = "stack") +
  ggtitle("Anzahl Fälle A16, B01, B02 und J11 pro Woche 2015-2017" ) +
  xlab("Woche ab 1.1.2015 bis 31.12.2017") +
  ylab("Anzahl Fälle pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.other.absolut.all.all.png", 
       width = 297, height =210, units = "mm" )


#======================================================================
# Correlationsanalyse B01 <-> B02, Crew <-> Pax
#----------------------------------------------------------------------
# Summe aller cases B01/B02 in einer Woche pro Schiff, aggregiert über Regionen
#----------------------------------------------------------------------
ds.B0x.tmp <- ts                                   %>%
  dplyr::filter(Code.ID %in% c("B01", "B02"))      %>%
  select(-`Port Name`, -lng, -lat)                 %>%
  group_by(Week, Schiff, Code.ID)                  %>%
  summarize(
    Nr.Cases.Crew = sum(Crew), 
    Nr.Cases.Pax  = sum(Pax), 
    Nr.Cases      = Nr.Cases.Crew+Nr.Cases.Pax,
    Nr.Days       = nrDays(Day),
    Pd.Pax        = sum(PaxNr),
    Pd.Crew       = sum(CrewNr),
    Pd.Pers       = Pd.Pax+Pd.Crew,
    relFreq.Pax   = Nr.Cases.Pax/(Pd.Pax/Nr.Days),
    relFreq.Crew  = Nr.Cases.Crew/(Pd.Crew/Nr.Days),
    relFreq.Pers  = Nr.Cases/(Pd.Pers/Nr.Days)
  )                 

gg <- ggplot(ds.B0x.tmp, aes(x=Week))

# Plotte absolute Anzahl Fälle
gg +
  # geom_step(aes(y=Nr.Cases), alpha = 1/2) +
  facet_grid( Schiff ~ Code.ID, scales = NULL ) +
  geom_bar(aes(y=Nr.Cases), stat = "identity", alpha = 3/4) +
  # geom_smooth(aes(x=Week, y=Nr.Cases), span = 6/12, fullrange = FALSE) +
  ggtitle("Anzahl Fälle Gastroenteritis (A09) pro Schiff" ) +
  xlab("Woche ab 1.1.2015 bis 31.12.2017") +
  ylab("Anzahl Fälle A09 pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.B0x.absolut.schiff.all.png", 
       width = 297, height =210, units = "mm" )

# Plotte relative Häufigkeiten/Woche Personen/Crew/Pax
gg +
  facet_wrap( ~ Schiff, scales = "free_x" ) +
  # geom_step(aes(y=relFreq.Pers), stat = "identity", alpha = 1/2) +
  geom_bar(aes(y=relFreq.Pers), stat = "identity", alpha = 3/4) +
  geom_smooth(aes(x=Week, y=relFreq.Pers), span = 0.3) +
  # geom_smooth(aes(x=Week, y=relFreq.Pax), span = 0.3, color="red") +
  # geom_smooth(aes(x=Week, y=relFreq.Crew), span = 0.3, color="green") +
  ggtitle("relative Häufigkeit Gastroenteritis (A09) pro Schiff", subtitle = "normiert auf Anzahl Personentage/Anzahl Tage" ) +
  xlab("Wochen im Analysezeitraum 1.1.2015 bis 31.12.2017") +
  ylab("relative Häufigkeit A09 pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.relativ.schiff.all.png", 
       width = 297, height =210, units = "mm" )


#----------------------------------------------------------------------
# Summe aller cases in einer Woche pro Region, aggregiert über Schiffe
#----------------------------------------------------------------------
ds.A09.tmp <- ts                                    %>%
  dplyr::filter(Code.ID == "A09" )  %>%
  group_by(Week, Region)                            %>%
  summarize(
    Nr.Cases.Crew = sum(Crew), 
    Nr.Cases.Pax  = sum(Pax), 
    Nr.Cases      = Nr.Cases.Crew+Nr.Cases.Pax,
    Nr.Days       = nrDays(Day),
    Pd.Pax        = sum(PaxNr),
    Pd.Crew       = sum(CrewNr),
    Pd.Pers       = Pd.Pax+Pd.Crew,
    relFreq.Pax   = Nr.Cases.Pax/(Pd.Pax/Nr.Days),
    relFreq.Crew  = Nr.Cases.Crew/(Pd.Crew/Nr.Days),
    relFreq.Pers  = Nr.Cases/(Pd.Pers/Nr.Days)
  )                 

gg <- ggplot(ds.A09.tmp, aes(x=Week))

# Plotte absolute Anzahl Fälle
gg +
  geom_bar(aes(y=Nr.Cases), stat="identity", alpha = 3/4) +
  # geom_step(aes(y=Nr.Cases), alpha = 3/4) +
  # geom_smooth(aes(x=Week, y=Nr.Cases), span = 6/12, fullrange = FALSE) +
  facet_wrap( ~ Region, scales = NULL ) +
  ggtitle("Anzahl Fälle Gastroenteritis (A09) pro Region" ) +
  xlab("Woche ab 1.1.2015 bis 31.12.2017") +
  ylab("Anzahl Fälle A09 pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.absolut.region.all.png", 
       width = 297, height =210, units = "mm" )

# Plotte relative Häufigkeiten/Woche Personen
gg +
  facet_wrap( ~ Region, scales = NULL ) +
  geom_bar(aes(y=relFreq.Pers), stat = "identity", alpha = 3/4) +
  # geom_smooth(aes(x=Week, y=relFreq.Pers), span = 0.3) +
  # geom_smooth(aes(x=Week, y=relFreq.Pax), span = 0.3, color="red") +
  # geom_smooth(aes(x=Week, y=relFreq.Crew), span = 0.3, color="green") +
  ggtitle("relative Häufigkeit Gastroenteritis (A09) pro Region", subtitle = "normiert auf Summe Personentage/Anzahl Tage" ) +
  xlab("Wochen im Analysezeitraum 1.1.2015 bis 31.12.2017") +
  ylab("relative Häufigkeit A09 pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.relativ.region.all.png", 
       width = 297, height =210, units = "mm" )

#--------------------------------------------------------------------
# Korrelation B01 <-> B02 ?
#--------------------------------------------------------------------

ds.tmp <- cases.infect.codes %>%
  dplyr::filter(Code.ID == "B01" | Code.ID == "B02") %>%
  

