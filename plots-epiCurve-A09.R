#----------------------------------------------------------------------
#
#
#
#
#
#----------------------------------------------------------------------
# set some defaults
#----------------------------------------------------------------------
theme_update(plot.title = element_text(hjust = 0.5))
theme_update(plot.subtitle = element_text(hjust = 0.5))

#---------------------------------------------------
# A09 - aggregiert über Schiffe und Regionen
#---------------------------------------------------
# Summe aller cases in einer Woche inkl. relativen Häufigkeiten
ds.A09.tmp <- ts                                    %>%
  dplyr::filter(Code.ID == "A09" | is.na(Code.ID))  %>%
  group_by(Week)                                    %>%
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

# Plot absulte Anzahl Cases
gg +
  geom_step(aes(y=Nr.Cases), stat = "identity", alpha = 1/2) +
  geom_smooth(aes(x=Week, y=Nr.Cases), span = 0.3) +
  geom_smooth(aes(x=Week, y=Nr.Cases.Pax), color="red", span = 0.3) +
  geom_smooth(aes(x=Week, y=Nr.Cases.Crew), color="green", span = 0.3) +
  ggtitle("Anzahl Fälle Gastroenteritis (A09) gesamt" ) +
  xlab("Woche ab 1.1.2015 bis 31.12.2017") +
  ylab("Anzahl Fälle A09 pro Woche")
# geom_smooth(aes(x=Week, y=Nr.Cases.Crew), span = 0.3, color = "red") +
# geom_smooth(aes(x=Week, y=Nr.Cases.Pax), span = 0.3, color = "green") #+

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.absolut.all.all.png", 
       width = 297, height =210, units = "mm" )

# Plotte relative Häufigkeiten/Woche Personen/Crew/Pax
gg +
  geom_step(aes(y=relFreq.Pers), stat = "identity", alpha = 1/2) +
  geom_smooth(aes(x=Week, y=relFreq.Pers), span = 0.3) +
  geom_smooth(aes(x=Week, y=relFreq.Pax), span = 0.3, color="red") +
  geom_smooth(aes(x=Week, y=relFreq.Crew), span = 0.3, color="green") +
  ggtitle("relative Häufigkeit Gastroenteritis (A09) gesamt", subtitle = "normiert auf Anzahl Personentage/Anzahl Tage" ) +
  xlab("Wochen im Analysezeitraum 1.1.2015 bis 31.12.2017") +
  ylab("relative Häufigkeit A09 pro Woche")
# geom_smooth(aes(x=Week, y=Nr.Cases.Crew), span = 0.3, color = "red") +
# geom_smooth(aes(x=Week, y=Nr.Cases.Pax), span = 0.3, color = "green") #+

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.relativ.all.all.png", 
       width = 297, height =210, units = "mm" )

#----------------------------------------------------------------------
# Summe aller cases in einer Woche pro Schiff, aggregiert über Regionen
#----------------------------------------------------------------------
ds.A09.tmp <- ts                                    %>%
  dplyr::filter(Code.ID == "A09")  %>%
  group_by(Week, Schiff)                            %>%
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
  # geom_step(aes(y=Nr.Cases), alpha = 1/2) +
  geom_bar(aes(y=Nr.Cases), stat = "identity", alpha = 3/4) +
  geom_smooth(aes(x=Week, y=Nr.Cases), span = 6/12, fullrange = FALSE) +
  facet_wrap( ~ Schiff, scales = NULL ) +
  ggtitle("Anzahl Fälle Gastroenteritis (A09) pro Schiff" ) +
  xlab("Woche ab 1.1.2015 bis 31.12.2017") +
  ylab("Anzahl Fälle A09 pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.absolut.schiff.all.png", 
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
  dplyr::filter(Code.ID == "A09" | is.na(Code.ID))  %>%
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

# Auffälligkeit in Mittelamrika & Karibik 
ds.A09.tmp <- ts                                    %>%
  dplyr::filter(
    Code.ID == "A09" & 
      Region == "Mittelamerika & Karibik" &
      Week > 25 & Week < 75)                        %>%
  group_by(Week,  Schiff)                           %>%
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

# gg <- ggplot(ds.A09.tmp %>% dplyr::filter(Week > 25 & Week < 75 & Schiff == "MS3"), aes(x=Week))


ds.tmp <- ds.A09.tmp %>% select(Week, Nr.Cases.Crew, Nr.Cases.Pax) %>% melt(id.vars="Week")
ggplot(ds.tmp %>% dplyr::filter(Week > 25 & Week < 75), aes(x=Week)) + 
  geom_bar(aes(y=value, fill=variable), stat = "identity", position = "stack") +
  geom_smooth(data=ds.A09.tmp, aes(x=Week, y=Nr.Cases), span = 6/12, fullrange = FALSE) +
  # ylim(-5,25) + 
  ggtitle("Anzahl Fälle Gastroenteritis (A09), MS3, Mittelamerika & Karibik" ) +
  xlab("Wochen in Saison Winter 2015/2016") +
  ylab("Anzahl Fälle A09 pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.absolut.mittelamerika.MS3.png", 
       width = 297, height =210, units = "mm" )

# Plotte absolute Anzahl Fälle
ds.tmp <- ds.A09.tmp %>% select(Week, relFreq.Crew, relFreq.Pax) %>% melt(id.vars="Week")
ggplot(ds.A09.tmp %>% dplyr::filter(Week > 25 & Week < 75), aes(x=Week)) +
  geom_bar(
    data = ds.tmp, 
    aes(x=Week, y=value, fill = variable), position = "dodge", stat = "identity", alpha = 1/2) +
  # geom_bar(aes(y=relFreq.Crew, fill = "Crew"), position = "dodge", stat = "identity", alpha = 1/2) +
  # geom_bar(aes(y=relFreq.Pax,  fill = "Pax" ), position = "dodge", stat = "identity", alpha = 1/2) +
  # geom_bar(aes(y=relFreq.Pers), stat = "identity", alpha = 1/4) +
  # geom_line(aes(y=relFreq.Crew, color="Crew"), size=1.1) +
  # geom_line(aes(y=relFreq.Pax, color="Pax"), size = 1.1) +
  # geom_smooth(aes(x=Week, y=Nr.Cases), span = 6/12, fullrange = FALSE) +
  # facet_wrap( ~ Schiff, scales = NULL ) +
  geom_step(aes(y=relFreq.Pers), alpha = 1, size = 1.2) +
  ggtitle("relative Häufigkeit Gastroenteritis (A09), MS3, Mittelamerika & Karibik" ) +
  xlab("Wochen in Saison Winter 2015/2016") +
  ylab("relative Häufigkeit A09 pro Woche")

ggsave("C:\\Users\\user\\OneDrive\\Bachelorarbeit\\Diagramme\\epi_curve.A09.relativ.mittelamerika.MS3.png", 
       width = 297, height =210, units = "mm" )

