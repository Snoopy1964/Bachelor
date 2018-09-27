ggplot(ds, mapping=aes(x=chapter)) + geom_bar() + coord_flip()
ggplot(ds, mapping=aes(x=chapter)) + geom_bar() + facet_wrap(~ Schiff)+ coord_flip()
ggplot(ds.loc.infect, mapping=aes(x=chapter)) + geom_bar() + facet_wrap(~ year(Datum))+ coord_flip()

ggplot(dplyr::filter(ds.loc.infect, chapter == "Certain infectious and parasitic diseases" & count(ICD10.code)$n >= 10)) +
  geom_bar(mapping=aes(x=ICD10.code, fill=Schiff), position="dodge")
#  facet_wrap(~ year(Datum)) +
# coord_flip()

  
#
# plot counts vs. Code (n>= 1000)
#
# ggds <- ds %>% group_by(ICD10.code) %>% mutate(group_num = n()) %>% dplyr::filter(group_num >= 100, chapter =="Certain infectious and parasitic diseases" ) %>% ggplot()
ggds <- ds.loc.infect %>% group_by(ICD10.code) %>% ggplot()
ggds + geom_bar(mapping=aes(x=major)) + coord_flip()
  


# plot infections by Ship
ggplot(ds.loc.infect, mapping=aes(x=chapter)) + geom_bar() + facet_wrap(~ Schiff) + coord_flip()   
ggplot(ds.loc.infect, mapping=aes(x=ICD10.code)) + 
  geom_bar(mapping=aes(fill=chapter)) + 
  facet_wrap(~ Schiff) + 
  coord_flip() + 
  theme(legend.position = "top")

# plot long