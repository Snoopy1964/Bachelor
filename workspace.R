# adjust codes of missing sub-codes in icd10cm2016 data base -> compare tibble missing.codes
# A099  -> A09
# I1090 -> I10 
# K0888 -> K08
# T14*  -> T14


tmp.ds <- rbind(ds.2012, ds.2013, ds.2014, ds.2015, ds.2016, ds.2017, ds.2018)
# convert Schiff from Character to Factor
tmp.ds[[3]] <- as.factor(tmp.ds[[3]])

# convert ICD10 to code format of icd.data::icd10cm2016
tmp.ds <- tmp.ds %>% mutate(ICD10=str_replace(ICD10, "\\.", ""))

###tmp.ds <- left_join(tmp.ds, dplyr::filter(icd10.cm2016, str_length(code)==3), by=c("code" = "three_digit"))
tmp.ds <- left_join(tmp.ds, icd10cm2016[,c("code","three_digit", "major", "sub_chapter", "chapter")], by=c("ICD10" = "code"))




ds.na.code    <- dplyr::filter(tmp.ds, is.na(three_digit) )
missing.codes <- count(ds.na.code, ICD10)
ggplot(dplyr::filter(missing.codes, n >= 1000)) + geom_point(mapping=aes(x=ICD10, y=n))



###### joining the data right!

ICD10.codes3 <- as.tibble(dplyr::filter(icd10cm2016[, c("code", "major", "sub_chapter", "chapter")], str_length(code)==3))

View(left_join(tmp.ds, ICD10.codes3, by=c("ICD10.code" = "code")))

ds.tmp <- left_join(tmp.ds, dplyr::filter(icd10cm2016[, c("code", "major", "sub_chapter", "chapter")], str_length(code)==3), by=c("ICD10.code" = "code"))

ds.na.code    <- dplyr::filter(ds.tmp, is.na(major) )
missing.codes <- count(ds.na.code, ICD10)
ggplot(dplyr::filter(missing.codes, n >= 10)) + geom_point(mapping=aes(x=ICD10, y=n))

# do something new



######################################################################
# de-duplicate trips and write back as csv
#----------------------------------------------------------------------
#     mapping contains
#     - TravelNumberTrip 
#       Every trip is splitted at the end of the month and needs to be combined
#     - TravelNumberDesc
#       typically in the form <harbor-start>/<harbor-end> of trip, but
#       sometimes it's just a description like "Karibik trifft Mittelmeer II" 
#       or "Kurzreise Lissabon & Ibizza"
#     - DateStart
#     - DateEnd
#----------------------------------------------------------------------
fileName.Mapping      <- "data/Passagierzahlen/Mapping Cruise Start End.xlsx"
fileName.deDuplecated <- "data/mapping_cruises_start_end.csv"
#----------------------------------------------------------------------

Trip.tmp <- read_excel(fileName.Mapping,
                       range = "A1:D940", 
                       col_types = c("text", "text", "date", "date"))

Trip.old <- Trip.tmp[[1,1]]
for (i in 2:length(Trip.tmp$TravelNumberTrip)) {
  Trip.new <- Trip.tmp[[i,1]]
  if(Trip.old == Trip.new){
    # print(sprintf("i: %3i, old: %s, new: %s",i, Trip.old, Trip.new))
    # print(Trip.tmp[i-1,])
    # print(Trip.tmp[i,])
    Trip.tmp[[i  ,3]] <- Trip.tmp[[i-1,3]]   # set start date from former row
    Trip.tmp[[i-1,4]] <- Trip.tmp[[i  ,4]]   # set former end date to current row
    # print("#----------------")
    # print(Trip.tmp[i-1,])
    # print(Trip.tmp[i,])
  }
  Trip.old <- Trip.new
}
Trip.Mapping <- unique(Trip.tmp)
# convert Datum from Type Datetime to Date (readxl is always converting to Datetime)
Trip.Mapping[[3]] <- as.Date(Trip.Mapping[[3]])
Trip.Mapping[[4]] <- as.Date(Trip.Mapping[[4]])


write_excel_csv(Trip.Mapping, fileName.deDuplecated)


# clean up memory
rm(list = ls(pattern = glob2rx("*.tmp")))
rm("Trip.old", "Trip.new", "i")

#----------------------------------------------------------------------
# end of de-duplicate trips and write back as csv
#############################################################################
