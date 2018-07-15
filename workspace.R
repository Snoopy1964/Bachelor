# adjust codes of missing sub-codes in icd10cm2016 data base -> compare tibble missing.codes
# A099  -> A09
# I1090 -> I10 
# K0888 -> K08
# T14*  -> T14


# tmp.ds <- rbind(ds.2012, ds.2013, ds.2014, ds.2015, ds.2016, ds.2017, ds.2018)
# # convert Schiff from Character to Factor
# tmp.ds[[3]] <- as.factor(tmp.ds[[3]])
# 
# # convert ICD10 to code format of icd.data::icd10cm2016
# tmp.ds <- tmp.ds %>% mutate(ICD10=str_replace(ICD10, "\\.", ""))
# 
# ###tmp.ds <- left_join(tmp.ds, dplyr::filter(icd10.cm2016, str_length(code)==3), by=c("code" = "three_digit"))
# tmp.ds <- left_join(tmp.ds, icd10cm2016[,c("code","three_digit", "major", "sub_chapter", "chapter")], by=c("ICD10" = "code"))
# 
tmp.ds <- ds


ds.na.code    <- dplyr::filter(tmp.ds, is.na(code) )
missing.codes <- count(ds.na.code, ICD10)
ggplot(dplyr::filter(missing.codes, n >= 1000)) + geom_point(mapping=aes(x=ICD10, y=n))

###### joining the data right!

ICD10.codes3 <- as.tibble(dplyr::filter(icd10cm2016[, c("code", "major", "sub_chapter", "chapter")], str_length(code)==3))

View(left_join(tmp.ds, ICD10.codes3, by=c("ICD10.code" = "code")))

ds.tmp <- left_join(tmp.ds, dplyr::filter(icd10cm2016[, c("code", "major", "sub_chapter", "chapter")], str_length(code)==3), by=c("ICD10.code" = "code"))

ds.na.code    <- dplyr::filter(ds.tmp, is.na(major) )
missing.codes <- count(ds.na.code, ICD10)
ggplot(dplyr::filter(missing.codes, n >= 10)) + geom_point(mapping=aes(x=ICD10, y=n))


#############################################################################
# get (long,lat) for PortNames
#----------------------------------------------------------------------------

ports.tmp <- as_tibble(GNsearch(q=PortNames.tmp[[1]], maxRows=1)[,c(-1,-11)])
for(i in 2:length(PortNames)){
# for(i in 2:10){
  tmp <- GNsearch(q=PortNames[[i]], maxRows=1)
  if( ncol(tmp) == 16) {
    print(sprintf("row: %i, ncol: %i, %s", i, ncol(tmp), PortNames[[i]]))
    ports.tmp <- rbind(ports.tmp, tmp[,c(-1,-11)])
  } else if(ncol(tmp) == 15) {
    print(sprintf("row: %i, ncol: %i, %s", i, ncol(tmp), PortNames[[i]]))
    ports.tmp <- rbind(ports.tmp, tmp[,c(-1)])
  } else if(ncol(tmp) == 14) {
    print(sprintf("row: %i, ncol: %i, %s", i, ncol(tmp), PortNames[[i]]))
    ports.tmp <- rbind(ports.tmp, tmp)
  } else {
    print(sprintf("row: %i, ncol: %i, %s", i, ncol(tmp), PortNames[[i]]))
  }
}
                         
#----------------------------------------------------------------------------
# end of get (long,lat) for PortNames
#############################################################################



# new stuff will be added here:-)

