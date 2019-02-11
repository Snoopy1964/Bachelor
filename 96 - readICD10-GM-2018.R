################################################################
#
# Build DB of ICD10 codes (WHO) 2016 based on
# www.dimdi.de (deutsche Version)
#
# Description of ......
#
# for the analysis the 4 and 5 digit codes are not used, 
# all cases (incidents) will be converted to 3 digit codes only
# This reduces the complexity and the memory consumption
#
################################################################

# Kapitel
icd10.chapters <- read_delim("data/ICD codes dimdi (deutsch)/ICD10-GM-2018/Klassifikationsdateien/icd10gm2018syst_kapitel.txt",
                             ";", 
                             escape_double = FALSE, 
                             col_names = c("Kapitel.ID", "Kapitel.Titel"),
                             col_types = cols(
                               Kapitel.ID = col_character(),
                               Kapitel.Titel = col_character()
                             ),
                             trim_ws = TRUE)

icd10.chapters <- icd10.chapters %>% 
  mutate(Kapitel.ID    = as.factor(Kapitel.ID),
         Kapitel.Titel = as.factor(str_c(Kapitel.ID, Kapitel.Titel, sep = " - ")))

# Gruppen
icd10.groups <- read_delim("data/ICD codes dimdi (deutsch)/ICD10-GM-2018/Klassifikationsdateien/icd10gm2018syst_gruppen.txt",
                            ";", 
                            escape_double = FALSE, 
                            col_names = c("StartCode", "EndCode", "Kapitel.ID", "Gruppen.Titel"),
                            col_types = cols(
                              StartCode = col_character(),
                              EndCode = col_character(),
                             Kapitel.ID = col_character(),
                              Gruppen.Titel = col_character()
                            ),
                            trim_ws = TRUE)

icd10.groups <- icd10.groups %>% 
  mutate(Gruppen.ID    = as.factor(str_c(StartCode,EndCode, sep ="-")), 
         Kapitel.ID    = as.factor(Kapitel.ID),
         Gruppen.Titel = as.factor(str_c(Gruppen.ID, Gruppen.Titel, sep = " - "))) %>%
  select(c(5,3,4,1,2))

# Codes
icd10.codes <- read_delim("data/ICD codes dimdi (deutsch)/ICD10-GM-2018/Klassifikationsdateien/icd10gm2018syst_kodes.txt",
                        ";", 
                        escape_double = FALSE, 
                        col_names = FALSE,
                        col_types = cols(
                          .default = col_character(),
                          X1 = col_integer()
                        ),
                        trim_ws = TRUE)

icd10.codes <- icd10.codes %>%
  dplyr::filter(X1 == 3)  %>%    # don't use 4 and 5 digit codes for the analysis
  select(c(8,5,4,10))
  
names(icd10.codes) <- c("Code.ID", "Group.StartCode", "Kapitel.ID", "Code.Titel")

icd10.codes <- icd10.codes %>%
  # convert to factors
  mutate(Kapitel.ID = as.factor(Kapitel.ID),
         Code.Titel = as.factor(str_c(Code.ID,Code.Titel, sep=" - ")))         %>%      
  # Join with groups to identify Gruppen.ID and Gruppen.Titel
  left_join(icd10.groups[, c("StartCode","Gruppen.ID", "Gruppen.Titel")], 
            by = c("Group.StartCode" = "StartCode")) %>%      
  # remove unneccesary columns and re-order columns
  select(c(1,5,3,4,6))                               %>% 
  # Join with Titels of chapters
  left_join(icd10.chapters, by = "Kapitel.ID")
  



